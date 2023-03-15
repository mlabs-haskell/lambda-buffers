{-# LANGUAGE OverloadedLabels #-}

module LambdaBuffers.Compiler.TypeClassCheck.Utils (
  -- exports for Validate & Tests
  type Instance,
  TypeClassError (..),
  BasicConditionViolation (..),
  lookupOr,
  checkInstance,
  -- for the superclass cycle check
  mkClassInfos,
  toClassMap,
  -- the point of this module
  ModuleBuilder (..),
  mkBuilders,
  moduleNameToMiniLogFilepath,
) where

import Control.Lens ((^.), (^?))
import Control.Lens.Combinators (Ixed (ix))
import Control.Monad.Except (MonadError (throwError))
import Control.Monad.State (foldM)
import Data.List (foldl')
import GHC.Generics (Generic)

import Data.Map qualified as M
import Data.Set qualified as S
import Data.Text qualified as T

import LambdaBuffers.Compiler.TypeClassCheck.Compat (
  defToExp,
  modulename,
  tyToPat,
 )
import LambdaBuffers.Compiler.TypeClassCheck.Rules (
  Class (Class),
  Constraint (C),
  FQClassName (FQClassName),
  Rule ((:<=)),
 )

import Data.Text (Text)
import LambdaBuffers.Compiler.ProtoCompat.Types qualified as P (
  ClassDef,
  CompilerInput (CompilerInput),
  Constraint (Constraint),
  InstanceClause (InstanceClause),
  Module,
  ModuleName,
  SourceInfo,
  TyClassRef (ForeignCI, LocalCI),
 )
import LambdaBuffers.Compiler.TypeClassCheck.Pat (Exp, Literal (ModuleName), Pat (AppP, ConsP, DecP, LabelP, LitP, NilP, ProdP, RecP, RefP, SumP, VarP))

import Data.Char (toLower)
import Data.Foldable (Foldable (toList))
import LambdaBuffers.Compiler.ProtoCompat.InfoLess qualified as P
import LambdaBuffers.Compiler.ProtoCompat.Types qualified as PC
import LambdaBuffers.Compiler.ProtoCompat.Utils qualified as PC
import LambdaBuffers.Compiler.TypeClassCheck.Pretty (pointies, (<///>))
import LambdaBuffers.Compiler.TypeClassCheck.Solve (Overlap (Overlap))
import Prettyprinter (
  Pretty (pretty),
  hcat,
  indent,
  line,
  nest,
  prettyList,
  punctuate,
  vcat,
  (<+>),
 )

-- *Here* it's useful to distinguish them
type Instance = Rule Pat

{- Some of these are perfunctory & used to keep functions total.
-}
data TypeClassError
  = UnknownClass FQClassName P.SourceInfo
  | UnknownModule P.ModuleName
  | MissingModuleInstances P.ModuleName
  | MissingModuleScope P.ModuleName
  | ClassNotFoundInModule Text [Text]
  | LocalTyRefNotFound T.Text (P.InfoLess P.ModuleName)
  | SuperclassCycleDetected [[FQClassName]]
  | FailedToSolveConstraints (P.InfoLess P.ModuleName) [Constraint Exp] Instance
  | MalformedTyDef (P.InfoLess P.ModuleName) Exp
  | BadInstance BasicConditionViolation
  deriving stock (Show, Eq, Generic)

instance Pretty TypeClassError where
  pretty = \case
    UnknownClass cref si ->
      "Error at" <+> pretty si <+> nest 2 ("Unknown class: " <> pretty cref)
    UnknownModule mn ->
      "INTERNAL ERROR: Unknown Module" <+> pretty mn
    MissingModuleInstances mn ->
      "INTERNAL ERROR: Missing instance data for module" <+> pretty mn
    MissingModuleScope mn ->
      "INTERNAL ERROR: Could not determine TypeClass scope for module" <+> pretty mn
    ClassNotFoundInModule cn mn ->
      "Error: Expected to find class"
        <+> pretty cn
        <+> "in module"
        <+> pretty mn
        <+> "but it isn't there!"
    LocalTyRefNotFound txt mn ->
      "Error: Expected to find a type definition for a type named"
        <+> pretty txt
        <+> "in module"
        <+> P.withInfoLess mn pretty
        <+> "but it isn't there!"
    SuperclassCycleDetected crs ->
      "Error: Superclass cycles detected in compiler input:"
        <+> nest
          2
          ( vcat
              . map (hcat . punctuate " => " . map pretty)
              $ crs
          )
    FailedToSolveConstraints mn cs i ->
      "Error: Could not derive instance:"
        <+> pointies (pretty i)
          <> line
          <> line
          <> indent 2 "in module"
        <+> P.withInfoLess mn pretty
          <> line
          <> line
          <> indent 2 "because the following constraint(s) were not satisfied:"
          <> line
          <> line
          <> indent 2 (vcat $ map pretty cs)
    MalformedTyDef mn xp ->
      "Error: Encountered malformed type definition:"
        <> line
        <> indent 2 (pretty xp)
        <> line
        <> indent 2 ("in module" <+> P.withInfoLess mn pretty)
    BadInstance bcv -> pretty bcv

data BasicConditionViolation
  = TyConInContext Instance (Constraint Pat)
  | OnlyTyVarsInHead Instance (Constraint Pat)
  | OverlapDetected Overlap
  deriving stock (Show, Eq, Generic)

instance Pretty BasicConditionViolation where
  pretty = \case
    TyConInContext inst cst ->
      "Error: Invalid instance declaration!"
        <///> indent 2 (pretty inst)
        <///> "The instance constraint"
        <///> indent 2 (pretty cst)
        <///> "Contains a type constructor, but may only contain type variables"
    OnlyTyVarsInHead inst cst ->
      "Error: Invalid instance declaration!"
        <///> indent 2 (pretty inst)
        <///> "The instance constraint"
        <///> indent 2 (pretty cst)
        <///> "only contains type variables, but must contain at least one constructor"
    OverlapDetected (Overlap cst rules) ->
      "Error: Overlapping instances detected when trying to solve constraint"
        <+> pretty cst
          <> line
          <> indent 2 (vcat (map pretty rules))

{- NOTE: We need different conditions for MPTCs but these are correct for now

From https://www.microsoft.com/en-us/research/wp-content/uploads/2016/02/jfp06.pdf p.10-11
1. The context C of a class and instance declaration can mention only type
variables, not type constructors, and in each individual class constraint CC
all the type variables are distinct.

2. In an instance declaration instance C => T C t1 . . . tn, at least one of the types ti must not be a type variable

3. The instance declarations must not overlap.
-}

checkInstance :: Rule Pat -> Either TypeClassError ()
checkInstance rule@(C cls hd :<= constraints) =
  case traverse cond1 constraints of
    Left e -> Left e
    Right _ -> case hd of
      VarP _ -> Left . BadInstance $ OnlyTyVarsInHead rule (C cls hd)
      -- NOTE: THIS ASSUMES THAT EVERYTHING HAS BEEN KIND-CHECKED AND
      --       THAT NO TYVARS HAVE KIND (* -> *). If every tyvar
      --       is of kind * and we have no MPTCs then the
      --       only case we have to worry about for condition 2 is
      --       a bare type variable in the head
      _ -> Right ()
  where
    cond1 :: Constraint Pat -> Either TypeClassError ()
    cond1 cst@(C _ p) = case p of
      VarP _ -> Right ()
      _ -> Left . BadInstance $ TyConInContext rule cst

{- This contains:
     - Everything needed to validate instances (in the form needed to do so)
     - Everything needed for codegen (modulo sourceInfo)
-}
data ModuleBuilder = ModuleBuilder
  { mbTyDefs :: M.Map T.Text Exp -- sourceInfo needs to be in here somehow (these are all local refs)
  , mbInstances :: Instances -- instances to be generated
  , mbClasses :: Classes -- classes to be generated
  , mbScope :: Instances -- Instances to use as rules when checking instance clauses in the module
  }
  deriving stock (Show, Eq, Generic)

instance Pretty ModuleBuilder where
  pretty (ModuleBuilder defs insts clss scop) =
    vcat
      [ "Type Defs:" <+> nest 2 (prettyList $ M.elems defs)
      , "Instances:" <+> nest 2 (prettyList $ S.toList insts)
      , "Classes:" <+> nest 2 (prettyList $ S.toList clss)
      , "Scope:" <+> nest 2 (prettyList $ S.toList scop)
      ]

{-
    Classes
    NOTE: We discard the arguments to the class and the arguments to the superclasses
          In the absence of MPTCs this should be an acceptable form of eta reduction
    NOTE: Actually previous note is probably wrong, this only allows for "direct superclasses"
          like (C a, D a) => E a. Or maybe it's not wrong? Without MPTCs, the only thing that
          wouldn't be direct is something like (C a, D a, E f) => G (f a b), but ATM we don't
          support type variables that represent kinds other than Star, so this should be
          good enough *for now*
    NOTE: MUST BE CHECKED FOR SUPERCLASS CYCLES OR WILL LOOP FOREVER
-}

-- could use a tuple but this makes the signatures more readable
data ClassInfo = ClassInfo {ciName :: FQClassName, ciSupers :: [FQClassName]}
  deriving stock (Show, Eq, Ord, Generic)

type Classes = S.Set Class

mkClassInfos :: [P.Module] -> M.Map (P.InfoLess P.ModuleName) [ClassInfo]
mkClassInfos = foldl' (\acc mdl -> M.insert (P.mkInfoLess $ mdl ^. #moduleName) (go mdl) acc) M.empty
  where
    go :: P.Module -> [ClassInfo]
    go m = map (defToClassInfo $ m ^. #moduleName) (M.elems $ m ^. #classDefs)

defToClassInfo :: P.ModuleName -> P.ClassDef -> ClassInfo
defToClassInfo mName cd =
  ClassInfo (FQClassName (cd ^. #className . #name) mn) $
    map (\x -> tyRefToFQClassName mn $ x ^. #classRef) (cd ^. #supers)
  where
    mn = modulename mName

tyRefToFQClassName :: [Text] -> P.TyClassRef -> FQClassName
tyRefToFQClassName mn = \case
  P.LocalCI lr -> FQClassName (lr ^. #className . #name) mn
  P.ForeignCI fr -> FQClassName (fr ^. #className . #name) (modulename $ fr ^. #moduleName)

toClassMap :: [ClassInfo] -> M.Map FQClassName [FQClassName]
toClassMap = foldl' (\acc (ClassInfo nm sups) -> M.insert nm sups acc) M.empty

{- |
This constructs a Map where the keys are fully qualified class names and the
values are values of the Class data type from TypeClass.Rules
-}
buildClasses :: M.Map (P.InfoLess P.ModuleName) [ClassInfo] -> Either TypeClassError (M.Map FQClassName Class)
buildClasses cis = foldM go M.empty (concat $ M.elems cis)
  where
    superclasses = foldl' M.union M.empty $ M.elems (toClassMap <$> cis)

    go :: M.Map FQClassName Class -> ClassInfo -> Either TypeClassError (M.Map FQClassName Class)
    go acc (ClassInfo nm sups) = do
      ss <- traverse resolveSuper sups
      pure $ M.insert nm (Class nm ss) acc

    resolveSuper :: FQClassName -> Either TypeClassError Class
    resolveSuper cr@(FQClassName cn mn) = do
      sups <- lookupOr cr superclasses $ ClassNotFoundInModule cn mn
      Class cr <$> traverse resolveSuper sups

{-
    Instances
-}

type Instances = S.Set Instance

getInstances :: M.Map FQClassName Class -> P.ModuleName -> [P.InstanceClause] -> Either TypeClassError Instances
getInstances ctable mn = foldM go S.empty
  where
    go :: S.Set Instance -> P.InstanceClause -> Either TypeClassError Instances
    go acc (P.InstanceClause (P.Constraint cn h _) csts si') = case ctable ^? ix cref of
      Nothing -> throwError $ UnknownClass cref si'
      Just cls -> do
        let p = tyToPat h
        cs <- traverse goConstraint csts
        let inst = C cls p :<= cs
        checkInstance inst
        pure $ S.insert inst acc
      where
        cref = tyRefToFQClassName (modulename mn) cn

    goConstraint :: P.Constraint -> Either TypeClassError (Constraint Pat)
    goConstraint (P.Constraint cn arg si') = case ctable ^? ix cref of
      Nothing -> throwError $ UnknownClass cref si'
      Just cls -> do
        let p = tyToPat arg
        pure $ C cls p
      where
        cref = tyRefToFQClassName (modulename mn) cn

mkModuleClasses :: P.CompilerInput -> M.Map (P.InfoLess P.ModuleName) [ClassInfo]
mkModuleClasses (P.CompilerInput ms) = mkClassInfos (M.elems ms)

{- |
This constructs the instances defined in each module (NOT the instances in scope in that module)
-}
mkModuleInstances ::
  P.CompilerInput ->
  M.Map FQClassName Class ->
  Either TypeClassError (M.Map (P.InfoLess P.ModuleName) Instances)
mkModuleInstances (P.CompilerInput ms) ctable = foldM go M.empty ms
  where
    go :: M.Map (P.InfoLess P.ModuleName) Instances -> P.Module -> Either TypeClassError (M.Map (P.InfoLess P.ModuleName) Instances)
    go acc modl = case getInstances ctable (modl ^. #moduleName) (modl ^. #instances) of
      Left e -> Left e
      Right is -> Right $ M.insert (P.mkInfoLess $ modl ^. #moduleName) is acc

{- |
This fetches the *Rules* used as the scope for constraint solving
for a given module.
-}
moduleScope ::
  M.Map (P.InfoLess P.ModuleName) P.Module ->
  M.Map (P.InfoLess P.ModuleName) Instances ->
  P.ModuleName ->
  Either TypeClassError (S.Set (Rule Pat))
moduleScope modls is = go
  where
    go :: P.ModuleName -> Either TypeClassError Instances
    go mn = case modls ^? (ix (P.mkInfoLess mn) . #imports) of
      Nothing -> Left $ UnknownModule mn
      Just impts -> mconcat <$> traverse goImport (toList impts)

    -- NOTE: This doesn't do recursive scope fetching anymore.
    --       If a user wants an instance rule in scope, they
    --       have to import the module (or something from it)
    --       (this is how it works in haskell/ps/rust)
    goImport :: P.ModuleName -> Either TypeClassError Instances
    goImport mn = case is ^? ix (P.mkInfoLess mn) of
      Nothing -> Left $ UnknownModule mn
      Just insts -> Right $ contextualize mn insts

-- imported instances might contain improperly localized RefPs in their Pat
-- i.e. they might be a RefP Nil (Name t), which indicates a local reference,
-- but should be a RefP (ModuleName ...) (Name t) in the scope of the
-- module that imports them
contextualize :: P.ModuleName -> Instances -> Instances
contextualize mn = S.map (fmap go)
  where
    go :: Pat -> Pat
    go (RefP NilP t) = RefP (LitP . ModuleName $ modulename mn) t
    go (LabelP p1 p2) = LabelP (go p1) (go p2)
    go (ConsP p1 p2) = ConsP (go p1) (go p2)
    go (RecP fs) = RecP $ go fs
    go (ProdP fs) = ProdP $ go fs
    go (SumP fs) = SumP $ go fs
    go (AppP p1 p2) = AppP (go p1) (go p2)
    go (DecP p1 p2 p3) = DecP (go p1) (go p2) (go p3)
    go other = other

lookupOr :: Ord k => k -> M.Map k v -> e -> Either e v
lookupOr k m e = case M.lookup k m of
  Nothing -> Left e
  Just v -> Right v

mkBuilders :: P.CompilerInput -> Either TypeClassError (M.Map (P.InfoLess P.ModuleName) ModuleBuilder)
mkBuilders ci = do
  classTable <- buildClasses classInfos
  insts <- mkModuleInstances ci classTable
  scope <- traverse (\m -> moduleScope (ci ^. #modules) insts (m ^. #moduleName)) (ci ^. #modules)
  foldM (go classTable insts scope) M.empty (ci ^. #modules)
  where
    classInfos :: M.Map (P.InfoLess P.ModuleName) [ClassInfo]
    classInfos = mkModuleClasses ci

    go ::
      M.Map FQClassName Class ->
      M.Map (P.InfoLess P.ModuleName) Instances ->
      M.Map (P.InfoLess P.ModuleName) Instances ->
      M.Map (P.InfoLess P.ModuleName) ModuleBuilder ->
      P.Module ->
      Either TypeClassError (M.Map (P.InfoLess P.ModuleName) ModuleBuilder)
    go classTable insts scope acc m = do
      mbinsts <- lookupOr (P.mkInfoLess $ m ^. #moduleName) insts $ MissingModuleInstances $ m ^. #moduleName
      mbscope <- lookupOr (P.mkInfoLess $ m ^. #moduleName) scope $ MissingModuleScope $ m ^. #moduleName
      mdule <- lookupOr (P.mkInfoLess $ m ^. #moduleName) (ci ^. #modules) $ UnknownModule $ m ^. #moduleName
      mbclasses <- resolveClasses classTable (m ^. #moduleName) . M.elems $ mdule ^. #classDefs
      let mbtydefs =
            foldl'
              ( \accM t ->
                  M.insert
                    (t ^. #tyName . #name)
                    (defToExp t)
                    accM
              )
              M.empty
              $ mdule ^. #typeDefs
          mb =
            ModuleBuilder
              { mbTyDefs = mbtydefs
              , mbInstances = mbinsts
              , mbClasses = mbclasses
              , mbScope = mbscope
              }
      pure $ M.insert (P.mkInfoLess $ m ^. #moduleName) mb acc

    resolveClasses ::
      M.Map FQClassName Class ->
      P.ModuleName ->
      [P.ClassDef] ->
      Either TypeClassError Classes
    resolveClasses _ _ [] = pure S.empty
    resolveClasses classTable mn (c : cs) = do
      let cname = c ^. #className . #name
          mname = modulename mn
          fqNm = FQClassName cname mname
      cls <-
        lookupOr fqNm classTable $ ClassNotFoundInModule cname mname
      S.insert cls <$> resolveClasses classTable mn cs

moduleNameToMiniLogFilepath :: PC.ModuleName -> FilePath
moduleNameToMiniLogFilepath = fmap ((\c -> if c == '.' then '_' else c) . toLower) . show . PC.prettyModuleName
