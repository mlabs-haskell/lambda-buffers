{-# LANGUAGE OverloadedLabels #-}

module LambdaBuffers.Compiler.TypeClassCheck.Utils (
  lookupOr,
  checkInstance,
  Tagged (..),
  unTag,
  getTag,
  -- for the superclass cycle check
  mkClassInfos,
  toClassMap,
  -- the point of this module
  ModuleBuilder (..),
  mkBuilders,
) where

import Control.Lens ((^.), (^?))
import Control.Lens.Combinators (Ixed (ix))
import Control.Monad.Except (MonadError (throwError))
import Control.Monad.State (foldM)
import Data.List (foldl')
import Data.Map.Internal (traverseWithKey)
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
import LambdaBuffers.Compiler.TypeClassCheck.Pat (Exp, Literal (ModuleName), Pat (AppP, ConsP, DecP, LabelP, LitP, NilP, ProdP, RecP, RefP, SumP, VarP), Tagged (Tag), getTag, unTag)

import Prettyprinter (
  Pretty (pretty),
  nest,
  prettyList,
  vcat,
  (<+>),
 )

import LambdaBuffers.Compiler.TypeClassCheck.Errors (
  BasicConditionViolation (OnlyTyVarsInHead, TyConInContext),
  Instance,
  TypeClassError (
    BadInstance,
    ClassNotFoundInModule,
    MissingModuleInstances,
    MissingModuleScope,
    UnknownClass,
    UnknownModule
  ),
 )

-- *Here* it's useful to distinguish them

{- NOTE: We need different conditions for MPTCs but these are correct for now

From https://www.microsoft.com/en-us/research/wp-content/uploads/2016/02/jfp06.pdf p.10-11
1. The context C of a class and instance declaration can mention only type
variables, not type constructors, and in each individual class constraint CC
all the type variables are distinct.

2. In an instance declaration instance C => T C t1 . . . tn, at least one of the types ti must not be a type variable

3. The instance declarations must not overlap.
-}

checkInstance :: P.SourceInfo -> Rule Pat -> Either TypeClassError ()
checkInstance si rule@(C cls hd :<= constraints) =
  case traverse cond1 constraints of
    Left e -> Left e
    Right _ -> case hd of
      VarP _ -> Left $ BadInstance (OnlyTyVarsInHead rule (C cls hd)) si
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
      _ -> Left $ BadInstance (TyConInContext rule cst) si

{- This contains:
     - Everything needed to validate instances (in the form needed to do so)
     - Everything needed for codegen (modulo sourceInfo)
-}
data ModuleBuilder = ModuleBuilder
  { mbTyDefs :: M.Map T.Text (Tagged Exp) -- sourceInfo needs to be in here somehow (these are all local refs)
  , mbInstances :: S.Set (Tagged Instance) -- instances to be generated
  , mbClasses :: Classes -- classes to be generated
  , mbScope :: S.Set (Tagged Instance) -- Instances to use as rules when checking instance clauses in the module
  }
  deriving stock (Show, Eq, Generic)

instance Pretty ModuleBuilder where
  pretty (ModuleBuilder defs insts clss scop) =
    vcat
      [ "Type Defs:" <+> nest 2 (prettyList $ M.elems defs)
      , "Instances:" <+> nest 2 (prettyList . S.toList $ insts)
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

mkClassInfos :: [P.Module] -> M.Map P.ModuleName [ClassInfo]
mkClassInfos = foldl' (\acc mdl -> M.insert (mdl ^. #moduleName) (go mdl) acc) M.empty
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
buildClasses :: M.Map P.ModuleName [ClassInfo] -> Either TypeClassError (M.Map FQClassName Class)
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

getInstance :: M.Map FQClassName Class -> P.ModuleName -> P.InstanceClause -> Either TypeClassError (Tagged Instance)
getInstance ctable mn (P.InstanceClause cn h csts si') = case ctable ^? ix cref of
  Nothing -> throwError $ UnknownClass cref si'
  Just cls -> do
    let p = tyToPat h

    cs <- traverse goConstraint csts
    let inst = C cls p :<= cs
    checkInstance si' inst
    pure $ Tag si' inst
  where
    cref = tyRefToFQClassName (modulename mn) cn

    goConstraint :: P.Constraint -> Either TypeClassError (Constraint Pat)
    goConstraint (P.Constraint cnx arg six') = case ctable ^? ix cref of
      Nothing -> throwError $ UnknownClass crefx six'
      Just cls -> do
        let p = tyToPat arg
        pure $ C cls p
      where
        crefx = tyRefToFQClassName (modulename mn) cnx

mkModuleClasses :: P.CompilerInput -> M.Map P.ModuleName [ClassInfo]
mkModuleClasses (P.CompilerInput ms) = mkClassInfos (M.elems ms)

{- |
This constructs the instances defined in each module (NOT the instances in scope in that module)
-}
mkModuleInstances ::
  P.CompilerInput ->
  M.Map FQClassName Class ->
  Either TypeClassError (M.Map P.ModuleName (S.Set (Tagged Instance)))
mkModuleInstances (P.CompilerInput ms) ctable = foldM go M.empty ms
  where
    go :: M.Map P.ModuleName (S.Set (Tagged Instance)) -> P.Module -> Either TypeClassError (M.Map P.ModuleName (S.Set (Tagged Instance)))
    go acc modl = case traverse (getInstance ctable (modl ^. #moduleName)) (modl ^. #instances) of
      Left e -> Left e
      Right is -> Right $ M.insert (modl ^. #moduleName) (S.fromList is) acc

moduleMap :: P.CompilerInput -> M.Map P.ModuleName P.Module
moduleMap (P.CompilerInput ms) = foldl' (\acc m -> M.insert (m ^. #moduleName) m acc) M.empty ms

{- |
This fetches the *Rules* used as the scope for constraint solving
for a given module.
-}
moduleScope ::
  M.Map P.ModuleName P.Module ->
  M.Map P.ModuleName (S.Set (Tagged Instance)) ->
  P.ModuleName ->
  Either TypeClassError (S.Set (Tagged Instance))
moduleScope modls is = go
  where
    go :: P.ModuleName -> Either TypeClassError (S.Set (Tagged Instance))
    go mn = case modls ^? (ix mn . #imports) of
      Nothing -> Left $ UnknownModule mn
      Just impts -> mconcat <$> traverse goImport (S.toList impts)

    -- NOTE: This doesn't do recursive scope fetching anymore.
    --       If a user wants an instance rule in scope, they
    --       have to import the module (or something from it)
    --       (this is how it works in haskell/ps/rust)
    goImport :: P.ModuleName -> Either TypeClassError (S.Set (Tagged Instance))
    goImport mn = case is ^? ix mn of
      Nothing -> Left $ UnknownModule mn
      Just insts -> Right $ contextualize mn insts

-- imported instances might contain improperly localized RefPs in their Pat
-- i.e. they might be a RefP Nil (Name t), which indicates a local reference,
-- but should be a RefP (ModuleName ...) (Name t) in the scope of the
-- module that imports them
contextualize :: P.ModuleName -> S.Set (Tagged Instance) -> S.Set (Tagged Instance)
contextualize mn = S.map (fmap (fmap go))
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

mkBuilders :: P.CompilerInput -> Either TypeClassError (M.Map P.ModuleName ModuleBuilder)
mkBuilders ci = do
  classTable <- buildClasses classInfos
  insts <- mkModuleInstances ci classTable
  scope <- traverseWithKey (\k _ -> moduleScope modTable insts k) modTable
  foldM (go classTable insts scope) M.empty (M.keys modTable)
  where
    modTable :: M.Map P.ModuleName P.Module
    modTable = moduleMap ci

    classInfos :: M.Map P.ModuleName [ClassInfo]
    classInfos = mkModuleClasses ci

    go ::
      M.Map FQClassName Class ->
      M.Map P.ModuleName (S.Set (Tagged Instance)) ->
      M.Map P.ModuleName (S.Set (Tagged Instance)) ->
      M.Map P.ModuleName ModuleBuilder ->
      P.ModuleName ->
      Either TypeClassError (M.Map P.ModuleName ModuleBuilder)
    go classTable insts scope acc mn = do
      mbinsts <- lookupOr mn insts $ MissingModuleInstances mn
      mbscope <- lookupOr mn scope $ MissingModuleScope mn
      mdule <- lookupOr mn modTable $ UnknownModule mn
      mbclasses <- resolveClasses classTable mn . M.elems $ mdule ^. #classDefs
      let mbtydefs =
            foldl'
              ( \accM t ->
                  M.insert
                    (t ^. #tyName . #name)
                    (Tag (t ^. #sourceInfo) $ defToExp t)
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
      pure $ M.insert mn mb acc

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
