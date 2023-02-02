{-# LANGUAGE OverloadedLabels #-}

module LambdaBuffers.Compiler.TypeClass.Validate.Utils (
  TypeClassError (..),
  ModuleBuilder (..),
  -- for the superclass cycle check
  mkClassInfos,
  toClassMap,
  checkDerive,
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

import LambdaBuffers.Compiler.ProtoCompat.NameLike (coerceName)
import LambdaBuffers.Compiler.TypeClass.Compat (
  defToPat,
  modulename,
  tyToPat,
 )
import LambdaBuffers.Compiler.TypeClass.Rules (
  Class (Class),
  ClassRef (CRef),
  Constraint (C),
  Instance,
  Rule ((:<=)),
  mapPat,
 )

import LambdaBuffers.Compiler.ProtoCompat.Types qualified as P (
  ClassDef,
  ClassName,
  CompilerInput (CompilerInput),
  Constraint (Constraint),
  InstanceClause (InstanceClause),
  Module,
  ModuleName,
  SourceInfo,
  TyRef (..),
 )
import LambdaBuffers.Compiler.TypeClass.Pat (
  Pat (
    AppP,
    DecP,
    ModuleName,
    Name,
    Nil,
    Opaque,
    ProdP,
    RecP,
    RefP,
    SumP,
    (:*),
    (:=)
  ),
  for,
  _body,
  _l,
  _name,
  _vars,
  _x,
  _xs,
 )
import LambdaBuffers.Compiler.TypeClass.Pretty (pointies)
import LambdaBuffers.Compiler.TypeClass.Solve (solve)
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

{- Some of these are perfunctory & used to keep functions total.
-}
data TypeClassError
  = UnknownClass ClassRef P.SourceInfo
  | UnknownModule P.ModuleName
  | MissingModuleInstances P.ModuleName
  | MissingModuleScope P.ModuleName
  | ClassNotFoundInModule P.ClassName P.ModuleName
  | LocalTyRefNotFound T.Text P.ModuleName
  | SuperclassCycleDetected [[ClassRef]]
  | CouldntSolveConstraints P.ModuleName [Constraint] Instance
  deriving stock (Show)

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
        <+> pretty (cn ^. #name)
        <+> "in module"
        <+> pretty mn
        <+> "but it isn't there!"
    LocalTyRefNotFound txt mn ->
      "Error: Expected to find a type definition for a type named"
        <+> pretty txt
        <+> "in module"
        <+> pretty mn
        <+> "but it isn't there!"
    SuperclassCycleDetected crs ->
      "Error: Superclass cycles detected in compiler input:"
        <+> nest
          2
          ( vcat
              . map (hcat . punctuate " => " . map pretty)
              $ crs
          )
    CouldntSolveConstraints mn cs i ->
      "Error: Could not derive instance:"
        <+> pointies (pretty i)
          <> line
          <> line
          <> indent 2 "in module"
        <+> pretty mn
          <> line
          <> line
          <> indent 2 "because the following constraint(s) were not satisfied:"
          <> line
          <> line
          <> indent 2 (vcat $ map pretty cs)

{- This contains:
     - Everything needed to validate instances (in the form needed to do so)
     - Everything needed for codegen (modulo sourceInfo)
-}
data ModuleBuilder = ModuleBuilder
  { mbTyDefs :: M.Map T.Text Pat -- sourceInfo needs to be in here somehow (these are all local refs)
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
data ClassInfo = ClassInfo {ciName :: ClassRef, ciSupers :: [ClassRef]}
  deriving stock (Show, Eq, Ord, Generic)

type Classes = S.Set Class

mkClassInfos :: [P.Module] -> M.Map P.ModuleName [ClassInfo]
mkClassInfos = foldl' (\acc mdl -> M.insert (mdl ^. #moduleName) (go mdl) acc) M.empty
  where
    go :: P.Module -> [ClassInfo]
    go m = map (defToClassInfo $ m ^. #moduleName) (m ^. #classDefs)

defToClassInfo :: P.ModuleName -> P.ClassDef -> ClassInfo
defToClassInfo mn cd =
  ClassInfo (CRef (cd ^. #className) mn) $
    map (\x -> tyRefToCRef mn $ x ^. #classRef) (cd ^. #supers)

tyRefToCRef :: P.ModuleName -> P.TyRef -> ClassRef
tyRefToCRef mn = \case
  P.LocalI lr -> CRef (coerceName $ lr ^. #tyName) mn
  P.ForeignI fr -> CRef (coerceName $ fr ^. #tyName) (fr ^. #moduleName)

toClassMap :: [ClassInfo] -> M.Map ClassRef [ClassRef]
toClassMap = foldl' (\acc (ClassInfo nm sups) -> M.insert nm sups acc) M.empty

buildClasses :: M.Map P.ModuleName [ClassInfo] -> Either TypeClassError (M.Map ClassRef Class)
buildClasses cis = foldM go M.empty (concat $ M.elems cis)
  where
    superclasses = foldl' M.union M.empty $ M.elems (toClassMap <$> cis)

    go :: M.Map ClassRef Class -> ClassInfo -> Either TypeClassError (M.Map ClassRef Class)
    go acc (ClassInfo nm sups) = do
      ss <- traverse resolveSuper sups
      pure $ M.insert nm (Class nm ss) acc

    resolveSuper :: ClassRef -> Either TypeClassError Class
    resolveSuper cr@(CRef cn mn) = do
      sups <- lookupOr cr superclasses $ ClassNotFoundInModule cn mn
      Class cr <$> traverse resolveSuper sups

{-
    Instances
-}

type Instances = S.Set Instance

getInstances :: M.Map ClassRef Class -> P.ModuleName -> [P.InstanceClause] -> Either TypeClassError Instances
getInstances ctable mn = foldM go S.empty
  where
    go :: S.Set Instance -> P.InstanceClause -> Either TypeClassError Instances
    go acc (P.InstanceClause cn h csts si') = case ctable ^? ix cref of
      Nothing -> throwError $ UnknownClass cref si'
      Just cls -> do
        let p = tyToPat h
        cs <- traverse goConstraint csts
        pure . flip S.insert acc $ C cls p :<= cs
      where
        cref = tyRefToCRef mn cn

    goConstraint :: P.Constraint -> Either TypeClassError Constraint
    goConstraint (P.Constraint cn arg si') = case ctable ^? ix cref of
      Nothing -> throwError $ UnknownClass cref si'
      Just cls -> do
        let p = tyToPat arg
        pure $ C cls p
      where
        cref = tyRefToCRef mn cn

mkModuleClasses :: P.CompilerInput -> M.Map P.ModuleName [ClassInfo]
mkModuleClasses (P.CompilerInput ms) = mkClassInfos ms

-- not the in scope instances, just the instances defined in each module
mkModuleInstances ::
  P.CompilerInput ->
  M.Map ClassRef Class ->
  Either TypeClassError (M.Map P.ModuleName Instances)
mkModuleInstances (P.CompilerInput ms) ctable = foldM go M.empty ms
  where
    go :: M.Map P.ModuleName Instances -> P.Module -> Either TypeClassError (M.Map P.ModuleName Instances)
    go acc modl = case getInstances ctable (modl ^. #moduleName) (modl ^. #instances) of
      Left e -> Left e
      Right is -> Right $ M.insert (modl ^. #moduleName) is acc

moduleMap :: P.CompilerInput -> M.Map P.ModuleName P.Module
moduleMap (P.CompilerInput ms) = foldl' (\acc m -> M.insert (m ^. #moduleName) m acc) M.empty ms

moduleScope ::
  M.Map P.ModuleName P.Module ->
  M.Map P.ModuleName Instances ->
  P.ModuleName ->
  Either TypeClassError Instances
moduleScope modls is = go
  where
    go :: P.ModuleName -> Either TypeClassError Instances
    go mn = case modls ^? (ix mn . #imports) of
      Nothing -> Left $ UnknownModule mn
      Just impts -> mconcat <$> traverse goImport impts

    goImport :: P.ModuleName -> Either TypeClassError Instances
    goImport mn = case is ^? ix mn of
      Nothing -> Left $ UnknownModule mn
      Just insts -> case modls ^? ix mn of
        Nothing -> Left $ UnknownModule mn
        Just m -> (contextualize mn insts <>) . mconcat <$> traverse goImport (m ^. #imports)

-- imported instances might contain improperly localized RefPs in their Pat
-- i.e. they might be a RefP Nil (Name t), which indicates a local reference,
-- but should be a RefP (ModuleName ...) (Name t) in the scope of the
-- module that imports them
contextualize :: P.ModuleName -> Instances -> Instances
contextualize mn = S.map (mapPat go)
  where
    go :: Pat -> Pat
    go (RefP Nil t) = RefP (ModuleName $ modulename mn) t
    go (p1 := p2) = go p1 := go p2
    go (p1 :* p2) = go p1 :* go p2
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
      M.Map ClassRef Class ->
      M.Map P.ModuleName Instances ->
      M.Map P.ModuleName Instances ->
      M.Map P.ModuleName ModuleBuilder ->
      P.ModuleName ->
      Either TypeClassError (M.Map P.ModuleName ModuleBuilder)
    go classTable insts scope acc mn = do
      mbinsts <- lookupOr mn insts $ MissingModuleInstances mn
      mbscope <- lookupOr mn scope $ MissingModuleScope mn
      mdule <- lookupOr mn modTable $ UnknownModule mn
      mbclasses <- resolveClasses classTable mn $ mdule ^. #classDefs
      let mbtydefs =
            foldl'
              ( \accM t ->
                  M.insert
                    (t ^. #tyName . #name)
                    (defToPat t)
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
      M.Map ClassRef Class ->
      P.ModuleName ->
      [P.ClassDef] ->
      Either TypeClassError Classes
    resolveClasses _ _ [] = pure S.empty
    resolveClasses classTable mn (c : cs) = do
      cls <-
        lookupOr (CRef (c ^. #className) mn) classTable $
          ClassNotFoundInModule (c ^. #className) mn
      S.insert cls <$> resolveClasses classTable mn cs

---- checks

splitInstance :: Instance -> (Constraint, [Constraint])
splitInstance (C c t :<= is) = (C c t, is)

assume :: [Constraint] -> [Instance]
assume cs = for cs $ \(C c t) -> C c t :<= []

mkStructuralRules :: Class -> [Instance]
mkStructuralRules c =
  [ C c Nil :<= []
  , C c (_x :* _xs) :<= [C c _x, C c _xs]
  , C c (_l := _x) :<= [C c _x]
  , C c (RecP _xs) :<= [C c _xs]
  , C c (ProdP _xs) :<= [C c _xs]
  , C c (SumP _xs) :<= [C c _xs]
  , C c (DecP _name _vars _body) :<= [C c _body]
  ]

constraintClass :: Constraint -> Class
constraintClass (C c _) = c

{- We presume that all locally defined instances of the forms:
     1. instance C Opaque0
     2. instance C var => C (Opaque1 var)
   Should be interpreted as axioms, and, therefore, are
   automagically solved.
-}
assumeLocalOpaqueInstances :: M.Map T.Text Pat -> [Instance] -> [Instance]
assumeLocalOpaqueInstances localTyScope =
  foldl'
    ( \acc i -> case i of
        cx@(C _ (RefP Nil (Name t)) :<= []) -> case M.lookup t localTyScope of
          Just (DecP _ _ Opaque) -> cx : acc
          _ -> acc
        cx@(C _ (AppP (RefP Nil (Name t)) _) :<= _) -> case M.lookup t localTyScope of
          Just (DecP _ _ Opaque) -> cx : acc
          _ -> acc
        _ -> acc
    )
    []

-- TODO(gnumonik): It might make sense to move the "lookup local refs" thing into `solve` directly.
-- NOTE: Practically this enforces the "must define instances where types are defined"
--       half of Haskell's orphan instances rule. We could relax that in various ways
--       but it would require reworking a lot of the utilities above.
checkDerive :: P.ModuleName -> ModuleBuilder -> Instance -> Either TypeClassError [Constraint]
checkDerive mn mb i = fmap concat . traverse solveRef $ solve assumptions c
  where
    (c, cs) = splitInstance i

    localInstances = mbInstances mb

    localTyDefs = mbTyDefs mb

    inScopeInstances = mbScope mb

    solveRef cstx = case cstx of
      C cx (RefP Nil (Name t)) -> do
        refOf <- lookupOr t localTyDefs $ LocalTyRefNotFound t mn
        pure $ solve assumptions (C cx refOf)
      other -> pure [other]

    -- TODO(gnumonik): Quadruple check that these are the correct assumptions
    assumptions =
      S.toList . S.fromList $
        S.toList inScopeInstances -- the basic set of in-scope instances
          <> concatMap (mkStructuralRules . constraintClass) (c : cs) -- structural rules for all in scope classes
          <> assume cs -- local assumptions, i.e., the `C a` in `instance C a => C (F a)`
          <> S.toList (S.filter (/= i) localInstances) -- all local instances that aren't the one we're trying to check
          <> assumeLocalOpaqueInstances localTyDefs (S.toList localInstances) -- all local instances (i.e. ones to be generated) with an opaque body (treat them as assertions/axioms)
