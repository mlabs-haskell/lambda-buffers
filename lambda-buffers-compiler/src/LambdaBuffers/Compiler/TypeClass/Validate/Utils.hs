{-# LANGUAGE OverloadedLabels #-}

module LambdaBuffers.Compiler.TypeClass.Validate.Utils where

import Control.Lens ((^.), (^?))
import Control.Lens.Combinators (Ixed (ix))
import Control.Monad.Except (MonadError (throwError))
import Control.Monad.State (MonadTrans (lift), foldM, runState)
import Control.Monad.Trans.Except (ExceptT, runExceptT)
import Data.List (foldl')
import GHC.Generics (Generic)

import Data.Map qualified as M
import Data.Set qualified as S
import Data.Text qualified as T

import LambdaBuffers.Compiler.ProtoCompat.NameLike (coerceName)
import LambdaBuffers.Compiler.TypeClass.Compat (
  defToPat,
  tyToPat,
 )
import LambdaBuffers.Compiler.TypeClass.Rules (
  Class (Class),
  ClassRef (CRef),
  Constraint (..),
  Instance,
  Rule ((:<=)),
 )

import LambdaBuffers.Compiler.ProtoCompat qualified as P
import LambdaBuffers.Compiler.TypeClass.Pat
import LambdaBuffers.Compiler.TypeClass.Solve (solve)

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

mkClassBuilder :: [P.Module] -> M.Map P.ModuleName [ClassInfo]
mkClassBuilder = foldl' (\acc mod -> M.insert (mod ^. #moduleName) (go mod) acc) M.empty
  where
    defToClassInfo :: P.ModuleName -> P.ClassDef -> ClassInfo
    defToClassInfo mn cd =
      ClassInfo (CRef (cd ^. #className) mn) $
        map (\x -> tyRefToCRef mn $ x ^. #classRef) (cd ^. #supers)

    go :: P.Module -> [ClassInfo]
    go m = map (defToClassInfo $ m ^. #moduleName) (m ^. #classDefs)

tyRefToCRef :: P.ModuleName -> P.TyRef -> ClassRef
tyRefToCRef mn = \case
  P.LocalI lr -> CRef (coerceName $ lr ^. #tyName) mn
  P.ForeignI fr -> CRef (coerceName $ fr ^. #tyName) (fr ^. #moduleName)

toClassMap :: [ClassInfo] -> M.Map ClassRef [ClassRef]
toClassMap = foldl' (\acc (ClassInfo nm sups) -> M.insert nm sups acc) M.empty

buildClasses :: M.Map P.ModuleName [ClassInfo] -> M.Map ClassRef Class
buildClasses cis = foldl' go M.empty (concat $ M.elems cis)
  where
    superclasses = foldl' M.union M.empty $ M.elems (toClassMap <$> cis)

    go :: M.Map ClassRef Class -> ClassInfo -> M.Map ClassRef Class
    go acc (ClassInfo nm sups) =
      let ss = map resolveSuper sups
       in M.insert nm (Class nm ss) acc

    resolveSuper :: ClassRef -> Class
    resolveSuper cr = case superclasses ^? ix cr of
      Nothing -> error $ "classref " <> show cr <> " not found"
      Just sups -> Class cr $ map resolveSuper sups

{-
    Instances
-}

type Instances = S.Set Instance

data InstanceError = UnknownClass ClassRef P.SourceInfo deriving stock (Show)

getInstances :: M.Map ClassRef Class -> P.ModuleName -> [P.InstanceClause] -> Either InstanceError Instances
getInstances ctable mn = foldM go S.empty
  where
    go :: S.Set Instance -> P.InstanceClause -> Either InstanceError Instances
    go acc (P.InstanceClause cn h csts si') = case ctable ^? ix cref of
      Nothing -> throwError $ UnknownClass cref si'
      Just cls -> do
        let p = tyToPat h
        cs <- traverse goConstraint csts
        pure . flip S.insert acc $ C cls p :<= cs
      where
        cref = tyRefToCRef mn cn

    goConstraint :: P.Constraint -> Either InstanceError Constraint
    goConstraint (P.Constraint cn arg si') = case ctable ^? ix cref of
      Nothing -> throwError $ UnknownClass cref si'
      Just cls -> do
        let p = tyToPat arg
        pure $ C cls p
      where
        cref = tyRefToCRef mn cn

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

mkModuleClasses :: P.CompilerInput -> M.Map P.ModuleName [ClassInfo]
mkModuleClasses (P.CompilerInput ms) = mkClassBuilder ms

-- not the in scope instances, just the instances defined in each module
mkModuleInstances ::
  P.CompilerInput ->
  M.Map ClassRef Class ->
  Either InstanceError (M.Map P.ModuleName Instances)
mkModuleInstances (P.CompilerInput ms) ctable = foldM go M.empty ms
  where
    go :: M.Map P.ModuleName Instances -> P.Module -> Either InstanceError (M.Map P.ModuleName Instances)
    go acc modl = case getInstances ctable (modl ^. #moduleName) (modl ^. #instances) of
      Left e -> Left e
      Right is -> Right $ M.insert (modl ^. #moduleName) is acc

moduleMap :: P.CompilerInput -> M.Map P.ModuleName P.Module
moduleMap (P.CompilerInput ms) = foldl' (\acc m -> M.insert (m ^. #moduleName) m acc) M.empty ms

moduleScope ::
  M.Map P.ModuleName P.Module ->
  M.Map P.ModuleName Instances ->
  P.ModuleName ->
  Instances
moduleScope modls is = go
  where
    go :: P.ModuleName -> Instances
    go mn = case modls ^? (ix mn . #imports) of
      Nothing -> error $ "unknown module " <> show mn
      Just impts -> mconcat $ map goImport impts

    goImport :: P.ModuleName -> Instances
    goImport mn = case is ^? ix mn of
      Nothing -> error $ "unknown module " <> show mn
      Just insts -> case modls ^? ix mn of
        Nothing -> error $ "unknown module " <> show mn
        Just m -> insts <> mconcat (map goImport $ m ^. #imports)

-- we make a bunch of maps which are guaranteed to have the same keys,
-- an error on lookup means something has gone HORRIBLY wrong in the code below
unsafeLookup :: Ord k => k -> M.Map k v -> String -> v
unsafeLookup k m err = case M.lookup k m of
  Nothing -> error err
  Just v -> v

mkBuilders :: P.CompilerInput -> M.Map P.ModuleName ModuleBuilder
mkBuilders ci = case mkModuleInstances ci classTable of
  Left e -> error $ show e
  Right insts ->
    let scope = M.mapWithKey (\k _ -> moduleScope modTable insts k) modTable
     in foldl (go insts scope) M.empty (M.keys modTable)
  where
    go ::
      M.Map P.ModuleName Instances ->
      M.Map P.ModuleName Instances ->
      M.Map P.ModuleName ModuleBuilder ->
      P.ModuleName ->
      M.Map P.ModuleName ModuleBuilder
    go insts scope acc mn =
      let mbinsts = unsafeLookup mn insts $ "cannot find instances for module " <> show mn
          mbscope = unsafeLookup mn scope $ "cannot find scope for module " <> show mn
          mdule = unsafeLookup mn modTable $ "cannot find module " <> show mn

          mbtydefs = foldl' (\accM t -> M.insert (t ^. #tyName . #name) (defToPat t) accM) M.empty $ mdule ^. #typeDefs
          mbclasses = resolveClasses mn $ mdule ^. #classDefs
          mb =
            ModuleBuilder
              { mbTyDefs = mbtydefs
              , mbInstances = mbinsts
              , mbClasses = mbclasses
              , mbScope = mbscope
              }
       in M.insert mn mb acc

    resolveClasses :: P.ModuleName -> [P.ClassDef] -> Classes
    resolveClasses _ [] = S.empty
    resolveClasses mn (c : cs) =
      let cls =
            unsafeLookup (CRef (c ^. #className) mn) classTable $
              "no class named "
                <> T.unpack (c ^. (#className . #name))
                <> " found in module "
                <> show mn
       in S.insert cls $ resolveClasses mn cs

    modTable = moduleMap ci

    classInfos = mkModuleClasses ci

    classTable = buildClasses classInfos

---- checks

splitInstance :: Instance -> (Constraint, [Constraint])
splitInstance (C c t :<= is) = (C c t, is)

assume :: [Constraint] -> [Instance]
assume cs = for cs $ \(C c t) -> C c t :<= []

mkStructuralRules :: Class -> [Instance]
mkStructuralRules c =
  [ C c Nil :<= [] -- I'm not sure whether this really has any meaning
  , C c (_x :* _xs) :<= [C c _x, C c _xs]
  , C c (_l := _x) :<= [C c _x]
  , C c (RecP _xs) :<= [C c _xs]
  , C c (ProdP _xs) :<= [C c _xs]
  , C c (SumP _xs) :<= [C c _xs]
  , C c (DecP _name _vars _body) :<= [C c _body]
  ]

constraintClass :: Constraint -> Class
constraintClass (C c _) = c

-- this probably won't work for Opaque TyCons? Need to figure that out
assumeLocalOpaqueInstances :: M.Map T.Text Pat -> [Instance] -> [Instance]
assumeLocalOpaqueInstances localTyScope =
  foldl'
    ( \acc i -> case i of
        C c (RefP Nil (Name t)) :<= [] -> case M.lookup t localTyScope of
          Just (DecP _ _ Opaque) -> C c (RefP Nil (Name t)) :<= [] : acc
          _ -> acc
        _ -> acc
    )
    []

checkDerive :: ModuleBuilder -> Instance -> [Constraint]
checkDerive mb i = solve assumptions c
  where
    assumptions =
      S.toList . S.fromList $
        S.toList inScopeInstances -- the basic set of in-scope instances
          <> concatMap (mkStructuralRules . constraintClass) (c : cs) -- structural rules for all in scope classes
          <> assume cs -- local assumptions, i.e., the `C a` in `instance C a => C (F a)`
          <> S.toList (S.filter (/= i) localInstances) -- all local instances that aren't the one we're trying to check
          <> assumeLocalOpaqueInstances localTyDefs (S.toList localInstances) -- all local instances (i.e. ones to be generated) with an opaque body (treat them as assertions/axioms)
    (c, cs) = splitInstance i
    localInstances = mbInstances mb
    localTyDefs = mbTyDefs mb
    inScopeInstances = mbScope mb

-- an instance C Foo means something different if Foo is Opaque (i.e. that we should just assume the instance is "derived")
