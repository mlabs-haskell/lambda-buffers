module LambdaBuffers.Compiler.TypeClass.Validate (
  -- structural rules
  mkStructuralRules,
  -- pattern vars (for tests)
  _X,
  _XS,
  _VARS,
  _NAME,
  _L,
  _BODY,
  -- main function
  checkDerive,
) where

import Data.List (foldl')

import Data.Map qualified as M
import Data.Set qualified as S
import Data.Text qualified as T

import LambdaBuffers.Compiler.TypeClass.Rules (
  Class,
  Constraint (C),
  Rule ((:<=)),
 )

import LambdaBuffers.Compiler.ProtoCompat.Types qualified as P (
  ModuleName,
 )
import LambdaBuffers.Compiler.TypeClass.Pat (
  Pat (
    AppP,
    DecP,
    Name,
    Nil,
    Opaque,
    ProdP,
    RecP,
    RefP,
    SumP,
    VarP,
    (:*),
    (:=)
  ),
 )
import LambdaBuffers.Compiler.TypeClass.Solve (Overlap, solve)
import LambdaBuffers.Compiler.TypeClass.Utils (ModuleBuilder, TypeClassError (LocalTyRefNotFound, OverlapDetected), lookupOr, mbInstances, mbScope, mbTyDefs, type Instance)

-- hardcoded PATTERN variables, easier to read than (VarP "blah") everywhere
_X, _XS, _VARS, _NAME, _L, _BODY :: Pat
_X = VarP "x"
_XS = VarP "xs"
_VARS = VarP "vars"
_NAME = VarP "name"
_L = VarP "l"
_BODY = VarP "body"

-- Create a set of structural rules for a given class
mkStructuralRules :: Class -> [Rule]
mkStructuralRules c =
  [ C c Nil :<= []
  , C c (_X :* _XS) :<= [C c _X, C c _XS]
  , C c (_L := _X) :<= [C c _X]
  , C c (RecP _XS) :<= [C c _XS]
  , C c (ProdP _XS) :<= [C c _XS]
  , C c (SumP _XS) :<= [C c _XS]
  , C c (DecP _NAME _VARS _BODY) :<= [C c _BODY]
  ]

-- utility
splitInstance :: Instance -> (Constraint, [Constraint])
splitInstance (C c t :<= is) = (C c t, is)

-- utility
assume :: [Constraint] -> [Instance]
assume = map $ \(C c t) -> C c t :<= []

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

-- NOTE: Practically this enforces the "must define instances where types are defined"
--       half of Haskell's orphan instances rule. We could relax that in various ways
--       but it would require reworking a lot of the utilities above.
checkDerive :: P.ModuleName -> ModuleBuilder -> Instance -> Either TypeClassError [Constraint]
checkDerive mn mb i = concat <$> (traverse solveRef =<< catchOverlap (solve assumptions c))
  where
    catchOverlap :: Either Overlap a -> Either TypeClassError a
    catchOverlap = either (Left . OverlapDetected) pure

    (c, cs) = splitInstance i

    localInstances = mbInstances mb

    localTyDefs = mbTyDefs mb

    inScopeInstances = mbScope mb

    solveRef :: Constraint -> Either TypeClassError [Constraint]
    solveRef cstx = case cstx of
      C cx (RefP Nil (Name t)) -> do
        refOf <- lookupOr t localTyDefs $ LocalTyRefNotFound t mn
        catchOverlap $ solve assumptions (C cx refOf)
      other -> pure [other]

    -- TODO(gnumonik): THESE ARE NOT RIGHT. UPDATE TO WORK W/ TyVarP
    assumptions =
      S.toList . S.fromList $
        S.toList inScopeInstances -- the basic set of in-scope instances
          <> concatMap (mkStructuralRules . constraintClass) (c : cs) -- structural rules for all in scope classes
          <> assume cs -- local assumptions, i.e., the `C a` in `instance C a => C (F a)`
          <> S.toList (S.filter (/= i) localInstances) -- all local instances that aren't the one we're trying to check
          <> assumeLocalOpaqueInstances localTyDefs (S.toList localInstances) -- all local instances (i.e. ones to be generated) with an opaque body (treat them as assertions/axioms)
