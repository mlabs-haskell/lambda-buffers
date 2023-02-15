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

import Data.Set qualified as S

import LambdaBuffers.Compiler.TypeClass.Rules (
  Class,
  Constraint (C),
  Rule ((:<=)),
 )

import Debug.Trace (trace)
import LambdaBuffers.Compiler.ProtoCompat.Types qualified as P (
  ModuleName,
 )
import LambdaBuffers.Compiler.TypeClass.Pat
import LambdaBuffers.Compiler.TypeClass.Solve (Overlap, inst, solve)
import LambdaBuffers.Compiler.TypeClass.Utils (
  BasicConditionViolation (OverlapDetected),
  ModuleBuilder (mbInstances, mbScope, mbTyDefs),
  TypeClassError (BadInstance, LocalTyRefNotFound),
  lookupOr,
 )
import Prettyprinter (Pretty (..), vcat)

-- hardcoded PATTERN variables, easier to read than (VarP "blah") everywhere
_X, _XS, _VARS, _NAME, _L, _BODY :: Pat
_X = VarP "x"
_XS = VarP "xs"
_VARS = VarP "vars"
_NAME = VarP "name"
_L = VarP "l"
_BODY = VarP "body"

-- Create a set of structural rules for a given class
mkStructuralRules :: Class -> [Rule Pat]
mkStructuralRules c =
  [ C c NilP :<= []
  , C c (ConsP _X _XS) :<= [C c _X, C c _XS]
  , C c (LabelP _L _X) :<= [C c _X]
  , C c (RecP _XS) :<= [C c _XS]
  , C c (ProdP _XS) :<= [C c _XS]
  , C c (SumP _XS) :<= [C c _XS]
  , C c (DecP _NAME _VARS _BODY) :<= [C c _BODY]
  ]

-- utility
splitInstance :: Rule a -> (Constraint a, [Constraint a])
splitInstance (C c t :<= is) = (C c t, is)

{- NOTE: This is *only* a safe thing to do in cases where:
           1) The instances have all been kind-checked
           2) The instances have been checked for basic condition violations (specifically conditions 1 & 2, see Utils.hs)
           3) Steps 1 & 2 were performed IN THAT ORDER

         If the above checks have not been performed, a user can short-circuit all of our
         typeclass validation by doing something stupid like `instance Foo a => Foo a`
         (that's not the only thing that can go wrong, checking for duplicates isn't enough)
-}
assume :: [Constraint a] -> [Rule a]
assume = map $ \(C c t) -> C c t :<= []

constraintClass :: Constraint a -> Class
constraintClass (C c _) = c

lower :: Pat -> Pat
lower = \case
  VarP v -> LitP (TyVar v)
  ConsP x xs -> ConsP (lower x) (lower xs)
  LabelP l x -> LabelP (lower l) (lower x)
  ProdP xs -> ProdP (lower xs)
  RecP xs -> RecP (lower xs)
  SumP xs -> SumP (lower xs)
  AppP t1 t2 -> AppP (lower t1) (lower t2)
  RefP n x -> RefP (lower n) (lower x)
  DecP a b c -> DecP (lower a) (lower b) (lower c)
  LitP l -> LitP l
  NilP -> NilP

-- NOTE: Practically this enforces the "must define instances where types are defined"
--       half of Haskell's orphan instances rule. We could relax that in various ways
--       but it would require reworking a lot of the utilities above.
checkDerive :: P.ModuleName -> ModuleBuilder -> Rule Pat -> Either TypeClassError [Constraint Exp]
checkDerive mn mb i = trace traceStr concat <$> (traverse solveRef =<< catchOverlap (solve assumptions c))
  where
    traceStr = show $ vcat (map pretty assumptions)

    catchOverlap :: Either Overlap a -> Either TypeClassError a
    catchOverlap = either (Left . BadInstance . OverlapDetected) pure

    (c', cs') = splitInstance i

    cs = fmap (fmap lower) cs'

    c = fmap inst c'

    localInstances = mbInstances mb

    localTyDefs = mbTyDefs mb

    inScopeInstances = mbScope mb

    solveRef :: Constraint Exp -> Either TypeClassError [Constraint Exp]
    solveRef cstx = trace ("CSTX " <> show cstx) $ case cstx of
      C cx (RefE NilE (LitE (Name t))) -> do
        refOf <- lookupOr t localTyDefs $ LocalTyRefNotFound t mn
        case refOf of
          DecE _ _ (LitE Opaque) -> pure []
          other -> pure [cstx]
      other -> trace (show $ pretty other) $ pure [other]

    assumptions =
      S.toList . S.fromList $
        S.toList inScopeInstances -- the basic set of in-scope instances
          <> concatMap (mkStructuralRules . constraintClass) (c' : cs') -- structural rules for all in scope classes
          <> assume cs -- local assumptions, i.e., the `C a` in `instance C a => C (F a)`
          <> S.toList (S.filter (/= i) localInstances) -- all instances in the current module that aren't the one we're trying to check
