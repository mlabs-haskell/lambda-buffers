module LambdaBuffers.Compiler.TypeClassCheck.Validate (
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

import Control.Monad.Except (throwError)
import Data.Bifunctor (Bifunctor (bimap))
import LambdaBuffers.Compiler.ProtoCompat.Types qualified as P (
  ModuleName,
 )
import LambdaBuffers.Compiler.TypeClassCheck.Errors (
  BasicConditionViolation (OverlapDetected),
  Instance,
  TypeClassError (BadInstance, LocalTyRefNotFound, MalformedTyDef),
 )
import LambdaBuffers.Compiler.TypeClassCheck.Pat (Exp (DecE), Literal (Opaque), Pat (ConsP, LabelP, LitP, NilP, ProdP, RecP, SumP, VarP), getLocalRefE)
import LambdaBuffers.Compiler.TypeClassCheck.Rules (
  Class,
  Constraint (C),
  Rule ((:<=)),
 )
import LambdaBuffers.Compiler.TypeClassCheck.Solve (Overlap, defTag, inst, solve)
import LambdaBuffers.Compiler.TypeClassCheck.Utils (
  ModuleBuilder (mbInstances, mbScope, mbTyDefs),
  Tagged (Tag),
  getTag,
  lookupOr,
  unTag,
 )

-- hardcoded PATTERN variables, easier to read than (VarP "blah") everywhere
_X, _XS, _VARS, _NAME, _L, _BODY :: Pat
_X = VarP "x"
_XS = VarP "xs"
_VARS = VarP "vars"
_NAME = VarP "name"
_L = VarP "l"
_BODY = VarP "body"

-- Create a set of structural rules for a given class
mkStructuralRules :: Class -> [Tagged (Rule Pat)]
mkStructuralRules c =
  defTag
    <$> [ C c NilP :<= []
        , C c (ConsP _X _XS) :<= [C c _X, C c _XS]
        , C c (LabelP _L _X) :<= [C c _X]
        , C c (RecP _XS) :<= [C c _XS]
        , C c (ProdP _XS) :<= [C c _XS]
        , C c (SumP _XS) :<= [C c _XS]
        , C c (LitP Opaque) :<= []
        ]

-- utility
splitInstance :: Rule a -> (Constraint a, [Constraint a])
splitInstance (C c t :<= is) = (C c t, is)

constraintClass :: Constraint a -> Class
constraintClass (C c _) = c

-- NOTE: Practically this enforces the "must define instances where types are defined"
--       half of Haskell's orphan instances rule. We could relax that in various ways
--       but it would require reworking a lot of the utilities above.
checkDerive :: P.ModuleName -> ModuleBuilder -> Tagged Instance -> Either TypeClassError [Tagged (Constraint Exp)]
checkDerive mn mb ti = concat <$> secondPass
  where
    secondPass :: Either TypeClassError [[Tagged (Constraint Exp)]]
    secondPass = traverse solveRef =<< firstPass

    firstPass :: Either TypeClassError [Tagged (Constraint Exp)]
    firstPass = catchOverlap (solve assumptions c)

    catchOverlap :: Either Overlap a -> Either TypeClassError a
    catchOverlap = \case
      Left ovlp -> Left $ BadInstance (OverlapDetected ovlp) si
      Right x -> pure x

    i = unTag ti
    si = getTag ti
    (c', cs') = bimap defTag (map defTag) $ splitInstance i
    c = fmap inst <$> c'

    localInstances = mbInstances mb

    localTyDefs = mbTyDefs mb

    inScopeInstances = mbScope mb

    solveRef :: Tagged (Constraint Exp) -> Either TypeClassError [Tagged (Constraint Exp)]
    solveRef (Tag i' (C cx ty)) = case getLocalRefE ty of
      Just t -> do
        lookupOr t localTyDefs (LocalTyRefNotFound t mn i') >>= \case
          (Tag _ (DecE _ _ body)) -> catchOverlap $ solve assumptions (defTag $ C cx body)
          xp -> throwError $ MalformedTyDef mn (unTag xp) (getTag xp)
      _ -> pure [Tag i' (C cx ty)]

    assumptions =
      S.toList . S.fromList $
        S.toList inScopeInstances -- the basic set of in-scope instances
          <> concatMap (mkStructuralRules . constraintClass . unTag) (c' : cs') -- structural rules for all in scope classes
          <> S.toList (S.filter (/= ti) localInstances) -- all instances in the current module that aren't the one we're trying to check
