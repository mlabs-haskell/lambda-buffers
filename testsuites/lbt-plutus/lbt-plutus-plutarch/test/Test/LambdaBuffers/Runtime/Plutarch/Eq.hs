module Test.LambdaBuffers.Runtime.Plutarch.Eq (tests) where

import LambdaBuffers.Prelude.Plutarch qualified as Lb
import Plutarch.Builtin.BLS (
  pbls12_381_G1_compressed_generator,
  pbls12_381_G1_compressed_zero,
  pbls12_381_G1_uncompress,
  pbls12_381_G2_compressed_generator,
  pbls12_381_G2_compressed_zero,
  pbls12_381_G2_uncompress,
 )
import Plutarch.Evaluate (evalScript)
import Plutarch.Internal.Term (
  Config (Tracing),
  LogLevel (LogInfo),
  S,
  Term,
  TracingMode (DoTracing),
  compile,
 )
import Plutarch.Prelude (PBool (PTrue), PEq ((#==)), pcon, perror, pif, (#))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertFailure, testCase)

tests :: TestTree
tests =
  testGroup
    "LB Prelude Eq instances (Plutarch)"
    [ testCase "G1Element: an element equals itself" $
        assertTermTrue (g1Element #== g1Element)
    , testCase "G1Element: distinct elements compare unequal" $
        assertTermFalse (g1Element #== g1ZeroElement)
    , testCase "G2Element: an element equals itself" $
        assertTermTrue (g2Element #== g2Element)
    , testCase "G2Element: distinct elements compare unequal" $
        assertTermFalse (g2Element #== g2ZeroElement)
    ]

g1Element :: Term s Lb.G1Element
g1Element = pbls12_381_G1_uncompress # pbls12_381_G1_compressed_generator

g1ZeroElement :: Term s Lb.G1Element
g1ZeroElement = pbls12_381_G1_uncompress # pbls12_381_G1_compressed_zero

g2Element :: Term s Lb.G2Element
g2Element = pbls12_381_G2_uncompress # pbls12_381_G2_compressed_generator

g2ZeroElement :: Term s Lb.G2Element
g2ZeroElement = pbls12_381_G2_uncompress # pbls12_381_G2_compressed_zero

assertTermTrue :: (forall (s :: S). Term s PBool) -> Assertion
assertTermTrue t = assertEvaluationSucceeds (pif t (pcon PTrue) perror)

assertTermFalse :: (forall (s :: S). Term s PBool) -> Assertion
assertTermFalse t = assertEvaluationSucceeds (pif t perror (pcon PTrue))

assertEvaluationSucceeds :: (forall (s :: S). Term s PBool) -> Assertion
assertEvaluationSucceeds t =
  case compile (Tracing LogInfo DoTracing) t of
    Left err -> assertFailure $ "Error while compiling a Plutarch Term: " <> show err
    Right script -> case evalScript script of
      (Left err, _, _) -> assertFailure $ "Error while evaluating a Plutarch Term: " <> show err
      _ -> pure ()
