module Test.LambdaBuffers.Runtime.Plutarch (test) where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Hedgehog qualified as H
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import LambdaBuffers.Runtime.Plutarch (PList)
import LambdaBuffers.Runtime.Plutarch qualified as Lb
import Plutarch (ClosedTerm, Config (Config), TracingMode (DoTracingAndBinds), compile, pcon, perror)
import Plutarch.Evaluate (evalScript)
import Plutarch.Prelude (PBool (PTrue), PEq ((#==)), PInteger, pconstant, pif)
import Test.Tasty (TestTree, adjustOption, testGroup)
import Test.Tasty.HUnit (assertFailure)
import Test.Tasty.Hedgehog (testProperty)
import Test.Tasty.Hedgehog qualified as H

test :: TestTree
test =
  adjustOption (\_ -> H.HedgehogTestLimit $ Just 1000) $
    testGroup
      "PList tests"
      [ testProperty "forall xs :: [Integer] ys :: [Integer]. (xs == ys) === evalEq (plistFrom xs) (plistFrom ys)" $
          H.property $
            H.forAll
              ((,) <$> genInts <*> genInts)
              >>= ( \(xs, ys) -> do
                      b <- liftIO $ evalEq (Lb.plistFrom $ pconstant <$> xs) (Lb.plistFrom $ pconstant <$> ys)
                      (xs == ys) H.=== b
                  )
      , testProperty "forall xs :: [Integer]. evalEq (plistCase plistCons plistNil (plistFrom xs)) (plistFrom xs)" $
          H.property $
            H.forAll
              genInts
              >>= ( \xs -> do
                      b <- liftIO $ evalEq (Lb.plistCase Lb.plistCons Lb.plistNil (Lb.plistFrom $ pconstant <$> xs)) (Lb.plistFrom $ pconstant <$> xs)
                      True H.=== b
                  )
      ]
  where
    genInts :: H.Gen [Integer]
    genInts = Gen.list (Range.linear 0 100) (Gen.integral (Range.linear 0 100))

evalEq :: ClosedTerm (PList PInteger) -> ClosedTerm (PList PInteger) -> IO Bool
evalEq l r =
  let
    t :: ClosedTerm PBool
    t = pif (l #== r) (pcon PTrue) perror
   in
    case Plutarch.compile (Config DoTracingAndBinds) t of
      Left err -> assertFailure $ show ("Error while compiling a Plutarch Term" :: String, err)
      Right script -> case evalScript script of
        (Left _err, _, _) -> return False
        _ -> return True
