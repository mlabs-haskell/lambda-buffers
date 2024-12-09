module Test.LambdaBuffers.Runtime.Plutarch (test) where

-- import Control.Monad.IO.Class (MonadIO (liftIO))
-- import Hedgehog qualified as H
-- import Hedgehog.Gen qualified as Gen
-- import Hedgehog.Range qualified as Range
-- import LambdaBuffers.Runtime.Plutarch (PList)
-- import LambdaBuffers.Runtime.Plutarch qualified as Lb
-- import Plutarch.Prelude (
--     ClosedTerm, pcon, perror, plet, (#)
--     ,PBool (PTrue), PEq ((#==)), PListLike (pcons, pelimList, pnil), pconstant, pif, ptrace)
-- import Plutarch.Evaluate (evalScript)
-- import Plutarch.Internal.Term (
--   Config (Tracing),
--   LogLevel (LogInfo),
--   TracingMode (DoTracing),
--   compile,
--  )
import Test.Tasty (TestTree, adjustOption, testGroup)

-- import Test.Tasty.HUnit (assertFailure)
-- import Test.Tasty.Hedgehog (testProperty)
import Test.Tasty.Hedgehog qualified as H

test :: TestTree
test =
  adjustOption (\_ -> H.HedgehogTestLimit $ Just 1000) $
    testGroup
      "PList tests"
      []

--       [ testProperty "forall xs :: [Integer] ys :: [Integer]. (xs == ys) === evalEq (plistFrom xs) (plistFrom ys)" $
--           H.property $
--             H.forAll
--               ((,) <$> genInts <*> genInts)
--               >>= ( \(xs, ys) -> do
--                       b <- liftIO $ evalBool (Lb.plistFrom @PList (pconstant <$> xs) #== Lb.plistFrom (pconstant <$> ys))
--                       (xs == ys) H.=== b
--                   )
--       , testProperty "forall xs :: [Integer]. evalEq (pelimList pcons pnil (plistFrom xs)) (plistFrom xs)" $
--           H.property $
--             H.forAll
--               genInts
--               >>= ( \xs -> do
--                       b <-
--                         liftIO $
--                           evalBool
--                             (plet (Lb.plistFrom @PList $ pconstant <$> xs) $ \xs' -> pelimList (\x t -> pcons # x # t) pnil xs' #== xs')
--                       True H.=== b
--                   )
--       ]
--   where
--     -- WARN(bladyjoker): If I put the list size to >=56 the second test breaks.
--     genInts :: H.Gen [Integer]
--     genInts = Gen.list (Range.linear 0 55) (Gen.integral (Range.linear 0 100))

-- evalBool :: ClosedTerm PBool -> IO Bool
-- evalBool t =
--   let
--     t' :: ClosedTerm PBool
--     t' = pif t (pcon PTrue) (ptrace "Got False" perror)
--    in
--     case compile (Tracing LogInfo DoTracing) t' of
--       Left err -> assertFailure $ show ("Error while compiling a Plutarch Term" :: String, err)
--       Right script -> case evalScript script of
--         (Left _err, _, _trace) -> return False
--         _ -> return True
