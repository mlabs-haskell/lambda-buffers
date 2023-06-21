module Test.LambdaBuffers.Plutus.Golden.Json (writeGoldens, fromToGoldenTest) where

import Data.ByteString qualified as B
import LambdaBuffers.Runtime.Prelude (Json, fromJsonBytes, toJsonBytes)
import Test.LambdaBuffers.Plutus.Golden.Utils qualified as Utils
import Test.Tasty (TestName, TestTree)
import Test.Tasty.HUnit (assertEqual, assertFailure)

writeGoldens :: Json a => FilePath -> TestName -> [a] -> IO [FilePath]
writeGoldens goldenDir title = Utils.writeGoldens goldenDir title ".json"

-- | `fromToGoldenTest goldenDir title goldens`
fromToGoldenTest :: forall {a}. (Json a, Eq a, Show a) => FilePath -> TestName -> [a] -> IO TestTree
fromToGoldenTest goldenDir title =
  Utils.assertGoldens
    goldenDir
    title
    ".json"
    (\x -> "(toJson . fromJson) " <> x <> " == " <> x)
    ( \golden index fp -> do
        json <- B.readFile fp
        case fromJsonBytes @a json of
          Left err -> assertFailure $ show ("Golden bytes should parse as Json" :: String, title, index, fp, err)
          Right res -> do
            assertEqual "Golden values should match" golden res
            assertEqual "Golden bytes should match" json (toJsonBytes res)
    )
