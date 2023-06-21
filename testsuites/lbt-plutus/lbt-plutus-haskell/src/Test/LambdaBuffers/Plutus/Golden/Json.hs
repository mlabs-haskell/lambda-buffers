module Test.LambdaBuffers.Plutus.Golden.Json (writeGoldens, fromToGoldenTest) where

import Data.ByteString qualified as B
import Data.ByteString.Lazy qualified as BL
import Data.List (isPrefixOf)
import Data.Traversable (for)
import LambdaBuffers.Runtime.Prelude (Json, fromJsonBytes, toJsonBytes)
import System.FilePath (takeBaseName, (</>))
import Test.Tasty (TestName, TestTree, testGroup)
import Test.Tasty.Golden (findByExtension, goldenVsString)
import Test.Tasty.HUnit (assertEqual, assertFailure)

findGoldens :: FilePath -> TestName -> IO [(FilePath, String)]
findGoldens goldenDir title = do
  jsonFps <- filter (\fp -> title `isPrefixOf` takeBaseName fp) <$> findByExtension [".json"] goldenDir
  return $ (\fp -> (fp, takeBaseName fp)) <$> jsonFps

writeGoldens :: Json a => FilePath -> TestName -> [a] -> IO [FilePath]
writeGoldens goldenDir title goldens = do
  for (zip [0 :: Integer ..] goldens) $ \(index, golden) -> do
    let
      goldenJson = toJsonBytes golden
      jsonFp = goldenDir </> title <> "." <> show index <> ".json"
    B.writeFile jsonFp goldenJson
    return jsonFp

-- | `fromToGoldenTest goldenDir title goldens`
fromToGoldenTest :: forall {a}. (Json a, Eq a, Show a) => FilePath -> TestName -> [a] -> IO TestTree
fromToGoldenTest goldenDir title goldens =
  do
    goldens' <- findGoldens goldenDir title
    tests' <- for (zip goldens goldens') $ \(golden, (fp, index)) ->
      do
        json <- B.readFile fp
        case fromJsonBytes @a json of
          Left err -> assertFailure $ "Failed parsing " <> fp <> " " <> show err
          Right res -> do
            assertEqual "Golden values should match" golden res
            return $ goldenVsString index fp (return . BL.fromStrict . toJsonBytes $ res)
    return $
      testGroup
        (title <> ": (toJson . fromJson) golden == golden")
        tests'
