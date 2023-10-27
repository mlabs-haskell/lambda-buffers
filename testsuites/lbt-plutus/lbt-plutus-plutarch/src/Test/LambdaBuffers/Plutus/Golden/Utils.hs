module Test.LambdaBuffers.Plutus.Golden.Utils (findGoldens, writeGoldens, assertGoldens) where

import Control.Monad (when)
import Data.ByteString qualified as B
import Data.List (intercalate)
import Data.List.Split (splitOn)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Traversable (for)
import Debug.Trace qualified as Debug
import LambdaBuffers.Runtime.Prelude (Json, toJsonBytes)
import System.Directory (listDirectory)
import System.FilePath (takeFileName, (</>))
import Test.Tasty (TestName, TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertFailure, testCase)

findGoldens :: FilePath -> String -> TestName -> IO (Map String FilePath)
findGoldens goldenDir ext title =
  Map.fromList
    . filterMap
      ( \fp ->
          let
            filename = Debug.trace (takeFileName fp) (takeFileName fp)
           in
            case splitOn ext filename of
              [titleThenIndex, ""] -> case reverse $ splitOn "." titleThenIndex of
                (index : rtitle) ->
                  if title == (intercalate "." . reverse $ rtitle)
                    then Just (index, goldenDir </> fp)
                    else Nothing
                _ -> Nothing
              _ -> Nothing
      )
    <$> listDirectory goldenDir

filterMap :: forall {t} {a}. (t -> Maybe a) -> [t] -> [a]
filterMap _predMap [] = []
filterMap predMap (x : xs) = case predMap x of
  Nothing -> filterMap predMap xs
  Just y -> y : filterMap predMap xs

writeGoldens :: Json a => FilePath -> TestName -> String -> [a] -> IO [FilePath]
writeGoldens goldenDir title ext goldens = do
  for (zip [0 :: Integer ..] goldens) $ \(index, golden) -> do
    let
      goldenJson = toJsonBytes golden
      jsonFp = goldenDir </> title <> "." <> show index <> ext
    B.writeFile jsonFp goldenJson
    return jsonFp

-- | `assertGoldens goldenDir title ext assert goldens`
assertGoldens :: forall {a}. FilePath -> TestName -> String -> (String -> String) -> (a -> Int -> FilePath -> Assertion) -> [a] -> IO TestTree
assertGoldens goldenDir title ext propTitle assert goldens = do
  goldens' <- findGoldens goldenDir ext title
  when (null goldens') $
    assertFailure (show ("Expected to find some goldens" :: String, title, ext, "Did you forget to (re)generate goldens?" :: String, goldenDir))
  tests' <- for (zip goldens [(0 :: Int) .. (length goldens' - 1)]) $ \(golden, index) -> return $ testCase (show index) $ do
    fp <- case Map.lookup (show index) goldens' of
      Nothing -> assertFailure $ show ("Golden value index not in goldens" :: String, title, index)
      Just fp -> return fp
    assert golden index fp
  return $
    testGroup
      ("forall (golden : " <> title <> ".*" <> ext <> ")" <> ": " <> propTitle "golden")
      tests'
