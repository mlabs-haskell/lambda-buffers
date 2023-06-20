module Test.LambdaBuffers.Plutus.Golden.PlutusData (writeGoldens, fromToGoldenTest) where

import Control.Monad (when)
import Data.ByteString qualified as B
import Data.ByteString.Lazy qualified as BL
import Data.List (stripPrefix)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Traversable (for)
import LambdaBuffers.Runtime.Plutus ()
import LambdaBuffers.Runtime.Prelude (fromJsonBytes, toJsonBytes)
import PlutusTx qualified
import System.FilePath (splitExtension, takeFileName, (</>))
import Test.Tasty (TestName, TestTree, testGroup)
import Test.Tasty.Golden (findByExtension, goldenVsString)
import Test.Tasty.HUnit (assertEqual, assertFailure)

findGoldens :: FilePath -> TestName -> IO (Map String FilePath)
findGoldens goldenDir title =
  Map.fromList
    . filterMap
      ( \fp ->
          let
            (rest, _dotJson) = splitExtension fp
            (rest', dotPd) = splitExtension rest
            (rest'', dotIndex) = splitExtension rest'
            title' = takeFileName rest''
           in
            if title' == title && dotPd == ".pd"
              then do
                index <- stripPrefix "." dotIndex
                return (index, fp)
              else Nothing
      )
    <$> findByExtension [".json"] goldenDir

filterMap :: forall {t} {a}. (t -> Maybe a) -> [t] -> [a]
filterMap _predMap [] = []
filterMap predMap (x : xs) = case predMap x of
  Nothing -> filterMap predMap xs
  Just y -> y : filterMap predMap xs

writeGoldens :: PlutusTx.ToData a => FilePath -> TestName -> [a] -> IO [FilePath]
writeGoldens goldenDir title goldens = do
  for (zip [0 :: Integer ..] goldens) $ \(index, golden) -> do
    let
      goldenPd = PlutusTx.toData golden
      goldenPdJson = toJsonBytes goldenPd
      jsonFp = goldenDir </> title <> "." <> show index <> ".pd.json"
    B.writeFile jsonFp goldenPdJson
    return jsonFp

-- | `fromToGoldenTest goldenDir title goldens`
fromToGoldenTest :: forall {a}. (PlutusTx.FromData a, PlutusTx.ToData a, Eq a, Show a) => FilePath -> TestName -> [a] -> IO TestTree
fromToGoldenTest goldenDir title goldens =
  do
    goldens' <- findGoldens goldenDir title
    when (null goldens') $ assertFailure (show ("Expected to find some goldens" :: String, title, goldenDir, "Did you forget to (re)generate goldens?" :: String))
    tests' <- for (zip goldens [(0 :: Int) .. (length goldens' - 1)]) $ \(golden, index) ->
      do
        case Map.lookup (show index) goldens' of
          Nothing -> assertFailure $ show ("Golden value not in golden file" :: String, title, index)
          Just fp -> do
            pdJson <- B.readFile fp
            case fromJsonBytes @PlutusTx.Data pdJson of
              Left err -> assertFailure $ show ("Failed parsing PlutusData from Json" :: String, title, fp, err)
              Right pd -> do
                case PlutusTx.fromData @a pd of
                  Nothing -> assertFailure $ show ("Failed parsing PlutusData" :: String, title, fp)
                  Just res -> do
                    assertEqual (show ("Golden values should match" :: String, title, fp)) golden res
                    return $ goldenVsString (show (title, index)) fp (return . BL.fromStrict . toJsonBytes . PlutusTx.toData $ res)
    return $
      testGroup
        (title <> ": (toJson . toPlutusData . fromPlutusData . fromJson) golden == golden")
        tests'
