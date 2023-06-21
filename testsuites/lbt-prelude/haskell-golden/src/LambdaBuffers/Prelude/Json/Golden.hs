module LambdaBuffers.Prelude.Json.Golden (aGoldens, bGoldens, cGoldens, dGoldens, dayGoldens, workDayGoldens, integerGoldens, boolGoldens, bytesGoldens, charGoldens, textGoldens, maybeGoldens, eitherGoldens, listGoldens, setGoldens, mapGoldens, freeDayGoldens, fromToGoldenTest, writeGoldens) where

import Control.Monad (when)
import Data.ByteString qualified as B
import Data.ByteString.Lazy qualified as BL
import Data.List (isPrefixOf)
import Data.Map qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as Text
import Data.Traversable (for)
import LambdaBuffers.Days (Day (Day'Friday, Day'Monday, Day'Saturday, Day'Sunday, Day'Thursday, Day'Tuesday, Day'Wednesday), FreeDay (FreeDay), WorkDay (WorkDay))
import LambdaBuffers.Foo (A (A), B (B), C (C), D (D))
import LambdaBuffers.Foo.Bar (FooComplicated (FooComplicated), FooProd (FooProd), FooRec (FooRec), FooSum (FooSum'Bar, FooSum'Baz, FooSum'Faz, FooSum'Foo, FooSum'Qax))
import LambdaBuffers.Runtime.Prelude (Json, fromJsonBytes, toJsonBytes)
import System.FilePath (takeBaseName, (</>))
import Test.Tasty (TestName, TestTree, testGroup)
import Test.Tasty.Golden (findByExtension, goldenVsString)
import Test.Tasty.HUnit (assertFailure)

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
fromToGoldenTest :: forall {a}. (Json a) => FilePath -> TestName -> [a] -> IO TestTree
fromToGoldenTest goldenDir title goldens =
  do
    goldens' <- findGoldens goldenDir title
    when (length goldens' /= length goldens) $ do
      assertFailure $ "Expected to find " <> show (length goldens) <> " .json golden files for " <> title <> ", but got " <> show (length goldens') <> ". Forgot to (re)generate the goldens?"
    tests' <- for goldens' $ \(fp, index) ->
      do
        json <- B.readFile fp
        case fromJsonBytes @a json of
          Left err -> assertFailure $ "Failed parsing " <> fp <> " " <> show err
          Right res -> return $ goldenVsString index fp (return . BL.fromStrict . toJsonBytes $ res)
    return $
      testGroup
        (title <> ": (toJson . fromJson) golden == golden")
        tests'

fooSumGoldens :: a -> b -> c -> [FooSum a b c]
fooSumGoldens x y z =
  [ FooSum'Foo x y z
  , FooSum'Bar x y
  , FooSum'Baz y
  , FooSum'Qax
  , FooSum'Faz 0
  ]

aGoldens :: [A]
aGoldens = A <$> fooSumGoldens 1337 False "some bytes"

fooProdGoldens :: a -> b -> c -> [FooProd a b c]
fooProdGoldens x y z = [FooProd x y z 1337]

bGoldens :: [B]
bGoldens = B <$> fooProdGoldens 1337 False "some bytes"

fooRecGoldens :: a -> b -> c -> [FooRec a b c]
fooRecGoldens x y z = [FooRec x y z 1337]

cGoldens :: [C]
cGoldens = C <$> fooRecGoldens 1337 False "some bytes"

dGoldens :: [D]
dGoldens = do
  fooSum <- fooSumGoldens 1337 False "some bytes"
  fooProd <- fooProdGoldens 1337 False "some bytes"
  fooRec <- fooRecGoldens 1337 False "some bytes"
  return (D $ FooComplicated fooSum fooProd fooRec)

dayGoldens :: [Day]
dayGoldens = [Day'Monday, Day'Tuesday, Day'Wednesday, Day'Thursday, Day'Friday, Day'Saturday, Day'Sunday]

workDayGoldens :: [WorkDay]
workDayGoldens = WorkDay <$> [Day'Monday, Day'Tuesday, Day'Wednesday, Day'Thursday, Day'Friday]

freeDayGoldens :: [FreeDay]
freeDayGoldens = FreeDay <$> [Day'Saturday, Day'Sunday]

boolGoldens :: [Bool]
boolGoldens = [True, False]

integerGoldens :: [Integer]
integerGoldens =
  [ 0 :: Integer
  , 1
  , -1
  , 2 ^ (32 :: Integer)
  , -(2 ^ (32 :: Integer))
  , 2 ^ (64 :: Integer)
  , -(2 ^ (64 :: Integer))
  , 2 ^ (128 :: Integer)
  , -(2 ^ (128 :: Integer))
  , 2 ^ (256 :: Integer)
  , -(2 ^ (256 :: Integer))
  ]

bytesGoldens :: [B.ByteString]
bytesGoldens = [B.empty, B.singleton 0, B.pack [0x73, 0x6f, 0x6d, 0x65, 0x20, 0x62, 0x79, 0x74, 0x65, 0x73]]

charGoldens :: [Char]
charGoldens = ['\0', '\n', '\x1F643']

textGoldens :: [Text.Text]
textGoldens = [Text.empty, Text.singleton '\n', Text.pack "dražen popović"]

maybeGoldens :: [Maybe Bool]
maybeGoldens = [Nothing, Just True, Just False]

eitherGoldens :: [Either Bool Text.Text]
eitherGoldens = [Left True, Left False, Right (Text.pack "this is right")]

listGoldens :: [[Bool]]
listGoldens = [[], [True], [False], [True, True, False, False]]

setGoldens :: [Set.Set Bool]
setGoldens = [Set.empty, Set.singleton True, Set.fromList [True, False]]

mapGoldens :: [Map.Map Bool Bool]
mapGoldens = [Map.empty, Map.singleton True True, Map.fromList [(True, True), (False, False)]]
