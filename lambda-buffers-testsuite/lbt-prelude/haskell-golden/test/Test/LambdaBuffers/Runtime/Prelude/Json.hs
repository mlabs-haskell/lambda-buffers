module Test.LambdaBuffers.Runtime.Prelude.Json (tests) where

import Data.ByteString qualified as B
import Data.ByteString.Lazy qualified as BL
import Data.Foldable (for_)
import Data.List (isPrefixOf)
import Data.Map qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as Text
import Data.Traversable (for)
import Hedgehog qualified as H
import Hedgehog.Gen qualified as H
import LambdaBuffers.Days (Day (Day'Friday, Day'Monday, Day'Saturday, Day'Sunday, Day'Thursday, Day'Tuesday, Day'Wednesday), WeekDay (WeekDay), WorkDay (WorkDay))
import LambdaBuffers.Foo.Bar (FooComplicated (FooComplicated), FooProd (FooProd), FooRec (FooRec), FooSum (FooSum'Bar, FooSum'Baz, FooSum'Faz, FooSum'Foo, FooSum'Qax))
import LambdaBuffers.Runtime.Prelude (Json, fromJsonBytes, toJsonBytes)
import LambdaBuffers.Runtime.Prelude.Generators.Correct qualified as Lbr
import Paths_haskell_golden qualified as Paths
import System.Directory (removeFile)
import System.FilePath (takeBaseName, (</>))
import Test.LambdaBuffers.Runtime.Prelude.Generators.Correct qualified as Correct
import Test.Tasty (TestName, TestTree, adjustOption, testGroup)
import Test.Tasty.Golden (findByExtension, goldenVsString)
import Test.Tasty.HUnit (assertFailure)
import Test.Tasty.Hedgehog (testProperty)
import Test.Tasty.Hedgehog qualified as H

tests :: IO TestTree
tests = do
  golden <- goldenTests
  return $
    testGroup
      "lbf-prelude.Prelude.Json class derivation tests"
      [golden, hedgehogTests]

hedgehogTests :: TestTree
hedgehogTests =
  adjustOption (\_ -> H.HedgehogTestLimit $ Just 1000) $
    testGroup
      "Property tests"
      [ fooSumFromTo
      , fooProdFromTo
      , fooRecFromTo
      , fooComplicatedFromTo
      , dayFromTo
      , workDayFromTo
      , weekDayFromTo
      ]

goldenTests :: IO TestTree
goldenTests = do
  gts <-
    id
      `traverse` [ dayFromToGolden
                 , workDayFromToGolden
                 , weekDayFromToGolden
                 , fooSumFromToGolden Lbr.genBool Lbr.genBytes Lbr.genInteger
                 , fooProdFromToGolden Lbr.genBool Lbr.genBytes Lbr.genInteger
                 , fooRecFromToGolden Lbr.genBool Lbr.genBytes Lbr.genInteger
                 , fooComplicatedFromToGolden Lbr.genBool Lbr.genBytes Lbr.genInteger
                 , boolFromToGolden
                 , integerFromToGolden
                 , bytesFromToGolden
                 , charFromToGolden
                 , textFromToGolden
                 , maybeFromToGolden
                 , eitherFromToGolden
                 , listFromToGolden
                 , setFromToGolden
                 , mapFromToGolden
                 ]

  return $
    testGroup
      "Golden tests"
      gts

fromToTest :: forall {a}. (Show a, Eq a, Json a) => TestName -> H.Gen a -> TestTree
fromToTest title gen =
  testProperty
    (title <> ": (fromJson . toJson) x == x")
    ( H.property $ do
        x <- H.forAll gen
        (fromJsonBytes . toJsonBytes) x H.=== Right x
    )

fooSumFromTo :: TestTree
fooSumFromTo =
  fromToTest
    "Foo.Bar.FooSum"
    (Correct.genFooSum Lbr.genInteger Lbr.genBool Lbr.genBytes)

fooProdFromTo :: TestTree
fooProdFromTo =
  fromToTest
    "Foo.Bar.FooProd"
    (Correct.genFooProd Lbr.genInteger Lbr.genBool Lbr.genBytes)

fooRecFromTo :: TestTree
fooRecFromTo =
  fromToTest
    "Foo.Bar.FooRec"
    (Correct.genFooRec Lbr.genInteger Lbr.genBool Lbr.genBytes)

fooComplicatedFromTo :: TestTree
fooComplicatedFromTo =
  fromToTest
    "Foo.Bar.FooComplicated"
    (Correct.genFooComplicated Lbr.genInteger Lbr.genBool Lbr.genBytes)

dayFromTo :: TestTree
dayFromTo =
  fromToTest
    "Days.Day"
    Correct.genDay

workDayFromTo :: TestTree
workDayFromTo =
  fromToTest
    "Days.WorkDay"
    Correct.genWorkDay

weekDayFromTo :: TestTree
weekDayFromTo =
  fromToTest
    "Days.WeekDay"
    Correct.genWeekDay

-- | `fromToGoldenTest title gens`
fromToGoldenTest :: forall {a}. (Json a) => TestName -> [H.Gen a] -> IO TestTree
fromToGoldenTest title gens =
  do
    goldenDir <- Paths.getDataFileName "../golden"
    jsonFpsFound <- filter (\fp -> title `isPrefixOf` takeBaseName fp) <$> findByExtension [".json"] goldenDir
    jsonFps <-
      if length jsonFpsFound /= length gens
        then do
          for_ jsonFpsFound removeFile
          for (zip [0 :: Integer ..] gens) $ \(ix, gen) -> do
            x <- H.sample gen
            let
              xJson = toJsonBytes x
              jsonFp = goldenDir </> title <> "." <> show ix <> ".json"
            B.writeFile jsonFp xJson
            return jsonFp
        else return jsonFpsFound
    tests' <- for jsonFps $ \jsonFp ->
      do
        let index = takeBaseName jsonFp
        json <- B.readFile jsonFp
        case fromJsonBytes @a json of
          Left err -> assertFailure $ "Failed parsing " <> jsonFp <> " " <> show err
          Right res -> return $ goldenVsString index jsonFp (return . BL.fromStrict . toJsonBytes $ res)
    return $
      testGroup
        (title <> ": (toJson . fromJson) golden == golden")
        tests'

fooSumGens :: H.Gen a -> H.Gen b -> H.Gen c -> [H.Gen (FooSum a b c)]
fooSumGens genx geny genz =
  [ FooSum'Foo <$> genx <*> geny <*> genz
  , FooSum'Bar <$> genx <*> geny
  , FooSum'Baz <$> geny
  , return FooSum'Qax
  , FooSum'Faz <$> Lbr.genInteger
  ]

fooSumFromToGolden :: (Json a, Json b, Json c) => H.Gen a -> H.Gen b -> H.Gen c -> IO TestTree
fooSumFromToGolden genx geny genz =
  fromToGoldenTest
    "Foo.Bar.FooSum"
    (fooSumGens genx geny genz)

fooProdGens :: H.Gen a -> H.Gen b -> H.Gen c -> [H.Gen (FooProd a b c)]
fooProdGens genx geny genz =
  [FooProd <$> genx <*> geny <*> genz <*> Lbr.genInteger]

fooProdFromToGolden :: (Json a, Json b, Json c) => H.Gen a -> H.Gen b -> H.Gen c -> IO TestTree
fooProdFromToGolden genx geny genz =
  fromToGoldenTest
    "Foo.Bar.FooProd"
    (fooProdGens genx geny genz)

fooRecGens :: H.Gen a -> H.Gen b -> H.Gen c -> [H.Gen (FooRec a b c)]
fooRecGens genx geny genz =
  [FooRec <$> genx <*> geny <*> genz <*> Lbr.genInteger]

fooRecFromToGolden :: (Json a, Json b, Json c) => H.Gen a -> H.Gen b -> H.Gen c -> IO TestTree
fooRecFromToGolden genx geny genz =
  fromToGoldenTest
    "Foo.Bar.FooRec"
    (fooRecGens genx geny genz)

fooComplicatedFromToGolden :: (Json a, Json b, Json c) => H.Gen a -> H.Gen b -> H.Gen c -> IO TestTree
fooComplicatedFromToGolden genx geny genz =
  fromToGoldenTest
    "Foo.Bar.FooComplicated"
    ( do
        fooSumGen <- fooSumGens genx geny genz
        fooProdGen <- fooProdGens genx geny genz
        fooRecGen <- fooRecGens genx geny genz
        return (FooComplicated <$> fooSumGen <*> fooProdGen <*> fooRecGen)
    )

dayFromToGolden :: IO TestTree
dayFromToGolden = fromToGoldenTest "Days.Day" (return <$> [Day'Monday, Day'Tuesday, Day'Wednesday, Day'Thursday, Day'Friday, Day'Saturday, Day'Sunday])

workDayFromToGolden :: IO TestTree
workDayFromToGolden = fromToGoldenTest "Days.WorkDay" (return . WorkDay <$> [Day'Monday, Day'Tuesday, Day'Wednesday, Day'Thursday, Day'Friday])

weekDayFromToGolden :: IO TestTree
weekDayFromToGolden = fromToGoldenTest "Days.WeekDay" (return . WeekDay <$> [Day'Saturday, Day'Sunday])

boolFromToGolden :: IO TestTree
boolFromToGolden = fromToGoldenTest "Prelude.Bool" (return <$> [True, False])

integerFromToGolden :: IO TestTree
integerFromToGolden =
  fromToGoldenTest
    "Prelude.Integer"
    [ return 0
    , return 1
    , return (-1)
    , return $ 2 ^ (32 :: Integer)
    , return $ -(2 ^ (32 :: Integer))
    , return $ 2 ^ (64 :: Integer)
    , return $ -(2 ^ (64 :: Integer))
    , return $ 2 ^ (128 :: Integer)
    , return $ -(2 ^ (128 :: Integer))
    , return $ 2 ^ (256 :: Integer)
    , return $ -(2 ^ (256 :: Integer))
    , Lbr.genInteger
    ]

bytesFromToGolden :: IO TestTree
bytesFromToGolden = fromToGoldenTest "Prelude.Bytes" [return B.empty, return $ B.singleton 0, Lbr.genBytes]

charFromToGolden :: IO TestTree
charFromToGolden = fromToGoldenTest "Prelude.Char" [return '\0', return '\n', Lbr.genChar]

textFromToGolden :: IO TestTree
textFromToGolden = fromToGoldenTest "Prelude.Text" [return Text.empty, return $ Text.singleton 'x', return $ Text.pack "dražen popović", Lbr.genText]

maybeFromToGolden :: IO TestTree
maybeFromToGolden = fromToGoldenTest "Prelude.Maybe" [return Nothing, return $ Just True, return $ Just False]

eitherFromToGolden :: IO TestTree
eitherFromToGolden = fromToGoldenTest "Prelude.Either" [return $ Left True, return $ Right (Text.pack "this is right"), Lbr.genEither Lbr.genBool Lbr.genText]

listFromToGolden :: IO TestTree
listFromToGolden = fromToGoldenTest "Prelude.List" [return [], return [True], Lbr.genList Lbr.genBool]

setFromToGolden :: IO TestTree
setFromToGolden = fromToGoldenTest "Prelude.Set" [return Set.empty, return $ Set.singleton True, return $ Set.fromList [True, False]]

mapFromToGolden :: IO TestTree
mapFromToGolden = fromToGoldenTest "Prelude.Map" [return Map.empty, return $ Map.singleton True True, return $ Map.fromList [(True, True), (False, False)]]
