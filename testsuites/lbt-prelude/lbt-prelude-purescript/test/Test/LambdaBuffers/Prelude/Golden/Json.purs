module Test.LambdaBuffers.Prelude.Golden.Json
  ( tests
  ) where

import Prelude
import Data.Either (Either(..))
import Data.Foldable (for_)
import Data.Traversable (for)
import Effect (Effect)
import LambdaBuffers.Runtime.Prelude (class Json, fromJsonString, toJsonString)
import Node.Path (FilePath)
import Test.LambdaBuffers.Prelude.Golden as Golden
import Test.LambdaBuffers.Prelude.Golden.Utils (Spec, assertGoldens, (<+>))
import Test.Spec (describe, it)
import Test.Spec.Assertions (fail, shouldEqual)

-- | `fromToGoldenTest goldenDir title goldens`
fromToGoldenTest' :: forall a. Show a => Eq a => Json a => FilePath -> String -> Array a -> Effect (Spec Unit)
fromToGoldenTest' goldenDir title =
  assertGoldens
    goldenDir
    title
    ".json"
    (\x -> "(toJson . fromJson)" <+> x <+> "==" <+> x)
    ( \golden _index _fp text -> case fromJsonString text of
        Left err -> it "Golden file should parse as Json" $ fail (show err)
        Right res -> do
          it "Golden values should match" $ shouldEqual golden res
          it "Golden files should match"
            $ shouldEqual text (toJsonString res)
    )

fromToGoldenTest :: forall a. Json a => Eq a => Show a => String -> Array a -> Effect (Spec Unit)
fromToGoldenTest title goldens = fromToGoldenTest' "../golden" title goldens

tests :: Effect (Spec Unit)
tests = do
  goldenInstance <- goldenInstanceTests
  pure
    $ describe
        "Prelude.Json class tests" do
        describe "Instance" do
          goldenInstance

goldenInstanceTests :: Effect (Spec Unit)
goldenInstanceTests = do
  gts <- for preludeFromToGoldenTests (\t -> t)
  pure
    $ describe
        "Golden tests"
        (for_ gts (\t -> t))

-- | Prelude
preludeFromToGoldenTests :: Array (Effect (Spec Unit))
preludeFromToGoldenTests =
  [ fromToGoldenTest "Prelude.Bool" Golden.boolGoldens
  , fromToGoldenTest "Prelude.Char" Golden.charGoldens
  , fromToGoldenTest "Prelude.Integer" Golden.integerGoldens
  , fromToGoldenTest "Prelude.Text" Golden.textGoldens
  , fromToGoldenTest "Prelude.Bytes" Golden.bytesGoldens
  , fromToGoldenTest "Prelude.Maybe" Golden.maybeGoldens
  , fromToGoldenTest "Prelude.Either" Golden.eitherGoldens
  , fromToGoldenTest "Prelude.List" Golden.listGoldens
  , fromToGoldenTest "Prelude.Map" Golden.mapGoldens
  ]
