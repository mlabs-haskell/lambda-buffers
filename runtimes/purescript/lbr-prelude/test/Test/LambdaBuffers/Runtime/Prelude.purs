module Test.LambdaBuffers.Runtime.Prelude.Json
  ( tests
  ) where

import Prelude
import Data.Either (Either(Right), hush)
import Data.Enum (toEnumWithDefaults)
import Data.Maybe (Maybe(..))
import Data.String (CodePoint)
import Data.TextDecoder (decodeUtf8)
import Data.TextEncoder (encodeUtf8)
import Effect.Class (liftEffect)
import LambdaBuffers.Runtime.Prelude (class Json, fromJsonString, toJsonString)
import LambdaBuffers.Runtime.Prelude.Bytes as Bytes
import LambdaBuffers.Runtime.Prelude.Generators.Correct as Correct
import LambdaBuffers.Runtime.Prelude.Json (fromJsonBytes, toJsonBytes)
import Test.QuickCheck (quickCheckGen, (===)) as Q
import Test.QuickCheck.Gen (Gen) as Q
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

tests :: Spec Unit
tests = do
  describe "lbf-prelude.Prelude.Json class" do
    describe "Instance" do
      fromToTest "Prelude.Integer" Correct.genInteger
      fromToTest "Prelude.Bool" Correct.genBool
      fromToTest "Prelude.Char" Correct.genChar
      fromToTest "Prelude.Bytes" Correct.genBytes
      fromToTest "Prelude.Text" Correct.genText
      fromToTest "Prelude.Maybe" Correct.genMaybe
      fromToTest "Prelude.Either" Correct.genEither
      fromToTest "Prelude.List" Correct.genList
      fromToTest "Prelude.Set" Correct.genSet
      fromToTest "Prelude.Map" Correct.genMap
  describe "UTF-8 encoding must be checked in the 21st century" do
    it "(encodeUtf8 . decodeUtf8) 'test' == 'test'"
      $ (encodeUtf8 >>> decodeUtf8 >>> hush) "test" `shouldEqual` Just "test"
    it "(encodeUtf8 . decodeUtf8) 'dražen popović' == 'dražen popović'"
      $ (encodeUtf8 >>> decodeUtf8 >>> hush) "dražen popović" `shouldEqual` Just "dražen popović"
    it "(encodeUtf8 . decodeUtf8) '膺텧௩Ꝏ긕鎓�쑯' == '膺텧௩Ꝏ긕鎓�쑯'"
      $ (encodeUtf8 >>> decodeUtf8 >>> hush) "膺텧௩Ꝏ긕鎓�쑯" `shouldEqual` Just "膺텧௩Ꝏ긕鎓�쑯"
    it "forall (x : String). (encodeUtf8 . decodeUtf8) x == x"
      $ do
          liftEffect
            $ Q.quickCheckGen do
                str <- Correct.genText
                pure $ (encodeUtf8 >>> decodeUtf8 >>> hush) str Q.=== Just str
  describe "Bytes" do
    it "forall (x : Bytes). x == x'"
      $ do
          liftEffect
            $ Q.quickCheckGen do
                bs <- Correct.genBytes
                pure $ bs Q.=== bs
    it "forall (x : Bytes). (fromIntArray . toIntArray) x == x'"
      $ do
          liftEffect
            $ Q.quickCheckGen do
                bs <- Correct.genBytes
                pure $ (Bytes.fromIntArray <<< Bytes.toIntArray) bs Q.=== bs
    it "toJson 'test' == '\"dGVzdA==\"'"
      $ toJsonString (Bytes.fromIntArray [ 116, 101, 115, 116 ]) `shouldEqual` "\"dGVzdA==\""
  let
    invertedSmiley = toEnumWithDefaults bottom top 0x1F643 :: CodePoint
  describe "Char" do
    it "fromJson \"🙃\" == '🙃'"
      $ fromJsonString "\"🙃\"" `shouldEqual` Right invertedSmiley
    it "toJson '🙃' == \"🙃\""
      $ toJsonString invertedSmiley `shouldEqual` "\"🙃\""

fromToTest :: forall a. Json a => Show a => Eq a => String -> Q.Gen a -> Spec Unit
fromToTest title gen =
  it ("forall (x: " <> title <> "): (fromJson . toJson) x == x") do
    liftEffect
      $ Q.quickCheckGen do
          x <- gen
          pure $ (toJsonBytes >>> fromJsonBytes) x Q.=== Right x
