module Test.LambdaBuffers.Prelude.Golden.Utils
  ( assertGoldens
  , Spec
  , (<+>)
  , appendSpace
  ) where

import Prelude
import Data.Array (length, zip, (..))
import Data.Either (Either)
import Data.Foldable (foldM, for_)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (Aff, Error)
import Node.Encoding (Encoding(..))
import Node.FS.Sync as NodeFS
import Node.Path (FilePath)
import Test.Spec (SpecT, describe)
import Test.Spec.Assertions (fail)

readFile :: String -> Effect String
readFile path = NodeFS.readTextFile UTF8 path

appendSpace :: String -> String -> String
appendSpace l r = l <> " " <> r

infixr 5 appendSpace as <+>

findGoldens :: FilePath -> String -> String -> Int -> Effect (Map Int (Tuple FilePath String))
findGoldens goldenDir ext title goldensLength =
  foldM
    ( \found ix -> do
        let
          goldenFp = goldenDir <> "/" <> title <> "." <> show ix <> ext
        goldenExists <- NodeFS.exists goldenFp
        if goldenExists then do
          goldenText <- readFile goldenFp
          pure $ Map.insert ix (Tuple goldenFp goldenText) found
        else
          pure found
    )
    Map.empty
    (0 .. (goldensLength - 1))

type Spec a
  = SpecT Aff Unit (Either Error) a

-- | `assertGoldens goldenDir title ext assert goldens`
assertGoldens :: forall a. FilePath -> String -> String -> (String -> String) -> (a -> Int -> FilePath -> String -> Spec Unit) -> Array a -> Effect (Spec Unit)
assertGoldens goldenDir title ext propTitle assert goldens = do
  goldens' <- findGoldens goldenDir ext title (length goldens)
  when (goldens' == Map.empty)
    $ fail ("Expected to find some goldens" <+> title <+> ext <+> " Did you forget to (re)generate goldens?" <+> goldenDir)
  let
    (tests' :: Spec Unit) =
      for_
        (zip goldens $ 0 .. (length goldens - 1))
        ( \(Tuple golden ix) ->
            describe (show ix)
              ( case Map.lookup ix goldens' of
                  Nothing -> fail $ "Golden value index not in goldens" <+> title <+> show ix
                  Just (Tuple fp text) -> assert golden ix fp text
              )
        )
  pure
    $ describe
        ("forall (golden :" <+> title <> ".*" <> ext <+> ")" <> ":" <+> propTitle "golden")
        tests'
