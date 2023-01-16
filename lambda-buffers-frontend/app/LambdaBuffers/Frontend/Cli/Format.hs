module LambdaBuffers.Frontend.Cli.Format (FormatOpts (..), format) where

import Control.Lens (makeLenses, (^.))
import Data.Text.IO qualified as Text
import LambdaBuffers.Frontend.PPrint ()
import LambdaBuffers.Frontend.Parsec qualified as Parsec
import Prettyprinter (Pretty (pretty))
import Proto.Compiler ()

data FormatOpts = FormatOpts
  { _moduleFilepath :: FilePath
  , _inPlace :: Bool
  }
  deriving stock (Eq, Show)

makeLenses ''FormatOpts

format :: FormatOpts -> IO ()
format opts = do
  modContent <- Text.readFile (opts ^. moduleFilepath)
  modOrErr <- Parsec.runParser Parsec.parseModule (opts ^. moduleFilepath) modContent
  case modOrErr of
    Left err -> print err
    Right m -> do
      let formatted = show . pretty $ m
      if opts ^. inPlace
        then Prelude.writeFile (opts ^. moduleFilepath) formatted
        else Prelude.putStrLn formatted
