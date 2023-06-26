module Main (main) where

import Control.Applicative ((<**>))
import LambdaBuffers.Prelude.Cli.Generate (GenerateOpts (GenerateOpts), generate)
import Options.Applicative (
  Parser,
  ParserInfo,
  command,
  customExecParser,
  fullDesc,
  help,
  helper,
  info,
  metavar,
  prefs,
  progDesc,
  showHelpOnEmpty,
  showHelpOnError,
  strArgument,
  subparser,
 )

newtype Command
  = Generate GenerateOpts
  deriving stock (Show, Eq, Ord)

genOptsP :: Parser GenerateOpts
genOptsP =
  GenerateOpts
    <$> strArgument (metavar "DIR" <> help "Directory to output golden samples to")

commandP :: Parser Command
commandP =
  subparser $
    command
      "generate"
      (info (Generate <$> genOptsP <* helper) (progDesc "Generate lbf-prelude golden samples"))

parserInfo :: ParserInfo Command
parserInfo = info (commandP <**> helper) (fullDesc <> progDesc "LambdaBuffers `lbf-prelude` test suite command-line interface tool")

main :: IO ()
main = do
  cmd <- customExecParser (prefs (showHelpOnEmpty <> showHelpOnError)) parserInfo
  case cmd of
    Generate opts -> generate opts
