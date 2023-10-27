module Main (main) where

import Control.Applicative ((<**>))
import Options.Applicative (
  Parser,
  ParserInfo,
  auto,
  command,
  customExecParser,
  fullDesc,
  help,
  helper,
  info,
  long,
  metavar,
  option,
  prefs,
  progDesc,
  showDefault,
  showHelpOnEmpty,
  showHelpOnError,
  strArgument,
  subparser,
  value,
 )
import Test.LambdaBuffers.Plutus.Cli.GenerateJson (GenerateJsonOpts (GenerateJsonOpts), generateJson)
import Test.LambdaBuffers.Plutus.Cli.GeneratePlutusData (GeneratePlutusDataOpts (GeneratePlutusDataOpts), generatePlutusData)

data Command
  = GenerateJson GenerateJsonOpts
  | GeneratePlutusData GeneratePlutusDataOpts
  deriving stock (Show, Eq, Ord)

genJsonOptsP :: Parser GenerateJsonOpts
genJsonOptsP =
  GenerateJsonOpts
    <$> option
      auto
      ( long "max-samples"
          <> metavar "SAMPLES"
          <> help "Number of maximum golden samples to generate per type"
          <> value 10
          <> showDefault
      )
    <*> strArgument (metavar "DIR" <> help "Directory to output golden Json samples to")

genPlutusDataOptsP :: Parser GeneratePlutusDataOpts
genPlutusDataOptsP =
  GeneratePlutusDataOpts
    <$> option
      auto
      ( long "max-samples"
          <> metavar "SAMPLES"
          <> help "Number of maximum golden samples to generate per type"
          <> value 10
          <> showDefault
      )
    <*> strArgument (metavar "DIR" <> help "Directory to output golden PlutusData samples to")

commandP :: Parser Command
commandP =
  subparser $
    command
      "generate-json"
      (info (GenerateJson <$> genJsonOptsP <* helper) (progDesc "Generate golden Json samples for `lbf-plutus`"))
      <> command
        "generate-plutusdata"
        (info (GeneratePlutusData <$> genPlutusDataOptsP <* helper) (progDesc "Generate golden PlutusData samples for `lbf-plutus`"))

parserInfo :: ParserInfo Command
parserInfo = info (commandP <**> helper) (fullDesc <> progDesc "LambdaBuffers `lbt-plutus` test suite command-line interface tool")

main :: IO ()
main = do
  cmd <- customExecParser (prefs (showHelpOnEmpty <> showHelpOnError)) parserInfo
  case cmd of
    GenerateJson opts -> generateJson opts
    GeneratePlutusData opts -> generatePlutusData opts
