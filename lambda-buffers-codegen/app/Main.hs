module Main (main) where

import Control.Applicative (optional, (<**>))
import LambdaBuffers.Codegen.Cli.Gen (GenOpts (GenOpts), gen)
import Options.Applicative (
  Parser,
  ParserInfo,
  command,
  customExecParser,
  flag,
  fullDesc,
  help,
  helper,
  info,
  long,
  metavar,
  prefs,
  progDesc,
  short,
  showDefault,
  showHelpOnEmpty,
  showHelpOnError,
  strOption,
  subparser,
 )

newtype Command
  = Gen GenOpts

genOptsP :: Parser GenOpts
genOptsP =
  GenOpts
    <$> strOption
      ( long "file"
          <> short 'f'
          <> metavar "FILEPATH"
          <> help "Compiled LambdaBuffers schema to compile"
      )
    <*> flag
      False
      True
      ( long "debug"
          <> short 'd'
          <> help "Run in debug mode"
          <> showDefault
      )
    <*> optional
      ( strOption
          ( long "work-dir"
              <> short 'w'
              <> metavar "FILEPATH"
              <> help "Working directory used to communicate with the Codegen"
              <> showDefault
          )
      )

optionsP :: Parser Command
optionsP =
  subparser $
    command
      "gen"
      (info (Gen <$> genOptsP <* helper) (progDesc "Generate code from a compiled LambdaBuffers schema"))

parserInfo :: ParserInfo Command
parserInfo = info (optionsP <**> helper) (fullDesc <> progDesc "LambdaBuffers Codegen command-line interface tool")

main :: IO ()
main = do
  cmd <- customExecParser (prefs (showHelpOnEmpty <> showHelpOnError)) parserInfo
  case cmd of
    Gen opts -> gen opts
