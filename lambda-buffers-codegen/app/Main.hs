module Main (main) where

import Control.Applicative (optional, (<**>))
import LambdaBuffers.Codegen.Cli.Gen (GenOpts (GenOpts))
import LambdaBuffers.Codegen.Cli.GenHaskell qualified as Haskell
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
  value,
 )

newtype Command
  = GenHaskell Haskell.GenOpts

genOptsP :: Parser GenOpts
genOptsP =
  GenOpts
    <$> strOption
      ( long "input-file"
          <> short 'i'
          <> metavar "FILEPATH"
          <> help "Compiled LambdaBuffers schema to code generate for"
      )
    <*> strOption
      ( long "output-file"
          <> short 'o'
          <> metavar "FILEPATH"
          <> value "codegen-output.textproto"
          <> help "Codegen output"
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
              <> value ".lbg"
              <> showDefault
          )
      )

haskellGenOptsP :: Parser Haskell.GenOpts
haskellGenOptsP =
  Haskell.MkGenOpts
    <$> genOptsP
    <*> strOption
      ( long "config"
          <> short 'c'
          <> metavar "FILEPATH"
          <> help "Configuration file for the Haskell codegen module"
      )

optionsP :: Parser Command
optionsP =
  subparser $
    command
      "gen-haskell"
      (info (GenHaskell <$> haskellGenOptsP <* helper) (progDesc "Generate Haskell code from a compiled LambdaBuffers schema"))

parserInfo :: ParserInfo Command
parserInfo = info (optionsP <**> helper) (fullDesc <> progDesc "LambdaBuffers Codegen command-line interface tool")

main :: IO ()
main = do
  cmd <- customExecParser (prefs (showHelpOnEmpty <> showHelpOnError)) parserInfo
  case cmd of
    GenHaskell opts -> Haskell.gen opts
