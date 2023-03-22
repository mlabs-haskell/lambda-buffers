module Main (main) where

import Control.Applicative (optional, (<**>))
import LambdaBuffers.Codegen.Cli.Gen (GenOpts (GenOpts))
import LambdaBuffers.Codegen.Cli.GenHaskell qualified as Haskell
import LambdaBuffers.Codegen.Cli.GenPurescript qualified as Purescript
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

data Command
  = GenHaskell Haskell.GenOpts
  | GenPurescript Purescript.GenOpts

genOptsP :: Parser GenOpts
genOptsP =
  GenOpts
    <$> strOption
      ( long "input"
          <> short 'i'
          <> metavar "FILEPATH"
          <> help "Compiled LambdaBuffers schema to generate code for"
      )
    <*> strOption
      ( long "output"
          <> short 'o'
          <> metavar "FILEPATH"
          <> value "codegen-output.textproto"
          <> help "Codegen output that can be used to inspect Codegen errors"
      )
    <*> strOption
      ( long "print-dir"
          <> short 'p'
          <> metavar "FILEPATH"
          <> help "Directory to print code generation output to"
      )
    <*> flag
      False
      True
      ( long "debug"
          <> short 'd'
          <> help "Run in debug mode"
          <> showDefault
      )

haskellGenOptsP :: Parser Haskell.GenOpts
haskellGenOptsP =
  Haskell.MkGenOpts
    <$> genOptsP
    <*> optional
      ( strOption
          ( long "config"
              <> short 'c'
              <> metavar "FILEPATH"
              <> help "Configuration file for the Haskell codegen module"
          )
      )

purescriptGenOptsP :: Parser Purescript.GenOpts
purescriptGenOptsP =
  Purescript.MkGenOpts
    <$> genOptsP
    <*> optional
      ( strOption
          ( long "config"
              <> short 'c'
              <> metavar "FILEPATH"
              <> help "Configuration file for the Purescript codegen module"
          )
      )

optionsP :: Parser Command
optionsP =
  subparser $
    command
      "gen-haskell"
      (info (GenHaskell <$> haskellGenOptsP <* helper) (progDesc "Generate Haskell code from a compiled LambdaBuffers schema"))
      <> command
        "gen-purescript"
        (info (GenPurescript <$> purescriptGenOptsP <* helper) (progDesc "Generate Purescript code from a compiled LambdaBuffers schema"))

parserInfo :: ParserInfo Command
parserInfo = info (optionsP <**> helper) (fullDesc <> progDesc "LambdaBuffers Codegen command-line interface tool")

main :: IO ()
main = do
  cmd <- customExecParser (prefs (showHelpOnEmpty <> showHelpOnError)) parserInfo
  case cmd of
    GenHaskell opts -> Haskell.gen opts
    GenPurescript opts -> Purescript.gen opts
