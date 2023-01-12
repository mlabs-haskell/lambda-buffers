module Main (main) where

import Control.Applicative (Alternative (many), (<**>))

import LambdaBuffers.Frontend.Cli.Compile (CompileOpts (CompileOpts), compile)
import Options.Applicative (
  Parser,
  ParserInfo,
  command,
  customExecParser,
  fullDesc,
  help,
  helper,
  info,
  long,
  metavar,
  prefs,
  progDesc,
  showHelpOnEmpty,
  showHelpOnError,
  strOption,
  subparser,
 )

newtype Command
  = Compile CompileOpts

importPathP :: Parser FilePath
importPathP =
  strOption
    ( long "import-path"
        <> metavar "FILEPATH"
        <> help "Directory to look for LambdaBuffer Module source files (.lbf)"
    )

compileOptsP :: Parser CompileOpts
compileOptsP = CompileOpts <$> many importPathP

optionsP :: Parser Command
optionsP =
  subparser $
    command
      "compile"
      (info (Compile <$> compileOptsP <* helper) (progDesc "Compile a LambdaBuffers Module (.lbf)"))

parserInfo :: ParserInfo Command
parserInfo = info (optionsP <**> helper) (fullDesc <> progDesc "LambdaBuffers Frontend command-line interface tool")

main :: IO ()
main = do
  cmd <- customExecParser (prefs (showHelpOnEmpty <> showHelpOnError)) parserInfo
  case cmd of
    Compile opts -> compile opts
