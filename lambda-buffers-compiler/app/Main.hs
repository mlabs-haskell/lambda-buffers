module Main (main) where

import Control.Applicative ((<**>))

import LambdaBuffers.Compiler.Cli.Compile (CompileOpts (CompileOpts), compile)
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
  short,
  showHelpOnEmpty,
  showHelpOnError,
  strOption,
  subparser,
 )

newtype Command = Compile CompileOpts

inputPathP :: Parser FilePath
inputPathP =
  strOption
    ( long "input-file"
        <> short 'i'
        <> metavar "FILEPATH"
        <> help "File to compile (lambdabuffers.compiler.CompilerInput in .textproto format)"
    )

outputPathP :: Parser FilePath
outputPathP =
  strOption
    ( long "output-file"
        <> short 'o'
        <> metavar "FILEPATH"
        <> help "File to write the output to (lambdabuffers.compiler.CompilerOutput in .textproto format)"
    )

compileOptsP :: Parser CompileOpts
compileOptsP = CompileOpts <$> inputPathP <*> outputPathP

optionsP :: Parser Command
optionsP =
  subparser $
    command
      "compile"
      (info (Compile <$> compileOptsP <* helper) (progDesc "Compile LambdaBuffers"))

parserInfo :: ParserInfo Command
parserInfo = info (optionsP <**> helper) (fullDesc <> progDesc "LambdaBuffers Compiler command-line interface tool")

main :: IO ()
main = do
  cmd <- customExecParser (prefs (showHelpOnEmpty <> showHelpOnError)) parserInfo
  case cmd of
    Compile opts -> compile opts
