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
  option,
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
import Options.Applicative.Builder (auto)

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

debugP :: Parser Bool
debugP =
  option
    auto
    ( long "debug"
        <> short 'd'
        <> metavar "DEBUG"
        <> help "Run everything in debug mode"
        <> value False
        <> showDefault
    )

compileOptsP :: Parser CompileOpts
compileOptsP = CompileOpts <$> inputPathP <*> outputPathP <*> debugP

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
