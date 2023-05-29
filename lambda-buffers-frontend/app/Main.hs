module Main (main) where

import Control.Applicative (Alternative (many), optional, (<**>))
import LambdaBuffers.Frontend.Cli.Build (BuildOpts (BuildOpts), build)
import LambdaBuffers.Frontend.Cli.Env qualified as Env
import LambdaBuffers.Frontend.Cli.Format (FormatOpts (FormatOpts), format)
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

data Command
  = Build BuildOpts
  | Format FormatOpts

importPathP :: Parser FilePath
importPathP =
  strOption
    ( long "import-path"
        <> short 'i'
        <> metavar "FILEPATH"
        <> help "Directory to look for LambdaBuffer Module source files (.lbf)"
    )

buildOptsP :: Parser BuildOpts
buildOptsP =
  BuildOpts
    <$> many importPathP
    <*> strOption
      ( long "file"
          <> short 'f'
          <> metavar "FILEPATH"
          <> help "LambdaBuffers file (.lbf) to build"
      )
    <*> optional
      ( strOption
          ( long "compiler"
              <> short 'c'
              <> metavar "FILEPATH"
              <> help ("LambdaBuffers Compiler location (if none is set the " <> Env.lbcVar <> " environment variable is used)")
          )
      )
    <*> optional
      ( strOption
          ( long "gen"
              <> short 'g'
              <> metavar "FILEPATH"
              <> help ("LambdaBuffers Codegen location (if none is set the " <> Env.lbgVar <> " environment variable is used)")
          )
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
              <> help "Working directory used to communicate with the Compiler"
              <> showDefault
          )
      )

formatOptsP :: Parser FormatOpts
formatOptsP =
  FormatOpts
    <$> strOption
      ( long "file"
          <> short 'f'
          <> metavar "FILEPATH"
          <> help "LambdaBuffers file to format"
      )
    <*> flag
      False
      True
      ( long "inplace"
          <> short 'i'
          <> help "Replace the file content with the formatted version"
          <> showDefault
      )

optionsP :: Parser Command
optionsP =
  subparser $
    command
      "build"
      (info (Build <$> buildOptsP <* helper) (progDesc "Build a LambdaBuffers Module (.lbf)"))
      <> command
        "format"
        (info (Format <$> formatOptsP <* helper) (progDesc "Format a LambdaBuffers Module (.lbf)"))

parserInfo :: ParserInfo Command
parserInfo = info (optionsP <**> helper) (fullDesc <> progDesc "LambdaBuffers Frontend command-line interface tool")

main :: IO ()
main = do
  cmd <- customExecParser (prefs (showHelpOnEmpty <> showHelpOnError)) parserInfo
  case cmd of
    Build opts -> build opts
    Format opts -> format opts
