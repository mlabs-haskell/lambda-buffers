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
  strArgument,
  strOption,
  subparser,
 )
import Options.Applicative.NonEmpty (some1)

data Command
  = Build BuildOpts
  | Format FormatOpts

importPathP :: Parser FilePath
importPathP =
  strOption
    ( long "import-path"
        <> short 'i'
        <> metavar "FILEPATH"
        <> help "Directory to look for LambdaBuffer schemas (.lbf)"
    )

buildOptsP :: Parser BuildOpts
buildOptsP =
  BuildOpts
    <$> many importPathP
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
    <*> strOption
      ( long "gen-dir"
          <> metavar "FILEPATH"
          <> help "Directory where the Codegen will store any output produced from the LambdaBuffers schema"
      )
    <*> many
      ( strOption
          ( long "gen-class"
              <> metavar "CLASS"
              <> help "Class to code generate implementations for (if empty only the type definitions will be printed)"
              <> showDefault
          )
      )
    <*> many
      ( strOption
          ( long "gen-opt"
              <> metavar "ARGUMENT"
              <> help "Additional options to provide to the Codegen module"
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
              <> help "Working directory used to communicate with the Compiler and Codegen components"
              <> showDefault
          )
      )
    <*> some1 (strArgument (metavar "[.lbf SCHEMA]..." <> help ".lbf schemas to build"))

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

commandP :: Parser Command
commandP =
  subparser $
    command
      "build"
      (info (Build <$> buildOptsP <* helper) (progDesc "Build LambdaBuffers .lbf schemas"))
      <> command
        "format"
        (info (Format <$> formatOptsP <* helper) (progDesc "Format a LambdaBuffers Module (.lbf)"))

parserInfo :: ParserInfo Command
parserInfo = info (commandP <**> helper) (fullDesc <> progDesc "LambdaBuffers Frontend command-line interface tool")

main :: IO ()
main = do
  cmd <- customExecParser (prefs (showHelpOnEmpty <> showHelpOnError)) parserInfo
  case cmd of
    Build opts -> build opts
    Format opts -> format opts
