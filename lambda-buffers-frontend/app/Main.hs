module Main (main) where

import Control.Applicative (Alternative (many), (<**>))

import LambdaBuffers.Frontend.Cli.Compile (CompileOpts (CompileOpts), compile)
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
  = Compile CompileOpts
  | Format FormatOpts

importPathP :: Parser FilePath
importPathP =
  strOption
    ( long "import-path"
        <> short 'i'
        <> metavar "FILEPATH"
        <> help "Directory to look for LambdaBuffer Module source files (.lbf)"
    )

compileOptsP :: Parser CompileOpts
compileOptsP =
  CompileOpts
    <$> many importPathP
    <*> strOption
      ( long "file"
          <> short 'f'
          <> metavar "FILEPATH"
          <> help "LambdaBuffers file to compile"
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
      "compile"
      (info (Compile <$> compileOptsP <* helper) (progDesc "Compile a LambdaBuffers Module (.lbf)"))
      <> command
        "format"
        (info (Format <$> formatOptsP <* helper) (progDesc "Format a LambdaBuffers Module (.lbf)"))

parserInfo :: ParserInfo Command
parserInfo = info (optionsP <**> helper) (fullDesc <> progDesc "LambdaBuffers Frontend command-line interface tool")

main :: IO ()
main = do
  cmd <- customExecParser (prefs (showHelpOnEmpty <> showHelpOnError)) parserInfo
  case cmd of
    Compile opts -> compile opts
    Format opts -> format opts