module Main (main) where

import Control.Applicative (Alternative (many), (<**>))
import GHC.IO.Encoding (setLocaleEncoding, utf8)
import LambdaBuffers.Codegen.Cli.Gen (GenOpts (GenOpts))
import LambdaBuffers.Codegen.Cli.GenHaskell qualified as Haskell
import LambdaBuffers.Codegen.Cli.GenPlutarch qualified as Plutarch
import LambdaBuffers.Codegen.Cli.GenPurescript qualified as Purescript
import LambdaBuffers.Codegen.Cli.GenTypescript qualified as Typescript
import Options.Applicative (
  InfoMod,
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
  value,
 )
import Options.Applicative.NonEmpty (some1)

data Command
  = GenHaskell Haskell.GenOpts
  | GenPurescript Purescript.GenOpts
  | GenTypescript Typescript.GenOpts
  | GenPlutarch Plutarch.GenOpts

genOptsP :: Parser GenOpts
genOptsP =
  GenOpts
    <$> strOption
      ( long "input"
          <> short 'i'
          <> metavar "FILEPATH"
          <> help "Codegen API input containing compiled LambdaBuffers schema checked by the Compiler"
      )
    <*> strOption
      ( long "output"
          <> short 'o'
          <> metavar "FILEPATH"
          <> value "codegen-output.textproto"
          <> help "Codegen API output that can be used to inspect Codegen errors"
      )
    <*> strOption
      ( long "gen-dir"
          <> short 'g'
          <> metavar "FILEPATH"
          <> help "Directory to write code generation output to"
      )
    <*> flag
      False
      True
      ( long "debug"
          <> short 'd'
          <> help "Run in debug mode"
          <> showDefault
      )
    <*> many
      ( strOption
          ( long "gen-class"
              <> metavar "CLASS"
              <> help "Class to code generate implementations for"
          )
      )
    <*> some1 (strArgument (metavar "[module]..." <> help "Modules to generate code for"))

haskellGenOptsP :: Parser Haskell.GenOpts
haskellGenOptsP =
  Haskell.MkGenOpts
    <$> many
      ( strOption
          ( long "config"
              <> short 'c'
              <> metavar "FILEPATH"
              <> help "Configuration file for the Haskell Codegen module (multiple `config`s are merged with left first merge conflict strategy)"
          )
      )
    <*> genOptsP

purescriptGenOptsP :: Parser Purescript.GenOpts
purescriptGenOptsP =
  Purescript.MkGenOpts
    <$> many
      ( strOption
          ( long "config"
              <> short 'c'
              <> metavar "FILEPATH"
              <> help "Configuration file for the Purescript Codegen module (multiple `config`s are merged with left first merge conflict strategy)"
          )
      )
    <*> genOptsP

typescriptGenOptsP :: Parser Typescript.GenOpts
typescriptGenOptsP =
  Typescript.MkGenOpts
    <$> many
      ( strOption
          ( long "config"
              <> short 'c'
              <> metavar "FILEPATH"
              <> help "Configuration file for the Typescript Codegen module (multiple `config`s are merged with left first merge conflict strategy)"
          )
      )
    <*> genOptsP

plutarchGenOptsP :: Parser Plutarch.GenOpts
plutarchGenOptsP =
  Plutarch.MkGenOpts
    <$> many
      ( strOption
          ( long "config"
              <> short 'c'
              <> metavar "FILEPATH"
              <> help "Configuration file for the Plutarch Codegen module (multiple `config`s are merged with left first merge conflict strategy)"
          )
      )
    <*> genOptsP

mkProgDesc :: forall {a}. String -> InfoMod a
mkProgDesc backend =
  progDesc $
    "Generate "
      <> backend
      <> " code for `modules` given a checked LambdaBuffers schema in `input` and configuration in `config`. "
      <> "Outputs the generated code in `gen-dir` directory and provides any errors encountered in the `output` file."

commandP :: Parser Command
commandP =
  subparser $
    command
      "gen-haskell"
      ( info
          (GenHaskell <$> (helper *> haskellGenOptsP))
          (mkProgDesc "Haskell")
      )
      <> command
        "gen-purescript"
        ( info
            (GenPurescript <$> (helper *> purescriptGenOptsP))
            (mkProgDesc "Purescript")
        )
      <> command
        "gen-typescript"
        ( info
            (GenTypescript <$> (helper *> typescriptGenOptsP))
            (mkProgDesc "Typescript")
        )
      <> command
        "gen-plutarch"
        ( info
            (GenPlutarch <$> (helper *> plutarchGenOptsP))
            (mkProgDesc "Plutarch")
        )

parserInfo :: ParserInfo Command
parserInfo = info (commandP <**> helper) (fullDesc <> progDesc "LambdaBuffers Codegen command-line interface tool")

main :: IO ()
main = do
  setLocaleEncoding utf8
  cmd <- customExecParser (prefs (showHelpOnEmpty <> showHelpOnError)) parserInfo
  case cmd of
    GenHaskell opts -> Haskell.gen opts
    GenPurescript opts -> Purescript.gen opts
    GenTypescript opts -> Typescript.gen opts
    GenPlutarch opts -> Plutarch.gen opts
