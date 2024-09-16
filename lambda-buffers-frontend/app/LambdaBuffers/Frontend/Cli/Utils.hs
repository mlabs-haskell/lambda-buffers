module LambdaBuffers.Frontend.Cli.Utils (logFrontendError, logCompilerError, logCodegenError, toCodegenCliModuleName, checkExists, FileOrDir (..)) where

import Data.List (intercalate)
import Data.Text qualified as Text
import LambdaBuffers.Frontend (FrontendError)
import LambdaBuffers.Frontend.Errors.Frontend qualified as Errors.Frontend
import LambdaBuffers.Frontend.PPrint ()
import LambdaBuffers.Frontend.Syntax qualified as Frontend
import LambdaBuffers.Logger (logError, logErrorWithSourceSpan)
import Prettyprinter (Doc)
import Prettyprinter qualified
import Prettyprinter.Render.String qualified
import System.Directory (doesPathExist)
import System.Exit (exitFailure)

logFrontendError :: FrontendError -> IO ()
logFrontendError err =
  let
    (maybeModuleName, sourceInfo) = Errors.Frontend.getFrontendModuleAndSourceInfoContext err
    msg = Errors.Frontend.prettyFrontendErrorMessage err
    filename = Text.unpack $ Frontend.filename sourceInfo
    startPos = (Frontend.row $ Frontend.from sourceInfo, Frontend.column $ Frontend.from sourceInfo)
    endPos = (Frontend.row $ Frontend.to sourceInfo, Frontend.column $ Frontend.to sourceInfo)
   in
    logErrorWithSourceSpan filename startPos endPos $
      showOneLine $
        case maybeModuleName of
          Just moduleName ->
            Prettyprinter.brackets
              ("module" Prettyprinter.<+> Prettyprinter.pretty moduleName)
              Prettyprinter.<+> msg
          Nothing -> msg

showOneLine :: Doc a -> String
showOneLine d = (Prettyprinter.Render.String.renderShowS . Prettyprinter.layoutPretty (Prettyprinter.defaultLayoutOptions {Prettyprinter.layoutPageWidth = Prettyprinter.Unbounded}) $ d) ""

logCompilerError :: FilePath -> String -> IO ()
logCompilerError = logError

logCodegenError :: FilePath -> String -> IO ()
logCodegenError = logError

-- NOTE(bladyjoker): Consider using the proto to supply requested modules.
toCodegenCliModuleName :: Frontend.ModuleName () -> String
toCodegenCliModuleName (Frontend.ModuleName parts _) = intercalate "." ((\(Frontend.ModuleNamePart p _) -> Text.unpack p) <$> parts)

data FileOrDir = File | Dir deriving stock (Eq)

checkExists :: FileOrDir -> String -> FilePath -> IO ()
checkExists fileOrDir purpose path = do
  exists <- doesPathExist path
  if exists
    then return ()
    else do
      logError path $
        "the provided "
          <> purpose
          <> " "
          <> ( if fileOrDir == File
                then "file"
                else "directory"
             )
          <> " "
          <> path
          <> " doesn't exist"
      exitFailure
