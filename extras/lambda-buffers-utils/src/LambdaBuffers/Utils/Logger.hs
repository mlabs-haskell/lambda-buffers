{- | Module: LambdaBuffers.Utils.Logger

Utilities for logging INFO, and ERROR messages.

This follows the GNU error message format:
<https://www.gnu.org/prep/standards/html_node/Errors.html>
-}
module LambdaBuffers.Utils.Logger (logInfo, logError, logErrorWithSourceSpan) where

import System.IO qualified

{- | Logs an INFO message of the format
> <file>: info: <msg>
if @<file>@ is non empty, otherwise, it prints
> info: <msg>
-}
logInfo :: FilePath -> String -> IO ()
logInfo "" msg = System.IO.putStrLn $ "info: " <> msg
logInfo fp msg = System.IO.putStrLn $ fp <> ": info: " <> msg

{- | Logs an ERROR message of the format
> <file>: error: <msg>
if @<file>@ is non empty, otherwise, it prints
> error: <msg>
-}
logError :: FilePath -> String -> IO ()
logError "" msg = System.IO.hPutStrLn System.IO.stderr $ "error: " <> msg
logError fp msg = System.IO.hPutStrLn System.IO.stderr $ fp <> ": error: " <> msg

{- | Logs an ERROR message of the format
> <file>:<line1>.<column1>-<line2>.<column2>: error: <msg>
or
> <file>:<line1>.<column1>: error: <msg>
if <line1>, <line2> are the same and <column1>, <column2> are the same.
-}
logErrorWithSourceSpan :: FilePath -> (Int, Int) -> (Int, Int) -> String -> IO ()
logErrorWithSourceSpan fp pos1 pos2 msg =
  System.IO.hPutStrLn System.IO.stderr $ fp <> ":" <> showLineCol pos1 <> (if pos1 == pos2 then "" else "-" <> showLineCol pos2) <> ": " <> "error: " <> msg
  where
    showLineCol :: (Int, Int) -> String
    showLineCol (l, c) = show l <> "." <> show c
