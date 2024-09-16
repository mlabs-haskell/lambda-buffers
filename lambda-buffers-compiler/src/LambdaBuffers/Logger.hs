{- | Module: LambdaBuffers.Logger

Utilities for logging INFO, WARNING, and ERROR messages.

This follows the GNU error message format:
<https://www.gnu.org/prep/standards/html_node/Errors.html>
-}
module LambdaBuffers.Logger (logInfo, logError, logErrorWithSourceSpan) where

{- | Logs an INFO message of the format
> <file>: info: <msg>
if @<file>@ is non empty, otherwise, it prints
> info: <msg>
-}
logInfo :: FilePath -> String -> IO ()
logInfo "" msg = putStrLn $ "info: " <> msg
logInfo fp msg = putStrLn $ fp <> ": info: " <> msg

{- | Logs an ERROR message of the format
> <file>: error: <msg>
if @<file>@ is non empty, otherwise, it prints
> error: <msg>
-}
logError :: FilePath -> String -> IO ()
logError "" msg = putStrLn $ "error: " <> msg
logError fp msg = putStrLn $ fp <> ": error: " <> msg

{- | Logs an ERROR message of the format
> <file>:<line1>.<column1>-<line2>.<column2>: error: <msg>
or
> <file>:<line1>.<column1>: error: <msg>
if <line1>, <line2> are the same and <column1>, <column2> are the same.
-}
logErrorWithSourceSpan :: FilePath -> (Int, Int) -> (Int, Int) -> String -> IO ()
logErrorWithSourceSpan fp pos1 pos2 msg =
  putStrLn $ fp <> ":" <> showLineCol pos1 <> (if pos1 == pos2 then "" else "-" <> showLineCol pos2) <> ": " <> "error: " <> msg
  where
    showLineCol :: (Int, Int) -> String
    showLineCol (l, c) = show l <> "." <> show c
