module Test.LambdaBuffers.Compiler.Utils.Golden (fails, succeeds) where

import Data.Foldable (for_)
import Data.Map (Map)
import Data.Map qualified as Map
import Paths_lambda_buffers_compiler qualified as Path
import System.FilePath ((<.>), (</>))
import Test.Tasty (TestName, TestTree)
import Test.Tasty.HUnit (assertFailure, testCase)

fails :: Eq t => FilePath -> (FilePath -> IO (t, FilePath)) -> (FilePath -> t -> IO ()) -> TestName -> Either t b -> TestTree
fails goldensDir readF writeF title runTest = testCase title $ do
  case runTest of
    Left gotErr -> do
      goldensDir' <- Path.getDataFileName goldensDir
      let testDir = goldensDir' </> title
      (wantedErr, wantedFp) <- readF testDir
      if wantedErr == gotErr
        then return ()
        else do
          _ <- writeF (wantedFp <.> "other") gotErr
          assertFailure $ "Must match with golden error in " <> wantedFp <> "\nCheck the diff with " <> (wantedFp <.> "other")
    Right _ -> assertFailure "Wanted an error but got a success"

-- TODO(bladyjoker): Document all this.
succeeds ::
  (Show err, Eq t) =>
  FilePath ->
  (FilePath -> IO (t, FilePath)) ->
  (FilePath -> t -> IO ()) ->
  TestName ->
  Either err (Map (Maybe FilePath) t) ->
  TestTree
succeeds goldensDir readF writeF title runTest = testCase title $ do
  case runTest of
    Left err -> assertFailure $ "Wanted a success but got error\n" <> show err
    Right wanteds -> do
      goldensDir' <- Path.getDataFileName goldensDir
      for_
        (Map.toList wanteds)
        ( \(mayBaseName, got) -> do
            let testDir = goldensDir' </> title
            (wanted, wantedFp) <- case mayBaseName of
              Nothing -> readF testDir
              Just baseName -> readF (testDir </> baseName)
            if wanted == got
              then return ()
              else do
                _ <- writeF (wantedFp <.> "other") got
                assertFailure $ "Must match with golden success in " <> wantedFp <> "\nCheck the diff with " <> (wantedFp <.> "other")
        )
