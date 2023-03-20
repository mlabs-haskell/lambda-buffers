module Test.LambdaBuffers.Codegen.Haskell (tests) where

import Control.Lens ((^.))
import Data.Aeson qualified as A
import Data.ByteString qualified as BS
import Data.Foldable (for_)
import Data.ProtoLens.Encoding qualified as Pb
import Data.ProtoLens.TextFormat qualified as PbText
import Data.Text.IO qualified as Text
import Data.Text.Lazy.IO qualified as LText
import LambdaBuffers.Codegen.Haskell qualified as H
import LambdaBuffers.Codegen.Haskell.Config qualified as H
import LambdaBuffers.Compiler.ProtoCompat.FromProto qualified as PC
import Paths_lambda_buffers_codegen qualified as Paths
import Proto.Compiler qualified as P
import System.FilePath ((<.>), (</>))
import System.FilePath.Lens (extension)
import Test.Tasty (TestName, TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase)

tests :: TestTree
tests =
  testGroup
    "LambdaBuffers.Codegen.Haskell"
    [ prints "lambda-buffers.input.textproto"
    ]

prints :: TestName -> TestTree
prints goldensFp = testCase goldensFp $ do
  configFp <- Paths.getDataFileName "data/haskell.json"
  cfg <- readHaskellConfig configFp
  ciFp <- Paths.getDataFileName ("data/goldens" </> goldensFp)
  ci <- readCodegenInput ciFp
  ci' <- case PC.runFromProto ci of
    Left err -> assertFailure (show err)
    Right res -> return res
  for_
    (ci' ^. #modules)
    ( \m -> case H.runPrint cfg ci' m of
        Left err -> assertFailure (show err)
        Right (fp', printed) -> do
          fp <- Paths.getDataFileName ("data/goldens/autogen" </> fp')
          golden <- Text.readFile fp
          if golden == printed
            then return ()
            else do
              let otherFp = fp <.> "other"
              -- TODO(bladyjoker): Use temp.
              Text.writeFile otherFp printed
              assertFailure $
                "Printed differs from the golden at "
                  <> fp
                  <> "\nCheck out the diff with "
                  <> otherFp
    )

readCodegenInput :: FilePath -> IO P.CompilerInput
readCodegenInput fp = do
  let ext = fp ^. extension
  case ext of
    ".pb" -> do
      content <- BS.readFile fp
      return $ Pb.decodeMessageOrDie content
    ".textproto" -> do
      content <- LText.readFile fp
      return $ PbText.readMessageOrDie content
    _ -> error $ "Unknown CompilerInput format " <> ext

readHaskellConfig :: FilePath -> IO H.Config
readHaskellConfig f = do
  mayCfg <- A.decodeFileStrict f
  case mayCfg of
    Nothing -> error $ "Invalid Haskell configuration file " <> f
    Just cfg -> return cfg
