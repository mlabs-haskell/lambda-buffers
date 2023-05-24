module Test.LambdaBuffers.Codegen.Purescript (tests) where

import Control.Lens ((^.))
import Data.Aeson qualified as A
import Data.ByteString qualified as BS
import Data.Foldable (for_)
import Data.ProtoLens.Encoding qualified as Pb
import Data.ProtoLens.TextFormat qualified as PbText
import Data.Text.IO qualified as Text
import Data.Text.Lazy.IO qualified as LText
import LambdaBuffers.Codegen.Purescript qualified as Purs
import LambdaBuffers.Codegen.Purescript.Config qualified as Purs
import LambdaBuffers.ProtoCompat qualified as PC
import Paths_lambda_buffers_codegen qualified as Paths
import Proto.Codegen qualified as P
import System.FilePath ((<.>), (</>))
import System.FilePath.Lens (extension)
import Test.Tasty (TestName, TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase)

tests :: TestTree
tests =
  testGroup
    "LambdaBuffers.Codegen.Purescript"
    [ prints "lambda-buffers.input.textproto"
    ]

prints :: TestName -> TestTree
prints goldensFp = testCase goldensFp $ do
  configFp <- Paths.getDataFileName "data/purescript.json"
  cfg <- readPurescriptConfig configFp
  ciFp <- Paths.getDataFileName ("data/goldens" </> goldensFp)
  ci <- readCodegenInput ciFp
  ci' <- case PC.codegenInputFromProto ci of
    Left err -> assertFailure (show err)
    Right res -> return res
  for_
    (ci' ^. #modules)
    ( \m -> case Purs.runPrint cfg ci' m of
        Left err -> assertFailure (show err)
        Right (fp', printed) -> do
          fp <- Paths.getDataFileName ("data/goldens/purescript-autogen" </> fp')
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

readCodegenInput :: FilePath -> IO P.Input
readCodegenInput fp = do
  let ext = fp ^. extension
  case ext of
    ".pb" -> do
      content <- BS.readFile fp
      return $ Pb.decodeMessageOrDie content
    ".textproto" -> do
      content <- LText.readFile fp
      return $ PbText.readMessageOrDie content
    _ -> error $ "Unknown Codegen Input format " <> ext

readPurescriptConfig :: FilePath -> IO Purs.Config
readPurescriptConfig f = do
  mayCfg <- A.decodeFileStrict f
  case mayCfg of
    Nothing -> error $ "Invalid Purescript configuration file " <> f
    Just cfg -> return cfg
