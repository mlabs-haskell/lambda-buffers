module LambdaBuffers.Codegen.Cli.GenHaskell (GenOpts (..), gen) where

import Control.Lens (makeLenses, view, (&), (.~), (^.))
import Data.Aeson (decodeFileStrict)
import Data.Foldable (for_)
import Data.Map qualified as Map
import Data.ProtoLens (Message (defMessage))
import LambdaBuffers.Codegen.Cli.Gen qualified as Gen
import LambdaBuffers.Codegen.Haskell (runPrint)
import LambdaBuffers.Codegen.Haskell.Config qualified as H
import Proto.Compiler_Fields qualified as P

data GenOpts = MkGenOpts
  { _common :: Gen.GenOpts
  , _config :: FilePath
  }

makeLenses 'MkGenOpts

gen :: GenOpts -> IO ()
gen opts = do
  cfg <- readHaskellConfig (opts ^. config)
  Gen.gen
    (opts ^. common)
    ( \ci ->
        let errOrDocs = runPrint cfg <$> ci ^. #modules
            (errs, docs) =
              foldr
                ( \(mn, errOrDoc) (errs', docs') ->
                    either
                      (\err -> (Map.insert mn err errs', docs'))
                      (\doc -> (errs', Map.insert mn doc docs'))
                      errOrDoc
                )
                (mempty, mempty)
                (Map.toList errOrDocs)
         in if null errs
              then do
                for_ docs print
                return $ Right defMessage
              else return $ Left $ defMessage & P.internalErrors .~ concat (view P.internalErrors <$> errs)
    )

readHaskellConfig :: FilePath -> IO H.Config
readHaskellConfig f = do
  mayCfg <- decodeFileStrict f
  case mayCfg of
    Nothing -> error $ "Invalid Haskell configuration file " <> f
    Just cfg -> return cfg
