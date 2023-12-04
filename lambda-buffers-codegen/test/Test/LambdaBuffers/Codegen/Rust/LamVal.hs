module Test.LambdaBuffers.Codegen.Rust.LamVal (tests) where

import Control.Lens (makeLenses, (^.))
import Control.Monad (unless)
import Data.Aeson (decodeFileStrict')
import Data.Maybe (fromJust)
import LambdaBuffers.Codegen.Check (runCheck)
import LambdaBuffers.Codegen.LamVal qualified as LV
import LambdaBuffers.Codegen.LamVal.MonadPrint qualified as MP
import LambdaBuffers.Codegen.Print qualified as Print
import LambdaBuffers.Codegen.Rust (runPrint)
import LambdaBuffers.Codegen.Rust qualified as Rust
import LambdaBuffers.Codegen.Rust.Config qualified as Config
import LambdaBuffers.Codegen.Rust.Config qualified as R
import LambdaBuffers.Codegen.Rust.Print qualified as RsPrint
import LambdaBuffers.Codegen.Rust.Print.Derive qualified as RsDerive
import LambdaBuffers.Codegen.Rust.Print.LamVal qualified as R
import LambdaBuffers.Codegen.Rust.Print.MonadPrint (MonadPrint)
import LambdaBuffers.Codegen.Rust.Print.Syntax qualified as RsSyntax
import LambdaBuffers.Codegen.Rust.Print.TyDef qualified as RsPrint
import LambdaBuffers.ProtoCompat qualified as PC
import Paths_lambda_buffers_codegen qualified as Paths
import Prettyprinter (defaultLayoutOptions, layoutPretty)
import Prettyprinter.Render.Text (renderStrict)
import Proto.Codegen qualified as P
import System.Directory (doesFileExist)
import System.Directory.Internal.Prelude (exitFailure)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

-- import Control.Monad.RWS (RWST (runRWST))

tests :: TestTree
tests =
  testGroup
    "LambdaBuffers.Codegen.Rust.LamVal"
    [ text
    , printFieldE
    ]

text :: TestTree
text = testCase "Print Text value" $ do
  let val = LV.TextE "testVal"
      result = Print.runPrint mempty (RsPrint.printModule rsPrintModuleEnv)
  result @?= result

-- data Context qtn qcn = Context
--   { _ctxCompilerInput :: PC.CodegenInput
--   , _ctxModule :: PC.Module -- TODO(bladyjoker): Turn into a `ModuleName` and do a lookup on the CI.
--   , _ctxTyImports :: Set PC.QTyName
--   , _ctxOpaqueTyImports :: Set qtn
--   , _ctxTraitImports :: Set qcn
--   , _ctxRuleImports :: Set (PC.InfoLess PC.ModuleName)
--   , _ctxTyExports :: Set (PC.InfoLess PC.TyName)
--   , _ctxConfig :: Config qtn qcn
--   }

-- -- | `runPrint ctx printer` runs a printing workflow that yields a module document and a set of package dependencies.
-- runPrint ::
--   forall qtn qcn qvn.
--   Context () () ->
--   PrintM () () () (Doc (), Set Text) ->
--   Either P.Error (Doc (), Set Text)
-- runPrint ctx modPrinter =
--   let p = runRWST modPrinter ctx (State mempty mempty mempty)
--    in case runExcept p of
--         Left err ->
--           Left $
--             defMessage
--               & P.internalErrors
--                 .~ [ err
--                    ]
--         Right (r, _, _) -> Right r

-- data ValueE where
--   LamE :: (ValueE -> ValueE) -> ValueE
--   AppE :: ValueE -> ValueE -> ValueE
--   RefE :: Ref -> ValueE
--   VarE :: String -> ValueE
--   -- | Sum expressions
--   CaseE :: QSum -> ValueE -> ((Ctor, [ValueE]) -> ValueE) -> ValueE
--   CtorE :: QCtor -> [ValueE] -> ValueE
--   -- | Record expressions
--   RecordE :: QRecord -> [(Field, ValueE)] -> ValueE
--   FieldE :: QField -> ValueE -> ValueE
--   -- | Product expressions
--   ProductE :: QProduct -> [ValueE] -> ValueE
--   LetE :: QProduct -> ValueE -> ([ValueE] -> ValueE) -> ValueE
--   -- | Int expressions
--   IntE :: Int -> ValueE
--   -- caseInt :: Int -> [(Int, a)] -> (Int -> a) -> a
--   CaseIntE :: ValueE -> [(ValueE, ValueE)] -> (ValueE -> ValueE) -> ValueE
--   -- | Text expressions
--   TextE :: Text -> ValueE
--   -- caseText :: Text -> [(Text, a)] -> (Text -> a) -> a
--   CaseTextE :: ValueE -> [(ValueE, ValueE)] -> (ValueE -> ValueE) -> ValueE
--   -- | List expressions
--   ListE :: [ValueE] -> ValueE
--   CaseListE :: ValueE -> [(Int, [ValueE] -> ValueE)] -> (ValueE -> ValueE) -> ValueE
--   -- | Tuple expressions
--   TupleE :: ValueE -> ValueE -> ValueE
--   -- | Error
--   ErrorE :: String -> ValueE

printFieldE :: TestTree
printFieldE = testCase "Print field accessor" $ do
  -- let tyName = "Test"
  --     fieldName = "testField"
  --     qField = (tyName, fieldName)
  --     val = "testVal"
  -- LamVal.prientFieldE qField val
  1 @?= 1

runPrint = do
  cfg <- readRustConfig =<< Paths.getDataFileName "data/rust-prelude-base.json"

  case runCheck cfg mempty m of
    Left err -> Left err
    Right ctx -> case Print.runPrint ctx (RsPrint.printModule rsPrintModuleEnv) of
      Left err -> Left err
      Right (modDoc, deps) ->
        Right
          ( RsSyntax.filepathFromModuleName (PC.ModuleName "Test")
          , renderStrict $ layoutPretty defaultLayoutOptions modDoc
          , deps
          )

readRustConfig :: FilePath -> IO Config.Config
readRustConfig f = do
  fExists <- doesFileExist f
  mayCfg <- decodeFileStrict' f
  return $ fromJust cfg

rsPrintModuleEnv :: MonadPrint m => RsPrint.PrintModuleEnv m ann
rsPrintModuleEnv =
  RsPrint.PrintModuleEnv
    RsSyntax.printModName
    RsDerive.rsTraitImplPrinters
    RsPrint.printTyDef
    ["no_implicit_prelude"]
