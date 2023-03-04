module Test.LambdaBuffers.Frontend (tests) where

import Test.Tasty (TestTree, testGroup)

import Data.Map qualified as Map
import Data.Set qualified as Set
import LambdaBuffers.Frontend (FrontendError, FrontendResult (FrontendResult), runFrontend)
import LambdaBuffers.Frontend.Parsec ()
import Prettyprinter (Pretty (pretty))
import System.FilePath ((</>))
import Test.Tasty.HUnit (Assertion, assertFailure, testCase, (@?=))

tests :: FilePath -> TestTree
tests extraSrcFiles =
  testGroup
    "LambdaBuffers.Frontend"
    [ frontendErrorTests extraSrcFiles
    , frontendSuccessTests extraSrcFiles
    ]

-- FIXME(bladyjoker): Seems like all the SourceInfo positions are off by one.
frontendErrorTests :: FilePath -> TestTree
frontendErrorTests extraSrcFiles =
  testGroup
    "Frontend error tests"
    [ testCase "Duplicate type definition" $ do
        let workDir = extraSrcFiles </> "duplicate_tydef"
            fileIn = workDir </> "A.lbf"
            fileErr = fileIn
        errOrMod <- runFrontend [workDir] fileIn
        assertError (fileErr <> ":(5:1)-(5:12) Duplicate type definition with the name A") errOrMod
    , testCase "Import cycle found" $ do
        let workDir = extraSrcFiles </> "import_cycle_found"
            fileIn = workDir </> "A.lbf"
            fileErr = workDir </> "C.lbf"
        errOrMod <- runFrontend [workDir] fileIn
        assertError (fileErr <> ":(3:1)-(3:9) Tried to load module A which constitutes a cycle [B, A, ]") errOrMod
    , testCase "Imported symbol not found" $ do
        let workDir = extraSrcFiles </> "imported_not_found"
            fileIn = workDir </> "A.lbf"
            fileErr = fileIn
        errOrMod <- runFrontend [workDir] fileIn
        assertError (fileErr <> ":(3:17)-(3:18) Type C not found in module B, did you mean one of [A, B]") errOrMod
    , testCase "Invalid module filepath" $ do
        let workDir = extraSrcFiles </> "invalid_module_filepath"
            fileIn = workDir </> "A.lbf"
            fileErr = fileIn
        errOrMod <- runFrontend [workDir] fileIn
        assertError (fileErr <> ":(1:8)-(1:13) File name " <> extraSrcFiles <> "/invalid_module_filepath/A.lbf doesn't match module name A.B.C expected A/B/C.lbf") errOrMod
    , testCase "Module not found" $ do
        let workDir = extraSrcFiles </> "module_not_found"
            fileIn = workDir </> "A.lbf"
            fileErr = fileIn
        errOrMod <- runFrontend [workDir] fileIn
        assertError (fileErr <> ":(3:1)-(3:9) Module B not found in available import paths [" <> extraSrcFiles <> "/module_not_found]") errOrMod
    , testCase "Module parse error" $ do
        let workDir = extraSrcFiles </> "module_parse_error"
            fileIn = workDir </> "A.lbf"
            fileErr = fileIn
        errOrMod <- runFrontend [workDir] fileIn
        assertError (fileErr <> ":(3:1):\nunexpected 't'\nexpecting lb new line, import statement, type definition, space or end of input") errOrMod
    , testCase "Multiple modules found" $ do
        let workDir = extraSrcFiles </> "multiple_modules_found"
            fileIn = workDir </> "A.lbf"
            fileErr = fileIn
        errOrMod <- runFrontend [workDir, workDir </> "another_import_path"] fileIn
        assertError (fileErr <> ":(3:1)-(3:9) Module B found in multiple files [" <> extraSrcFiles <> "/multiple_modules_found/B.lbf, " <> extraSrcFiles <> "/multiple_modules_found/another_import_path/B.lbf]") errOrMod
    , testCase "Symbol already imported" $ do
        let workDir = extraSrcFiles </> "symbol_already_imported"
            fileIn = workDir </> "A.lbf"
            fileErr = fileIn
        errOrMod <- runFrontend [workDir] fileIn
        assertError (fileErr <> ":(4:1)-(4:9) Symbol A already imported from module B") errOrMod
    , testCase "Type definition name conflict" $ do
        let workDir = extraSrcFiles </> "tydef_name_conflict"
            fileIn = workDir </> "A.lbf"
            fileErr = fileIn
        errOrMod <- runFrontend [workDir] fileIn
        assertError (fileErr <> ":(5:1)-(5:12) Type name A conflicts with an imported type name from module B") errOrMod
    , testCase "Type reference not found" $ do
        let workDir = extraSrcFiles </> "tyref_not_found"
            fileIn = workDir </> "A.lbf"
            fileErr = fileIn
        errOrMod <- runFrontend [workDir] fileIn
        assertError (fileErr <> ":(6:13)-(6:28) Type WhereIsThisType not found in the module's scope [A, B, C, B.B, C.C]") errOrMod
    ]

assertError :: String -> Either FrontendError FrontendResult -> Assertion
assertError expected (Left frErr) = expected @?= show frErr
assertError _ (Right _) = assertFailure "Expected to fail but succeeded"

assertSuccess :: [String] -> Either FrontendError FrontendResult -> Assertion
assertSuccess _ (Left err) = assertFailure $ "Expected to succeed but failed with: " <> show err
assertSuccess expected (Right (FrontendResult mods)) = Set.fromList expected @?= Set.fromList (show . pretty <$> Map.keys mods)

frontendSuccessTests :: FilePath -> TestTree
frontendSuccessTests extraSrcFiles =
  testGroup
    "Frontend success tests"
    [ testCase "Good" $ do
        let workDir = extraSrcFiles </> "good"
            fileIn = workDir </> "Test.lbf"
        errOrMod <- runFrontend [workDir] fileIn
        assertSuccess ["A", "A.B", "B", "C", "Test"] errOrMod
    , testGroup
        "Formatting" -- TODO(bladyjoker): Add Equality check on compiled inputs (Set semantics on imports, ty defs etc)
        [ testCase "BadFormat.lbf compiles" $ do
            let workDir = extraSrcFiles </> "formatting"
                fileIn = workDir </> "BadFormat.lbf"
            errOrMod <- runFrontend [workDir] fileIn
            assertSuccess ["A", "BadFormat"] errOrMod
        , testCase "good/BadFormat.lbf also compiles" $ do
            let workDir = extraSrcFiles </> "formatting" </> "good"
                fileIn = workDir </> "BadFormat.lbf"
            errOrMod' <- runFrontend [workDir] fileIn
            assertSuccess ["A", "BadFormat"] errOrMod'
        ]
    ]
