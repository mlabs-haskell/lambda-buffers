{-# OPTIONS_GHC -Wno-missing-local-signatures #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}

module Test.LambdaBuffers.Codegen.Plutarch (tests) where

import Control.Lens ((^.))
import Data.Aeson qualified as A
import Data.Char qualified as Char
import Data.Default (Default (def))
import Data.Foldable (Foldable (toList))
import Data.Functor (void)
import Data.Map qualified as Map
import Data.Map.Ordered qualified as OMap
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import LambdaBuffers.Codegen.Haskell.Config qualified as H
import LambdaBuffers.Codegen.Haskell.Print.Syntax qualified as HsSyntax
import LambdaBuffers.Codegen.LamVal qualified as LamVal
import LambdaBuffers.Codegen.LamVal.MonadPrint qualified as LamVal
import LambdaBuffers.Codegen.Plutarch.Print.LamVal qualified as PlLamVal
import LambdaBuffers.Compiler.LamTy qualified as LT
import LambdaBuffers.ProtoCompat qualified as PC
import Paths_lambda_buffers_codegen qualified as Path
import Paths_lambda_buffers_codegen qualified as Paths
import Proto.Codegen_Fields qualified as P
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@=?))

-- TODO(bladyjoker): Implement this test for all backends.
-- TODO(bladyjoker): Figure out records here.

tests :: TestTree
tests =
  testGroup
    "LambdaBuffers.Codegen.Plutarch"
    [ configParses
    , testLamValInterpretation
    ]

configParses :: TestTree
configParses = testCase "Haskell config parses" $ do
  preludeBaseConfigFp <- Paths.getDataFileName "data/plutarch-prelude.json"
  void $ readHaskellConfig preludeBaseConfigFp
  plutusTxConfigFp <- Paths.getDataFileName "data/plutarch-plutus.json"
  void $ readHaskellConfig plutusTxConfigFp

readHaskellConfig :: FilePath -> IO H.Config
readHaskellConfig f = do
  mayCfg <- A.decodeFileStrict f
  case mayCfg of
    Nothing -> error $ "Invalid Haskell configuration file " <> f
    Just cfg -> return cfg

lamValCases :: [(String, LamVal.ValueE, Either String String)]
lamValCases =
  [ ("1", LamVal.IntE 1, Right "lamval-cases/plutarch/IntE-1.hs")
  , ("-1", LamVal.IntE (-1), Right "lamval-cases/plutarch/IntE-2.hs")
  ,
    ( "case int of 1 -> 1; -1 -> -1; other -> other"
    , LamVal.CaseIntE
        (LamVal.VarE "int")
        [ (LamVal.IntE 1, LamVal.IntE 1)
        , (LamVal.IntE (-1), LamVal.IntE (-1))
        ]
        id
    , Right "lamval-cases/plutarch/CaseIntE-1.hs"
    )
  , ("some text", LamVal.TextE "some text", Right "lamval-cases/plutarch/TextE-1.hs")
  ,
    ( "case txt of \"a\" -> \"a it is\"; \"b\" -> \"b it is\"; other -> other"
    , LamVal.CaseTextE
        (LamVal.VarE "txt")
        [ (LamVal.TextE "a", LamVal.TextE "a it is")
        , (LamVal.TextE "b", LamVal.TextE "b it is")
        ]
        id
    , Right "lamval-cases/plutarch/CaseTextE-1.hs"
    )
  , ("[]", LamVal.ListE [], Right "lamval-cases/plutarch/ListE-1.hs")
  , ("[1, 2, a]", LamVal.ListE [LamVal.IntE 1, LamVal.IntE 2, LamVal.VarE "a"], Right "lamval-cases/plutarch/ListE-2.hs")
  ,
    ( "case xs of [] -> []; [a, b] -> [a, b]; [a, b, c, d] -> [a, b, c, d]; other -> other"
    , LamVal.CaseListE
        (LamVal.VarE "xs")
        [ (0, const $ LamVal.ListE [])
        , (2, LamVal.ListE)
        , (4, LamVal.ListE)
        ]
        id
    , Right "lamval-cases/plutarch/CaseListE-1.hs"
    )
  , ("(x, y)", LamVal.TupleE (LamVal.VarE "x") (LamVal.VarE "y"), Left "[LambdaBuffers.Codegen.Plutarch.Print.LamVal] LamVal tuple literal expression is not supported for Plutarch (yet)")
  , ("x", LamVal.VarE "x", Right "lamval-cases/plutarch/VarE-1.hs")
  , ("fooRef", LamVal.RefE ([], "fooRef"), Right "lamval-cases/plutarch/RefE-1.hs")
  , ("\\x -> x", LamVal.LamE id, Right "lamval-cases/plutarch/LamE-1.hs")
  , ("\\x -> (\\y -> y)", LamVal.LamE (\_argVal -> LamVal.LamE id), Right "lamval-cases/plutarch/LamE-2.hs")
  , ("f x", LamVal.AppE (LamVal.VarE "f") (LamVal.VarE "x"), Right "lamval-cases/plutarch/AppE-1.hs")
  ,
    ( "(f x) (g y)"
    , LamVal.AppE
        (LamVal.AppE (LamVal.VarE "f") (LamVal.VarE "x"))
        (LamVal.AppE (LamVal.VarE "g") (LamVal.VarE "y"))
    , Right "lamval-cases/plutarch/AppE-2.hs"
    )
  , ("UnitProduct x", LamVal.ProductE lbUnitProduct [LamVal.VarE "x"], Right "lamval-cases/plutarch/ProductE-1.hs")
  ,
    ( "let unitProduct (\\(UnitProduct msg) -> UnitProduct msg)"
    , LamVal.LetE lbUnitProduct (LamVal.VarE "unitProduct") (LamVal.ProductE lbUnitProduct)
    , Right "lamval-cases/plutarch/LetE-1.hs"
    )
  ,
    ( "FooProduct x 1 (UnitProduct \"works\")"
    , LamVal.ProductE
        lbFooProduct
        [ LamVal.VarE "x"
        , LamVal.IntE 1
        , LamVal.ProductE lbUnitProduct [LamVal.TextE "works"]
        ]
    , Right "lamval-cases/plutarch/ProductE-2.hs"
    )
  ,
    ( "let fooProduct (\\(FooProduct x y z) -> FooProduct x y z)"
    , LamVal.LetE lbFooProduct (LamVal.VarE "fooProduct") (LamVal.ProductE lbFooProduct)
    , Right "lamval-cases/plutarch/LetE-2.hs"
    )
  ,
    ( "UnitRecord x"
    , LamVal.RecordE
        lbUnitRecord
        [
          (
            ( PC.mkInfoLess $ PC.FieldName "foo" def
            , LT.TyVar $ PC.TyVar (PC.VarName "a" def)
            )
          , LamVal.VarE "x"
          )
        ]
    , Left "[LambdaBuffers.Codegen.Plutarch.Print.LamVal] LamVal record literal expression is not supported for Plutarch"
    )
  ,
    ( "unitRecord.foo"
    , LamVal.FieldE
        ((PC.mkInfoLess (PC.ModuleName [] def), PC.mkInfoLess (PC.TyName "UnitProduct" def)), PC.mkInfoLess $ PC.FieldName "foo" def)
        (LamVal.VarE "unitRecord")
    , Left "[LambdaBuffers.Codegen.Plutarch.Print.LamVal] LamVal record field accessor is not supported for Plutarch"
    )
  ,
    ( "Foo'Bar \"works\")"
    , LamVal.CtorE
        lbBarCtor
        [ LamVal.TextE "works"
        ]
    , Right "lamval-cases/plutarch/CtorE-1.hs"
    )
  ,
    ( "case fooSum of Bar x -> Bar x; Baz x y z -> Baz x y z"
    , LamVal.CaseE
        lbFooSum
        (LamVal.VarE "fooSum")
        (\(ctor, args) -> LamVal.CtorE ((PC.mkInfoLess (PC.ModuleName [] def), PC.mkInfoLess (PC.TyName "FooSum" def)), ctor) args)
    , Right "lamval-cases/plutarch/CaseE-1.hs"
    )
  ,
    ( "error \"some error\"'"
    , LamVal.ErrorE "some error"
    , Left "[LambdaBuffers.Codegen.Plutarch.Print.LamVal] LamVal error builtin was called with: some error"
    )
  ]

testLamValInterpretation :: TestTree
testLamValInterpretation =
  let
    interpret =
      LamVal.runPrint
        (LamVal.MkPrintRead $ \(_ty, refName) -> Map.lookup refName $ Map.singleton "fooRef" (HsSyntax.MkCabalPackageName "foo-pkg", HsSyntax.MkModuleName "Foo", HsSyntax.MkValueName "fooRef"))
        . PlLamVal.printValueE
    tcs :: [TestTree]
    tcs =
      fmap
        ( \(label, valE, expectation) ->
            let tc = testCase label
                interpreted = interpret valE
                assertion = case expectation of
                  Left wantErr -> case interpreted of
                    Left gotErr -> wantErr @=? Text.unpack (gotErr ^. P.msg)
                    Right (doc, imports) -> assertFailure $ show ("Wanted a failure", wantErr, "but got a success", toLamValCaseFile (show doc) imports)
                  Right gotLamValCaseFilepath -> do
                    dataDir <- Path.getDataFileName "data"
                    gotLamValCaseFile <- Text.readFile $ dataDir <> "/" <> gotLamValCaseFilepath
                    case interpreted of
                      Left gotErr -> assertFailure $ show ("Wanted a success", gotLamValCaseFilepath, gotLamValCaseFile, "but got a failure", gotErr ^. P.msg)
                      Right (doc, imports) -> gotLamValCaseFile @=? toLamValCaseFile (show doc) imports
             in tc assertion
        )
        lamValCases
   in
    testGroup "LamVal interpretation tests" tcs

groupByAndAgg :: Foldable f => Ord k => (a -> v) -> (a -> k) -> (v -> v -> v) -> f a -> Map.Map k v
groupByAndAgg intro key agg =
  Prelude.foldl
    (\m x -> Map.insertWith agg (key x) (intro x) m)
    Map.empty

toLamValCaseFile :: String -> Set HsSyntax.QValName -> Text
toLamValCaseFile doc imports =
  let
    groupedByPkgMod =
      groupByAndAgg
        (\(HsSyntax.MkCabalPackageName _pkg, HsSyntax.MkModuleName _m, HsSyntax.MkValueName v) -> Set.singleton v)
        (\(HsSyntax.MkCabalPackageName pkg, HsSyntax.MkModuleName m, HsSyntax.MkValueName _v) -> (pkg, m))
        Set.union
        imports
    importsTxt = Text.intercalate "\n" ["import " <> "\"" <> pkg <> "\" qualified " <> m <> " (" <> Text.intercalate ", " (valText <$> toList vals) <> ")" | ((pkg, m), vals) <- Map.toList groupedByPkgMod]
    valText val = case Text.uncons val of
      Just (v, _) -> if Char.isAlphaNum v then val else "(" <> val <> ")"
      Nothing -> "ERROR(bladyjoker): Should never happen"
   in
    Text.intercalate "\n\n" [importsTxt, Text.pack doc] <> "\n"

{- | Example unit product.

```lbf
prod UnitProduct a = a
```
-}
lbUnitProduct :: LamVal.QProduct
lbUnitProduct =
  ( (PC.mkInfoLess (PC.ModuleName [] def), PC.mkInfoLess (PC.TyName "UnitProduct" def))
  , LT.TyVar <$> [PC.TyVar (PC.VarName "a" def)]
  )

{- | Example product

```lbf
prod FooProduct a b c = a b c
```
-}
lbFooProduct :: LamVal.QProduct
lbFooProduct =
  ( (PC.mkInfoLess (PC.ModuleName [] def), PC.mkInfoLess (PC.TyName "FooProduct" def))
  , LT.TyVar <$> [PC.TyVar (PC.VarName "a" def), PC.TyVar (PC.VarName "b" def), PC.TyVar (PC.VarName "c" def)]
  )

{- | Example unit record.

```lbf
prod UnitRecord a = {foo: a}
```
-}
lbUnitRecord :: LamVal.QRecord
lbUnitRecord =
  ( (PC.mkInfoLess (PC.ModuleName [] def), PC.mkInfoLess (PC.TyName "UnitRecord" def))
  , OMap.fromList
      [
        ( PC.mkInfoLess $ PC.FieldName "foo" def
        , LT.TyVar $ PC.TyVar (PC.VarName "a" def)
        )
      ]
  )

{- | Example sum.
```lbf
sum FooSum a b c = Bar (UnitProduct a) | Baz a b c
```
-}
lbBarCtor :: LamVal.QCtor
lbBarCtor =
  ( (PC.mkInfoLess (PC.ModuleName [] def), PC.mkInfoLess (PC.TyName "FooSum" def))
  ,
    ( PC.mkInfoLess $ PC.ConstrName "Bar" def
    ,
      [ LT.TyApp
          (LT.TyRef $ PC.LocalI $ PC.LocalRef (PC.TyName "UnitProduct" def) def)
          [LT.TyVar $ PC.TyVar (PC.VarName "a" def)]
          Nothing
      ]
    )
  )

{- | Example sum.
```lbf
sum FooSum a b c = Bar (UnitProduct a) | Baz a b c
```
-}
lbFooSum :: LamVal.QSum
lbFooSum =
  ( (PC.mkInfoLess (PC.ModuleName [] def), PC.mkInfoLess (PC.TyName "FooSum" def))
  , OMap.fromList
      [
        ( PC.mkInfoLess $ PC.ConstrName "Bar" def
        , LT.TyProduct
            [ LT.TyApp
                (LT.TyRef $ PC.LocalI $ PC.LocalRef (PC.TyName "UnitProduct" def) def)
                [LT.TyVar $ PC.TyVar (PC.VarName "a" def)]
                Nothing
            ]
            (PC.Product [] def)
        )
      ,
        ( PC.mkInfoLess $ PC.ConstrName "Baz" def
        , LT.TyProduct
            [ LT.TyVar $ PC.TyVar (PC.VarName "a" def)
            , LT.TyVar $ PC.TyVar (PC.VarName "b" def)
            , LT.TyVar $ PC.TyVar (PC.VarName "c" def)
            ]
            (PC.Product [] def)
        )
      ]
  )
