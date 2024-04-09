-- | lbf-prelude.Prelude.Json class implementations
module LambdaBuffers.Codegen.LamVal.Json (deriveToJsonImpl, deriveFromJsonImpl) where

import Control.Lens (view)
import Data.List (sortOn)
import Data.Map.Ordered qualified as OMap
import Data.Text (Text)
import Data.Text qualified as Text
import LambdaBuffers.Codegen.LamVal (QProduct, QRecord, QSum, ValueE (CaseE, CaseListE, CtorE, ErrorE, FieldE, LamE, LetE, ListE, ProductE, RecordE, RefE, TextE, TupleE), (@))
import LambdaBuffers.Codegen.LamVal.Derive (deriveImpl)
import LambdaBuffers.Compiler.LamTy qualified as LT
import LambdaBuffers.ProtoCompat qualified as PC
import Proto.Codegen qualified as P

-- | Domain functions

-- | `toJson :: a -> Json`
toJsonRef :: LT.Ty -> ValueE
toJsonRef ty = RefE ([ty], "toJson")

-- | `fromJson :: Json -> Parser a`
fromJsonRef :: LT.Ty -> ValueE
fromJsonRef ty = RefE ([ty], "fromJson")

-- | `bindParse :: Parser a -> (a -> Parser b) -> Parser b`
bindParseRef :: LT.Ty -> LT.Ty -> ValueE
bindParseRef tyIn tyOut = RefE ([tyIn, tyOut], "bindParse")

-- | `failParse :: Text -> Parser a`
failParseRef :: LT.Ty -> ValueE
failParseRef ty = RefE ([ty], "failParse")

-- | `succeedParse :: a -> Parser a`
succeedParseRef :: LT.Ty -> ValueE
succeedParseRef ty = RefE ([ty], "succeedParse")

-- | `jsonConstructor :: Text -> [Json] -> Json`
jsonConstructorRef :: ValueE
jsonConstructorRef = RefE ([], "jsonConstructor")

-- | `caseJsonConstructor :: Text -> [(Text, [Json] -> Parser a)] -> Json -> Parser a`
caseJsonConstructor :: LT.Ty -> ValueE
caseJsonConstructor ty = RefE ([ty], "caseJsonConstructor")

-- | `jsonArray :: [Json] -> Json`
jsonArrayRef :: ValueE
jsonArrayRef = RefE ([], "jsonArray")

-- | `caseJsonArray :: Text -> ([Json] -> Parser a) -> Json -> Parser a
caseJsonArrayRef :: LT.Ty -> ValueE
caseJsonArrayRef ty = RefE ([ty], "caseJsonArray")

-- | `jsonObject :: [(Text, Json)] -> Json`
jsonObjRef :: ValueE
jsonObjRef = RefE ([], "jsonObject")

-- | `caseJsonObject` :: (JsonObj -> Parser a) -> Json -> Parser a
caseJsonObjectRef :: LT.Ty -> ValueE
caseJsonObjectRef ty = RefE ([ty], "caseJsonObject")

-- | `jsonField` :: Text -> JsonObj -> (Json -> Parser a) -> Parser a
jsonFieldRef :: LT.Ty -> ValueE
jsonFieldRef ty = RefE ([ty], "jsonField")

{- | `toJsonSum qsum` makes a `LamVal` function specification for encoding sum type values into their JSON representation `toJson :: <qsum> -> Json`.

 ```
 module Example

 sum Foo a b = Bar | Baz a | Qax a b
 ```

 Generated code in some functional language:

 ```
 fooToJson :: (Json a, Json b) => Foo a b -> Json
 fooToJson =
  \foo -> case foo of
    Bar -> jsonConstructor "Bar" []
    Baz x1 -> jsonConstructor "Baz" [toJson @a x1]
    Qax x1 x2 -> jsonConstructor "Baz" [toJson @a x1, toJson @b x2]
 ```

 TODO(bladyjoker): For completeness add `ty` as an argument?
-}
toJsonSum :: QSum -> ValueE
toJsonSum qsum@(_qtyn, _sumTy) =
  LamE
    ( \sumVal ->
        CaseE
          qsum
          sumVal
          ( \((ctorN, ctorTy), ctorVal) ->
              jsonConstructorRef
                @ ctorNameVal ctorN
                @ ListE [toJsonRef fieldTy @ fieldVal | (fieldVal, fieldTy) <- zip ctorVal ctorTy]
          )
    )

{- | `fromJsonSum ty qsum` makes a `LamVal` function for decoding sum type values from their JSON representation `fromJson :: Json -> Parser <ty>`.

 ```
 module Example

 sum Foo a b = Bar | Baz a | Qax a b
 ```

 Generated code in some functional language:

 ```
 fooFromJson :: (Json a, Json b) => Json -> Parser (Foo a b)
 fooFromJson =
    caseJsonConstructor "Foo" [
      ("Bar",
       \jsons -> case jsons of
                   [] -> succeedParse @(Foo a b) Bar)
                   _ -> fail "Expected a JSON Array with 0 elements"
      ),
      ("Baz",
       \jsons -> case jsons of
                   [x1] -> fromJson @a x1 >>= \y1 -> succeedParse @(Foo a b) (Baz y1)
                   _ -> fail "Expected a JSON Array with 1 elements"
      ),
      ("Qax",
       \jsons -> case jsons of
                   [x1, x2] -> fromJson @a x1 >>= \y1 -> fromJson @b x2 >>= \y2 -> succeedParse @(Foo a b) (Qax y1 y2)
                   _ -> fail "Expected a JSON Array with 2 elements"
      )
    ]
 ```
-}
fromJsonSum :: LT.Ty -> QSum -> ValueE
fromJsonSum ty (qtyN, sumTy) =
  caseJsonConstructor ty
    @ TextE (showQTyName qtyN)
    @ ListE
      ( [ TupleE (ctorNameVal ctorN) (LamE $ \jsons -> fromJsonCtor ctorN ctorTy jsons)
        | (ctorN, LT.TyProduct ctorTy _) <- OMap.assocs sumTy
        ]
      )
  where
    fromJsonCtor ctorN ctorTy jsons =
      CaseListE
        jsons
        [
          ( length ctorTy
          , \fields' -> fromJsonTys (zip ctorTy fields') ty (\xs -> succeedParseRef ty @ CtorE (qtyN, (ctorN, ctorTy)) xs)
          )
        ]
        (\_ -> failParseRef ty @ TextE ("Expected a JSON Array of " <> (Text.pack . show . length $ ctorTy) <> " elements"))

showQTyName :: PC.QTyName -> Text
showQTyName (_mn, _tyn) = "TODO(bladyjoker): Print qualified type name"

{- | `toJsonProduct qprod` makes a `LamVal` function for encoding product type values into their JSON representation `toJson :: <qprod> -> Json`.

 ```
 module Example

 product Foo a b = a b
 product Bar a = a
 ```

 Generated code in some functional language:

 ```
 fooToJson :: (Json a, Json b) => Foo a b -> Json
 fooToJson =
  \foo -> let foo (\x1 x2 -> jsonArray [toJson @a x1, toJson @b x2])

 barToJson :: Json a => Bar a -> Json
 barToJson =
  \bar -> let bar (\x1 -> toJson @a x1)
 ```
-}
toJsonProduct :: QProduct -> ValueE
toJsonProduct qprod@(_, prodTy) =
  LamE
    ( \prodVal ->
        LetE
          qprod
          prodVal
          ( \fieldVals ->
              case (fieldVals, prodTy) of
                ([], []) -> ErrorE "Got an empty Product type to print in `toJsonProduct`"
                ([fieldVal], [fieldTy]) -> toJsonRef fieldTy @ fieldVal
                (fieldVals', fieldTys)
                  | length fieldVals' == length fieldTys ->
                      jsonArrayRef
                        @ ListE
                          [ toJsonRef fieldTy @ fieldVal
                          | (fieldVal, fieldTy) <- zip fieldVals' fieldTys
                          ]
                _ -> ErrorE "Got mismatching number of variables in `toJsonProduct`"
          )
    )

{- | `fromJsonProduct qprod` makes a `LamVal` function for decoding product type values from their JSON representation `fromJson :: Json -> Parser <qprod>`.

 ```
 module Example

 product Foo a b = a b
 product Bar a = a
 ```

 Generated code in some functional language:

 ```
 fooFromJson :: (Json a, Json b) => Json -> Parser (Foo a b)
 fooFromJson =
  caseJsonArray "Foo"
    (\jsons -> case jsons of
                 [x1, x2] -> fromJson @a x1 >>= \y1 -> fromJson @b x2 >>= \y2 -> succeedParse @(Foo a b) (Foo y1 y2))
                 _ -> (failParse @(Foo a b) "Expected a JSON Array of 2 elements")
    )

 barToJson :: Json a => Bar a -> Json
 barToJson =
  \json -> fromJson @a json >>= \y1 -> succeedParse @(Bar a) (Bar y1)
 ```
-}
fromJsonProduct :: LT.Ty -> QProduct -> ValueE
fromJsonProduct ty qprod@(_, [fieldTy]) = LamE $ \json ->
  bindParseRef fieldTy ty
    @ (fromJsonRef fieldTy @ json)
    @ LamE (\fieldVal -> succeedParseRef ty @ ProductE qprod [fieldVal])
fromJsonProduct ty qprod@(qtyN, prodTy) =
  caseJsonArrayRef ty
    @ TextE (showQTyName qtyN)
    @ LamE
      ( \jsons ->
          CaseListE
            jsons
            [
              ( length prodTy
              , \jsons' -> fromJsonTys (zip prodTy jsons') ty (\xs -> succeedParseRef ty @ ProductE qprod xs)
              )
            ]
            (\_ -> failParseRef ty @ TextE ("Expected a JSON Array of " <> (Text.pack . show . length $ prodTy) <> " elements"))
      )

{- | `toJsonRecord qrec` makes a `LamVal` function for encoding record type values into their JSON representation `toJson :: <qrec> -> Json`.

 ```
 module Example

 record Foo a b = { fooA : a, fooB : b }
 record Bar a = { barA: a }
 ```

 Generated code in some functional language:

 ```
 fooToJson :: (Json a, Json b) => Foo a b -> Json
 fooToJson =
  \foo -> jsonObject [ ("fooA", toJson @a foo.fooA), ("fooB", toJson @b foo.fooB) ]

 barToJson :: Json a => Bar a -> Json
 barToJson =
  \bar -> jsonObject [ ("barA", toJson @a bar.barA) ]
 ```
-}
toJsonRecord :: QRecord -> ValueE
toJsonRecord (qtyN, recTy) =
  LamE
    ( \recVal ->
        jsonObjRef
          @ ListE
            [ TupleE (fieldNameVal fieldName) (toJsonRef fieldTy @ FieldE (qtyN, fieldName) recVal)
            | (fieldName, fieldTy) <- sortOn fst $ OMap.assocs recTy
            ]
    )

{- | `fromJsonRecord ty qrec` makes a `LamVal` function for decoding record type values from their JSON representation `fromJson :: Json -> Parser <ty>`.

 ```
 ```
 module Example

 record Foo a b = { fooA : a, fooB : b }
 record Bar a = { barA: a }
 ```

 Generated code in some functional language:

 ```
 fooFromJson :: (Json a, Json b) => Json -> Parser (Foo a b)
 fooFromJson =
  caseJsonObject \jsonObj ->
                   jsonField "fooA" jsonObj
                     \json'fooA -> fromJson @a json'fooA >>=
                        \fooA -> jsonObjField "fooB" jsonObj
                           \json'fooB -> fromJson @a json'fooB >>=
                             \fooB -> succeedParse @(Foo a b) (Foo {fooA, fooB})

 barToJson :: Json a => Bar a -> Json
 barToJson =
  caseJsonObj \jsonObj ->
                jsonField "barA" jsonObj
                  \json'barA -> fromJson @a json'barA >>=
                    \barA -> succeedParseRef @(Bar a) (Bar {barA})
 ```
 NOTE(bladyjoker): Infinitely complicated --.-- I wanna puke
-}
fromJsonRecord :: LT.Ty -> QRecord -> ValueE
fromJsonRecord ty qrec@(_, recTy) =
  caseJsonObjectRef ty @ LamE (\jsonObj -> parseRecordFromJson jsonObj (OMap.assocs recTy) [])
  where
    parseRecordFromJson _jsonObj [] parsedFields = succeedParseRef ty @ RecordE qrec parsedFields
    parseRecordFromJson jsonObj (field@(fieldN, fieldTy) : rest) parsedFields =
      jsonFieldRef fieldTy
        @ fieldNameVal fieldN
        @ jsonObj
        @ LamE
          ( \fieldJson ->
              bindParseRef fieldTy ty
                @ (fromJsonRef fieldTy @ fieldJson)
                @ LamE (\fieldVal -> parseRecordFromJson jsonObj rest ((field, fieldVal) : parsedFields))
          )

-- | Hooks
deriveToJsonImpl :: PC.ModuleName -> PC.TyDefs -> PC.Ty -> Either P.InternalError ValueE
deriveToJsonImpl mn tydefs = deriveImpl mn tydefs toJsonSum toJsonProduct toJsonRecord

deriveFromJsonImpl :: PC.ModuleName -> PC.TyDefs -> PC.Ty -> Either P.InternalError ValueE
deriveFromJsonImpl mn tydefs ty = deriveImpl mn tydefs (fromJsonSum (LT.fromTy ty)) (fromJsonProduct (LT.fromTy ty)) (fromJsonRecord $ LT.fromTy ty) ty

-- | Helpers
ctorNameVal :: PC.InfoLess PC.ConstrName -> ValueE
ctorNameVal cn = PC.withInfoLess cn (TextE . view #name)

fieldNameVal :: PC.InfoLess PC.FieldName -> ValueE
fieldNameVal fn = PC.withInfoLess fn (TextE . view #name)

fromJsonTys :: [(LT.Ty, ValueE)] -> LT.Ty -> ([ValueE] -> ValueE) -> ValueE
fromJsonTys pdsToParse tyOut = go pdsToParse []
  where
    go [] tot cont = cont (reverse tot)
    go ((tyX, pdX) : rest) tot cont =
      bindParseRef tyX tyOut
        @ (fromJsonRef tyX @ pdX)
        @ LamE (\x -> go rest (x : tot) cont)
