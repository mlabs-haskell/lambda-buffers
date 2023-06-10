-- | lbf-prelude.Prelude.Json class implementations
module LambdaBuffers.Codegen.LamVal.Json (deriveToJsonImpl, deriveFromJsonImpl) where

import Control.Lens (view, (^.))
import Data.Foldable (Foldable (toList))
import Data.Map.Ordered qualified as OMap
import Data.Text (Text)
import Data.Text qualified as Text
import LambdaBuffers.Codegen.LamVal (Product, QProduct, QRecord, QSum, Sum, ValueE (CaseE, CaseIntE, CaseListE, CaseTextE, CtorE, ErrorE, FieldE, IntE, LamE, LetE, ListE, ProductE, RecordE, RefE, TextE, TupleE), (@))
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

-- | `toJsonConstructor :: Text -> [Json] -> Json`
toJsonConstructorRef :: ValueE
toJsonConstructorRef = RefE ([], "toJsonConstructor")

-- | `toJsonUnitConstructor :: Text -> Json`
toJsonUnitConstructorRef :: ValueE
toJsonUnitConstructorRef = RefE ([], "toJsonUnitConstructor")

-- | `caseJsonConstructor :: (Text -> [Json] -> Parser a) -> Json -> Parser a`
caseJsonConstructor :: LT.Ty -> ValueE
caseJsonConstructor ty = RefE ([ty], "caseJsonConstructor")

-- | `caseJsonArray :: ([Json] -> Parser a) -> Json -> Parser a
caseJsonArrayRef :: LT.Ty -> ValueE
caseJsonArrayRef ty = RefE ([ty], "caseJsonArray")

{- | `caseJsonObjectField :: Text -> (Json -> Parser a) -> Json -> Parser a
NOTE(bladyjoker): This is not ideal, but it saved me from introducing MapE and LookupE in ValueE.
-}
caseJsonObjectFieldRef :: LT.Ty -> ValueE
caseJsonObjectFieldRef ty = RefE ([ty], "caseJsonObjectField")

-- | `listToJson :: [Json] -> Json`
listToJsonRef :: ValueE
listToJsonRef = RefE ([], "toJson")

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
    Bar -> toJsonUnitConstructor "Bar"
    Baz x1 -> toJsonConstructor "Baz" [toJson @a x1]
    Qax x1 x2 -> toJsonConstructor "Baz" [toJson @a x1, toJson @b x2]
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
              if isUnitProd ctorTy
                then
                  toJsonUnitConstructorRef
                    @ ctorNameVal ctorN
                else
                  toJsonConstructorRef
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
    caseJsonConstructor
      (\ctorNameGot fieldsJsonVal ->
        caseText ctorNameGot [
          ("Bar", caseList
                       fieldsJsonVal
                       (\[] -> succeedParse @(Foo a b) Bar)
                       fail
          ),
          ("Baz", caseList
                     fieldsJsonVal
                       (\[x1] -> fromJson @a x1 >>= \y1 -> succeedParse @(Foo a b) (Baz y1))
                       fail
          ),
          ("Qax", caseList
                     fieldsJsonVal
                       (\[x1, x2] -> fromJson @a x1 >>= \y1 -> fromJson @b x2 >>= \y2 -> succeedParse @(Foo a b) (Qax y1 y2))
                       fail
          )
        ]
      )
```
-}
fromJsonSum :: LT.Ty -> QSum -> ValueE
fromJsonSum ty (qtyN, sumTy) =
  caseJsonConstructor ty
    @ LamE
      ( \ctorNameGot -> LamE $ \fieldsJsonVal ->
          CaseTextE
            ctorNameGot
            [ ( ctorNameVal ctorN
              , fromJsonCtor ctorN ctorTy fieldsJsonVal
              )
            | (ctorN, LT.TyProduct ctorTy _) <- OMap.assocs sumTy
            ]
            (\_ -> failParseRef ty)
      )
  where
    {- `fromJsonCtor "Baz" [a] fields`
      caseList fields
        (\[x1] -> fromJson @a x1 >>= \y1 -> return (Baz y1))
        fail
    -}
    fromJsonCtor ctorN ctorTy fields =
      CaseListE
        fields
        [
          ( length ctorTy
          , \fields' -> fromJsonTys (zip ctorTy fields') ty (\xs -> succeedParseRef ty @ CtorE (qtyN, (ctorN, ctorTy)) xs)
          )
        ]
        (\_ -> failParseRef ty @ TextE ("Expected a JSON Array of " <> (Text.pack . show . length $ ctorTy) <> " elements to parse a " <> (Text.pack . show $ ctorN) <> " of a LambdaBuffers sum type " <> (Text.pack . show $ qtyN)))

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
  \foo -> let foo (\x1 x2 -> listToJson [toJson @a x1, toJson @b x2])

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
                      listToJsonRef
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
  caseJsonArray
    (\fields -> caseList fields
                  (\x1 x2 -> fromJson @a x1 >>= \y1 -> fromJson @b x2 >>= \y2 -> succeedParse @(Foo a b) (Foo y1 y2))
                  (failParse @(Foo a b) "Expected a JSON Array of size 2 when decoding a LambdaBuffers Example.Foo product type")
    )

barToJson :: Json a => Bar a -> Json
barToJson =
  \json -> fromJson @a json >>= \y1 -> succeedParse @(Bar a) (Bar y1)
```
-}
fromJsonProduct :: LT.Ty -> QProduct -> ValueE
fromJsonProduct ty qprod@(_, [fieldTy]) = LamE $ \jsonVal ->
  bindParseRef fieldTy ty
    @ (fromJsonRef fieldTy @ jsonVal)
    @ LamE (\fieldVal -> succeedParseRef ty @ ProductE qprod [fieldVal])
fromJsonProduct ty qprod@(_, prodTy) =
  caseJsonArrayRef ty
    @ LamE
      ( \jsonVals ->
          CaseListE
            jsonVals
            [
              ( length prodTy
              , \pds' -> fromJsonTys (zip prodTy pds') ty (\xs -> succeedParseRef ty @ ProductE qprod xs)
              )
            ]
            (\_ -> failParseRef ty)
      )

{- | `toJsonProduct qprod` makes a `LamVal` function for encoding product type values into their JSON representation `toJson :: <qprod> -> Json`.

```
module Example

record Foo a b = { fooA : a, fooB : b }
record Bar a = { barA: a }
```

Generated code in some functional language:

```
fooToJson :: (Json a, Json b) => Foo a b -> Json
fooToJson =
  \foo -> mapToJson [ ("fooA", toJson @a (getField @"fooA" foo), ("fooB", toJson @b (getField @"fooB" foo) ]

barToJson :: Json a => Bar a -> Json
barToJson =
  \bar -> mapToJson [ ("barA", toJson @a (getField @"barA" bar) ]
```
-}
toJsonRecord :: QRecord -> ValueE
toJsonRecord (qtyN, recTy) =
  LamE
    ( \recVal ->
        case OMap.assocs recTy of
          [] -> ErrorE "Got an empty Record type to print in `toJsonRecord`"
          _ ->
            listToJsonRef
              @ ListE
                [ TupleE (fieldNameVal fieldName) (toJsonRef fieldTy @ FieldE (qtyN, fieldName) recVal)
                | (fieldName, fieldTy) <- OMap.assocs recTy
                ]
    )

{- | `fromJsonRecord qrec` makes a `LamVal` function for decoding record type values from their JSON representation `fromJson :: Json -> Parser <qprod>`.

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
  \json -> caseJsonObjectField @a "fooA" json >>=
             \fooA -> caseJsonObjectField @b "fooB" json >>=
               \fooB -> succeedParse @(Foo a b) (Foo {fooA, fooB})

barToJson :: Json a => Bar a -> Json
barToJson =
  \json -> caseJsonObjectField @a "barA" json >>=
             \barA -> succeedParse @(Bar a) (Bar {barA})
```
NOTE(bladyjoker): Infinitely complicated --.-- I wanna puke
-}
fromJsonRecord :: LT.Ty -> QRecord -> ValueE
fromJsonRecord ty qrec@(_, recTy) = LamE $ \jsonVal -> parseRecordFromJson jsonVal (OMap.assocs recTy) []
  where
    parseRecordFromJson _jsonVal [] parsedFields = succeedParseRef ty @ RecordE qrec parsedFields
    parseRecordFromJson jsonVal (field@(fieldN, fieldTy) : rest) parsedFields =
      caseJsonObjectFieldRef fieldTy
        @ fieldNameVal fieldN
        @ LamE (\fieldVal -> parseRecordFromJson jsonVal rest ((field, fieldVal) : parsedFields))
        @ jsonVal

-- | Hooks
deriveToJsonImpl :: PC.ModuleName -> PC.TyDefs -> PC.Ty -> Either P.InternalError ValueE
deriveToJsonImpl mn tydefs = deriveImpl mn tydefs toJsonSum toJsonProduct toJsonRecord

deriveFromJsonImpl :: PC.ModuleName -> PC.TyDefs -> PC.Ty -> Either P.InternalError ValueE
deriveFromJsonImpl mn tydefs ty = deriveImpl mn tydefs (fromJsonSum (LT.fromTy ty)) (fromJsonProduct (LT.fromTy ty)) (fromJsonRecord $ LT.fromTy ty) ty

-- | Helpers
isUnitProd :: Product -> Bool
isUnitProd = null

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
