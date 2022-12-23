{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Gen.Rust where

import Data.Set (Set)
import Data.Set qualified as S
import Data.Text (Text)
import Data.Text qualified as T

import Common.Types
import Gen.Generator

import Debug.Trace

-- these are just convenience pattern variables
k, v, _a, _l, _x, _xs, _name, _vars :: Pat
k = VarP "k"
v = VarP "v"
_a = VarP "a"
_l = VarP "l"
_x = VarP "x"
_xs = VarP "xs"
_name = VarP "name"
_vars = VarP "vars"
_nil = VarP "nil"

{- utilities for formatting code -}
brackets :: Text -> Text
brackets t = "{\n" <> t <> " }"

indent :: Text -> Text
indent t = "  " <> t

indent2 :: Text -> Text
indent2 = indent . indent

parens :: Text -> Text
parens t = "(" <> t <> ")"

break :: Text -> Text
break t = t <> "\n"

-- error throwing utility
matchFail :: Pat -> Pat -> Either GenError a
matchFail p1 p2 = Left $ MatchFail p1 p2

{- Primitives -}
rustInt :: Gen
rustInt = primGen IntP "i64"

rustString :: Gen
rustString = primGen StringP "String"

rustBool :: Gen
rustBool = primGen BoolP "bool"

rustRef :: Gen
rustRef = Gen (RefP _x) $ \_ pat -> case pat of
  (RefP (Name n)) -> pure n
  other -> matchFail (RefP _x) other

{- Functors -}
rustMaybe :: Gen
rustMaybe = Gen (Maybe _x) $ \scope pat -> case pat of
  Maybe t -> do
    t' <- runGenerator scope t
    pure $ "Option<" <> t' <> ">"
  other -> matchFail (Maybe _x) other

rustList :: Gen
rustList = Gen (List _x) $ \scope pat -> case pat of
  List t -> do
    t' <- runGenerator scope t
    pure $ "Vec<" <> t' <> ">"
  other -> matchFail (List _x) other

{- Records -}
rustRecFields :: Gen
rustRecFields = Gen (RecFields _l _x _xs) $ \scope pat -> case pat of
  RecFields (Name n) t Nil -> do
    t' <- runGenerator scope t
    pure $ indent2 $ n <> ": " <> t' -- <> ","
  RecFields (Name n) t ys -> do
    t' <- runGenerator scope t
    rest <- runGenerator scope (RecP ys)
    pure $ indent2 $ n <> ": " <> t' <> "," <> "\n" <> rest
  other -> matchFail (RecFields _l _x _xs) other

rustRecConstr :: Gen
rustRecConstr = Gen (RecBody _l _xs) $ \scope pat -> case pat of
  RecBody (Name nm) fields -> do
    body <- runGenerator scope (RecP fields)
    pure $ nm <> " " <> brackets body
  other -> matchFail (RecBody _l _xs) other

rustBareType :: Gen
rustBareType = Gen (ProdP (_x :* Nil)) $ \scope pat -> case pat of
  ProdP (t :* Nil) -> runGenerator scope t
  other -> matchFail (ProdP (_x :* Nil)) other

rustTupleBody :: Gen
rustTupleBody = Gen (ProdP (_a :* _x :* _xs)) $ \scope pat -> case pat of
  ProdP (t :* ts) -> do
    t' <- runGenerator scope t
    ts' <- runGenerator scope (ProdP ts)
    pure $ t' <> ", " <> ts'
  other -> matchFail (ProdP (_x :* _xs)) other

rustTupleConstr :: Gen
rustTupleConstr = Gen (ProdBody _l _xs) $ \scope pat -> case pat of
  ProdBody (Name cName) ys -> do
    ys' <- runGenerator scope (ProdP ys)
    pure $ cName <> parens ys'
  other -> matchFail (_l := ProdP _xs) other

rustTypeNoFreeVars :: Gen
rustTypeNoFreeVars = Gen (Sum _name _nil _x) $ \scope pat -> case pat of
  Sum (Name tName) Nil p -> case p of
    -- normal record struct, will be caught by rustRecConstr
    t@(SumP (_ := RecP _ :* Nil)) -> do
      struct <- runGenerator scope t
      pure $ "struct " <> struct <> ";\n"

    -- tuple struct, will be caught by rustTupleConstr
    t@(SumP (_ := ProdP _ :* Nil)) -> do
      tup <- runGenerator scope t
      pure $ "struct " <> tup <> ";\n"
    SumP ts@(_ :* _) -> do
      case pToList ts of
        Nothing -> Left $ MalformedPatList ts
        Just tsList -> do
          constructors <- mapM (runGenerator scope) tsList
          let enumBody = brackets (T.concat $ map (\x -> indent x <> ",\n") constructors) <> "\n"
          pure $ "\nenum " <> tName <> " " <> enumBody
    other -> matchFail (Sum _name _nil _x) other
  other -> error $ show other
  where
    pToList :: Pat -> Maybe [Pat]
    pToList = \case
      -- \/ NOTE: Fix rustRecConstr and rustTupleConstr so this stupid thing isn't necessary
      (z :* zs) -> (SumP (z :* Nil) :) <$> pToList zs
      Nil -> Just []
      _ -> Nothing

rustScope :: Set Gen
rustScope =
  S.fromList
    [ rustInt
    , rustString
    , rustBool
    , rustRef
    , rustMaybe
    , rustList
    , rustBareType
    , rustRecFields
    , rustRecConstr
    , rustTupleBody
    , rustTupleConstr
    , rustTypeNoFreeVars
    ]
