{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Gen.Rust where

import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import Common.SourceTy (TyDef)

import Common.Types
import Common.Match
import Gen.Generator
import Data.Bifunctor
import Control.Applicative

import Prettyprinter

type CodeGen = Parser () (Doc ())

(</>) :: Doc a -> Doc a -> Doc a
a </> b = a <> line <> b

rBraces :: Doc a -> Doc a
rBraces d = "{" </> indent 2 d </> "}"

mkRecField :: Text -> Doc a -> Doc a
mkRecField l f = pretty l <> ":" <+> f

rint :: CodeGen
rint = match Int >> pure "i32"

rstring :: CodeGen
rstring = match String >> pure "String"

rbool :: CodeGen
rbool = match Bool >> pure "bool"

rref :: CodeGen
rref = do
  RefP (Name n) <- match (RefP _x)
  pure $ pretty n

rvar :: CodeGen
rvar = do
  VarP n <- match  _x
  pure $ pretty n

rMaybe :: CodeGen
rMaybe = do
  Maybe x <- match (Maybe _x)
  x' <- result tyArg x
  pure $ "Option<" <> x' <> ">"

rList :: CodeGen
rList = do
  List x <- match (List _x)
  x'     <- result tyArg x
  pure $ "Vec<" <> x' <> ">"

-- this is wrong, fix later, correct version is somewhat tricky to write, think I need a special combinator
rApp :: CodeGen
rApp =  do
  AppP p1 p2 <- match (AppP _a _x)
  p1' <- result tyArg p1
  p2' <- result tyArg p2
  pure $ p2' <> "<" <> p1' <> ">"

tyArg :: CodeGen
tyArg = rint
     <|> rstring
     <|> rbool
     <|> rref
     <|> rMaybe
     <|> rList
     <|> rApp
     <|> rvar

recField :: CodeGen
recField = do
  Name n := x <- match (_l := _x)
  x'          <- result tyArg x
  pure $ mkRecField n x'

rustBareType :: CodeGen
rustBareType = do
  ProdP (x :* Nil) <- match (ProdP (_x :* Nil))
  parens <$> result tyArg x

rRecFields :: CodeGen
rRecFields = rBraces . vcat . punctuate "," <$> someP recField

rTupleFields :: CodeGen
rTupleFields = parens . hcat . punctuate "," <$> someP tyArg

rType :: CodeGen
rType = do
  (tvars,Sum (Name tName) _ p) <- rustify
  case p of
    t@(SumP (Name cstr := RecP fields :* Nil)) -> do
      fields' <- result rRecFields fields
      pure $ "struct"
           <+> pretty cstr
           <> tvars
           <+> fields'
           <> ";"
           <> line

    t@(SumP (Name cstr := ProdP args :* Nil)) -> do
      args' <- result rTupleFields args
      pure $ "struct"
           <+> pretty cstr
           <>  tvars
           <+> args'
           <> ";"
           <> line

    SumP ts -> do
      cstrs <- rBraces . vcat . punctuate "," <$> result (manyP sumConstr) ts
      pure $  "enum"
           <+> pretty tName
           <> tvars
           <+> cstrs

    other -> error "invalid sum type"
 where
   sumConstr :: CodeGen
   sumConstr = do
     (Name cName := body) <- match (_l := _x)
     body' <- result (rustBareType <|> recConstr <|> tupleConstr) body
     pure $ pretty cName <+> body'

   recConstr = do
     RecP fields <- match (RecP _x)
     result rRecFields fields

   tupleConstr = do
     ProdP fields <- match (ProdP _x)
     result rTupleFields fields

{- this is all just stupid boilerplate for formatting type variables in a rust-ly way
   note: this is an artifact of the old approach, can be substantially simplified w/ the new one
-}
fmtParams :: [Text] -> Doc a
fmtParams []     = ""
fmtParams xs     = pointy . hcat . punctuate ", " . map pretty $ xs
  where
    pointy d = "<" <> d <> ">"

getTyParams :: Pat -> [Text]
getTyParams = \case
     DecP _ vs _      -> getTyParams vs
     (Name n) :* rest ->  n : getTyParams rest
     Nil              -> []
     other            -> error "malformed list" -- fix later

rustifyParams :: S.Set Text -> Pat -> Pat
rustifyParams tvs = \case
  vx@(VarP v) -> if S.member v tvs
                 then VarP (T.toTitle v)
                 else vx
  nx@(Name n) -> if S.member n tvs
                 then Name (T.toTitle n)
                 else nx
  List x      -> List $ go x
  Maybe x     -> Maybe $ go x
  (x :* xs)   -> go x :* go xs
  l := x      -> go l := go x
  Map k v     -> Map (go k) (go v)
  Either l r  -> Either (go l) (go r)
  ProdP xs    -> ProdP (go xs)
  RecP xs     -> RecP (go xs)
  SumP xs     -> SumP (go xs)
  AppP t1 t2  -> AppP (go t1) (go t2)
  RefP x      -> RefP (go x)
  DecP a b c  -> DecP (go a) (go b) (go c)
  other       -> other
 where
   go = rustifyParams tvs

rustify :: Parser e (Doc a,Pat)
rustify = P $ \inp -> case rustify' inp of
  (doc,pat) -> pure  ((doc,pat),Nil)

rustify' :: Pat -> (Doc a,Pat)
rustify' p = let tvars = getTyParams p
                 sanitized = rustifyParams (S.fromList tvars) p
                 params    = fmtParams . map T.toTitle $ tvars
              in (params,sanitized)

parseTest :: TyDef -> IO ()
parseTest x = case parse rType $ defToPat x of {Left err -> print err; Right a -> putStrLn "" >> print a}
