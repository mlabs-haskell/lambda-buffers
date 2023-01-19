{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds  #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}
module LambdaBuffers.Gen.RustGen.RustTy where

import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import Control.Applicative

import Prettyprinter

import LambdaBuffers.Gen.PP
import LambdaBuffers.Common.SourceTy (TyDef)
import LambdaBuffers.Common.Types
import LambdaBuffers.Gen.Generator

type RustGen c = Parser Rust c () (Doc ())

rint :: RustGen TypeDecl
rint = match Int >> pure "i32"

rstring :: RustGen TypeDecl
rstring = match String >> pure "String"

rbool :: RustGen TypeDecl
rbool = match Bool >> pure "bool"

rref :: RustGen TypeDecl
rref = do
  RefP (Name n) <- match (RefP _x)
  pure $ pretty n

rvar :: RustGen TypeDecl
rvar = do
  VarP n <- match  _x
  pure $ pretty n

rMaybe :: [RustGen TypeDecl] -> RustGen TypeDecl
rMaybe ps = do
  Maybe x <- match (Maybe _x)
  x' <- result (tyArg ps) x
  pure $ "Option<" <> x' <> ">"

rList :: [RustGen TypeDecl] -> RustGen TypeDecl
rList ps = do
  List x <- match (List _x)
  x'     <- result (tyArg ps) x
  pure $ "Vec<" <> x' <> ">"

-- this is wrong, fix later, correct version is somewhat tricky to write, think I need a special combinator
rApp :: [RustGen TypeDecl] -> RustGen TypeDecl
rApp ps =  do
  AppP p1 p2 <- match (AppP _a _x)
  p1' <- result (tyArg ps) p1
  p2' <- result (tyArg ps) p2
  pure $ p2' <> "<" <> p1' <> ">"


tyArg :: [RustGen TypeDecl] -> RustGen TypeDecl
tyArg ps = choice
         $  ps
         <> [ rMaybe (ps <> prims)
            , rList (ps <> prims)
            , rApp  (ps <> prims) ]
         <> prims
  where
    prims = [rint, rbool, rstring, rref, rvar]

recField :: [RustGen TypeDecl] -> RustGen TypeDecl
recField ps = do
  Name n := x <- match (_l := _x)
  x'          <- result (tyArg ps) x
  pure $ rRecField n x'

rustBareType :: [RustGen TypeDecl] -> RustGen TypeDecl
rustBareType ps = do
  ProdP (x :* Nil) <- match (ProdP (_x :* Nil))
  parens <$> result (tyArg ps) x

rRecFields :: [RustGen TypeDecl] -> RustGen TypeDecl
rRecFields ps = rBraces . vcat . punctuate "," <$> someP (recField ps)

rTupleFields :: [RustGen TypeDecl] -> RustGen TypeDecl
rTupleFields ps = parens . hcat . punctuate "," <$> someP (tyArg ps)

rType :: [RustGen TypeDecl] -> RustGen TypeDecl
rType ps = do
  (tvars,Sum (Name tName) _ p) <- rustify
  case p of
    SumP (Name cstr := RecP fields :* Nil) -> do
      fields' <- result (rRecFields ps) fields
      pure $ structify cstr tvars fields'

    SumP (Name cstr := ProdP args :* Nil) -> do
      args' <- result (rTupleFields ps) args
      pure $ structify cstr tvars args' <> ";" -- weirdly tuple structs, but ONLY tuple structs, end w/ a semicolon. bad design rust!

    SumP ts -> do
      cstrs <- rBraces . vcat . punctuate "," <$> result (manyP sumConstr) ts
      pure $  "enum"
           <+> pretty tName
           <> tvars
           <+> cstrs

    _ -> error "invalid sum type"
 where
   sumConstr :: RustGen TypeDecl
   sumConstr = do
     (Name cName := body) <- match (_l := _x)
     body' <- result (rustBareType ps <|> recConstr <|> tupleConstr <|> nullary) body
     pure $ pretty cName <+> body'

   nullary = match Nil >> pure ""

   recConstr = do
     RecP fields <- match (RecP _x)
     result (rRecFields ps) fields

   tupleConstr = do
     ProdP fields <- match (ProdP _x)
     result (rTupleFields ps) fields

   structify :: Text -> Doc a -> Doc a -> Doc a
   structify cstr tvars fields
     = "struct"
     <+> pretty cstr
     <> tvars
     <+> fields
     <> line

rType' :: RustGen TypeDecl
rType' = rType []

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
     _            -> error "malformed list" -- fix later

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

rustify :: Parser Rust c  e (Doc a,Pat)
rustify = P $ \inp -> case rustify' inp of
  (doc,pat) -> pure  ((doc,pat),Nil)

rustify' :: Pat -> (Doc a,Pat)
rustify' p = let tvars = getTyParams p
                 sanitized = rustifyParams (S.fromList tvars) p
                 params    = fmtParams . map T.toTitle $ tvars
              in (params,sanitized)

parseTest :: TyDef -> IO ()
parseTest x = case parse rType' $ defToPat x of {Left err -> print err; Right a -> putStrLn "" >> print a}
