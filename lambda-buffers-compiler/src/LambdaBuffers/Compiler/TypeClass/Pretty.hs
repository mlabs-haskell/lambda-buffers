{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
-- orphans are the whole point of this module!
{-# OPTIONS_GHC -Wno-orphans #-}

module LambdaBuffers.Compiler.TypeClass.Pretty (
  spaced,
  pointies,
  (<//>),
  (<///>),
) where

import Control.Lens ((^.))
import Data.Generics.Labels ()
import LambdaBuffers.Compiler.ProtoCompat qualified as P
import LambdaBuffers.Compiler.TypeClass.Pat (Pat (..), patList)
import LambdaBuffers.Compiler.TypeClass.Rules (
  Class (Class),
  ClassRef (CRef),
  Constraint (..),
  Instance,
  Rule ((:<=)),
 )
import Prettyprinter (
  Doc,
  Pretty (pretty),
  braces,
  encloseSep,
  hcat,
  line,
  list,
  nest,
  parens,
  punctuate,
  (<+>),
 )

instance Pretty ClassRef where
  pretty (CRef nm mn) = pretty mn <> "." <> pretty (nm ^. #name)

instance Pretty P.ModuleName where
  pretty (P.ModuleName pts _) = hcat . punctuate "." $ map (\x -> pretty $ x ^. #name) pts

instance Pretty Class where
  pretty (Class nm _) = pretty nm

instance Pretty Constraint where
  pretty (C cls p) = pretty cls <+> pretty p

instance Pretty Instance where
  pretty (c :<= []) = pretty c
  pretty (c :<= cs) = pretty c <+> "<=" <+> list (pretty <$> cs)

instance Pretty P.SourcePosition where
  pretty (P.SourcePosition col row) = pretty row <> ":" <> pretty col

instance Pretty P.SourceInfo where
  pretty (P.SourceInfo fname f t) =
    pretty fname <+> pretty f <> "-" <> pretty t

-- pretty should emit valid Haskell for well-formed DecPs
instance Pretty Pat where
  pretty = \case
    Name t -> pretty t
    ModuleName ts -> hcat . punctuate "." . map pretty $ ts
    Opaque -> "<OPAQUE>"
    RecP ps -> case patList ps of
      Nothing -> pretty ps
      Just fields -> case traverse prettyField fields of
        Just fs -> braces . nest 2 . hcat . punctuate ", " $ fs
        Nothing -> pretty ps
    ProdP Nil -> ""
    ProdP xs -> case patList xs of
      Just [f] -> pretty f
      Just fs -> parens . hcat . punctuate ", " . map pretty $ fs
      _ -> pretty xs
    SumP xs -> case patList xs of
      Nothing -> pretty xs
      Just cs -> case traverse prettyConstr cs of
        Nothing -> pretty cs
        Just cstrs -> nest 2 . sumFmt $ cstrs
    plist@(p1 :* p2) -> case patList plist of
      Just pl -> list . map pretty $ pl
      Nothing -> pretty p1 <+> ":*" <+> pretty p2
    Nil -> "Nil"
    RefP mn@(ModuleName _) n@(Name _) -> pretty mn <> "." <> pretty n
    RefP Nil (Name n) -> pretty n
    RefP p1 p2 -> parens $ "Ref" <+> pretty p1 <+> pretty p2
    VarP t -> pretty t
    ap@(AppP p1 p2) -> case prettyApp ap of
      Just pap -> parens pap
      Nothing -> "App" <+> pretty p1 <+> pretty p2
    p1 := p2 -> pretty p1 <+> ":=" <+> pretty p2
    DecP nm args body -> case nm of
      Name n -> case patList args of
        Nothing -> "Dec" <+> pretty n <+> pretty args <+> "=" <+> pretty body
        Just [] ->
          "data"
            <+> pretty n
            <+> "="
            <+> pretty body
        Just vars ->
          "data"
            <+> pretty n
            <+> hcat (punctuate " " . map pretty $ vars)
            <+> "="
            <+> pretty body
      _ -> "Dec" <+> pretty nm <+> pretty args <+> "=" <+> pretty body
    where
      prettyField :: forall a. Pat -> Maybe (Doc a)
      prettyField = \case
        Name l := t -> Just $ pretty l <+> "::" <+> pretty t
        _ -> Nothing

      prettyConstr :: forall a. Pat -> Maybe (Doc a)
      prettyConstr = \case
        Name l := (ProdP Nil) -> Just $ pretty l
        Name l := t -> Just $ pretty l <+> pretty t
        _ -> Nothing

      -- this is kind of annoying to get right, don't think this is it
      prettyApp :: forall a. Pat -> Maybe (Doc a)
      prettyApp = \case
        AppP p1 p2 -> (pretty p1 <+>) <$> prettyApp p2
        other -> Just $ pretty other

sumFmt :: [Doc a] -> Doc a
sumFmt = encloseSep "" "" " | "

spaced :: Doc a -> Doc a
spaced d = line <> d <> line

(<//>) :: Doc a -> Doc a -> Doc a
d1 <//> d2 = d1 <> line <> d2

(<///>) :: Doc a -> Doc a -> Doc a
d1 <///> d2 = d1 <> line <> line <> d2

pointies :: Doc a -> Doc a
pointies d = "<<" <> d <> ">>"
