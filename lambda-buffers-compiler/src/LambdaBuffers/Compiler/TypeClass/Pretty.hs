{-# LANGUAGE OverloadedStrings #-}
-- orphans are the whole point of this module!
{-# OPTIONS_GHC -Wno-orphans #-}

module LambdaBuffers.Compiler.TypeClass.Pretty (
  spaced,
  pointies,
  (<//>),
  (<///>),
) where

import Control.Lens.Operators ((^.))
import Data.Generics.Labels ()
import Data.Text qualified as T
import LambdaBuffers.Compiler.ProtoCompat qualified as P
import LambdaBuffers.Compiler.TypeClass.Pat
import LambdaBuffers.Compiler.TypeClass.Rules (
  Class (Class),
  Constraint (C),
  FQClassName (FQClassName),
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

instance Pretty P.ModuleName where
  pretty (P.ModuleName pts _) = hcat . punctuate "." $ map (\x -> pretty $ x ^. #name) pts

instance Pretty FQClassName where
  pretty (FQClassName cn mnps) = hcat (punctuate "." . map pretty $ mnps) <> pretty cn

instance Pretty Class where
  pretty (Class nm _) = pretty nm

instance Pretty a => Pretty (Constraint a) where
  pretty (C cls p) = pretty cls <+> pretty p

instance Pretty a => Pretty (Rule a) where
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
    LitP lit -> case lit of
      Name t -> pretty t
      ModuleName ts -> hcat . punctuate "." . map pretty $ ts
      Opaque -> "<OPAQUE>"
      TyVar v -> pretty v
    RecP ps -> case patList ps of
      Nothing -> pretty ps
      Just fields -> case traverse prettyField fields of
        Just fs -> braces . nest 2 . hcat . punctuate ", " $ fs
        Nothing -> pretty ps
    ProdP NilP -> ""
    ProdP xs -> case patList xs of
      Just [f] -> pretty f
      Just fs -> parens . hcat . punctuate ", " . map pretty $ fs
      _ -> pretty xs
    SumP xs -> case patList xs of
      Nothing -> pretty xs
      Just cs -> case traverse prettyConstr cs of
        Nothing -> pretty cs
        Just cstrs -> nest 2 . sumFmt $ cstrs
    plist@(ConsP p1 p2) -> case patList plist of
      Just pl -> list . map pretty $ pl
      Nothing -> pretty p1 <+> ":*" <+> pretty p2
    NilP -> "Nil"
    RefP mn@(LitP (ModuleName _)) n@(LitP (Name _)) -> pretty mn <> "." <> pretty n
    RefP NilP (LitP (Name n)) -> pretty n
    RefP p1 p2 -> parens $ "Ref" <+> pretty p1 <+> pretty p2
    -- Pattern variables are uppercased to distinguish them from proper TyVars
    VarP t -> pretty (T.toUpper t)
    ap@(AppP p1 p2) -> case prettyApp ap of
      Just pap -> parens pap
      Nothing -> "App" <+> pretty p1 <+> pretty p2
    LabelP p1 p2 -> pretty p1 <+> ":=" <+> pretty p2
    DecP nm args body -> case nm of
      LitP (Name n) -> case patList args of
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
        LabelP (LitP (Name l)) t -> Just $ pretty l <+> "::" <+> pretty t
        _ -> Nothing

      prettyConstr :: forall a. Pat -> Maybe (Doc a)
      prettyConstr = \case
        LabelP (LitP (Name l)) (ProdP NilP) -> Just $ pretty l
        LabelP (LitP (Name l)) t -> Just $ pretty l <+> pretty t
        _ -> Nothing

      -- this is kind of annoying to get right, don't think this is it
      prettyApp :: forall a. Pat -> Maybe (Doc a)
      prettyApp = \case
        AppP p1 p2 -> (pretty p1 <+>) <$> prettyApp p2
        other -> Just $ pretty other

instance Pretty Exp where
  pretty = \case
    LitE lit -> case lit of
      Name t -> pretty t
      ModuleName ts -> hcat . punctuate "." . map pretty $ ts
      Opaque -> "<OPAQUE>"
      TyVar v -> pretty v
    RecE ps -> case expList ps of
      Nothing -> pretty ps
      Just fields -> case traverse prettyField fields of
        Just fs -> braces . nest 2 . hcat . punctuate ", " $ fs
        Nothing -> pretty ps
    ProdE NilE -> ""
    ProdE xs -> case expList xs of
      Just [f] -> pretty f
      Just fs -> parens . hcat . punctuate ", " . map pretty $ fs
      _ -> pretty xs
    SumE xs -> case expList xs of
      Nothing -> pretty xs
      Just cs -> case traverse prettyConstr cs of
        Nothing -> pretty cs
        Just cstrs -> nest 2 . sumFmt $ cstrs
    plist@(ConsE p1 p2) -> case expList plist of
      Just pl -> list . map pretty $ pl
      Nothing -> pretty p1 <+> ":*" <+> pretty p2
    NilE -> "Nil"
    RefE mn@(LitE (ModuleName _)) n@(LitE (Name _)) -> pretty mn <> "." <> pretty n
    RefE NilE (LitE (Name n)) -> pretty n
    RefE p1 p2 -> parens $ "Ref" <+> pretty p1 <+> pretty p2
    -- Pattern variables are uppercased to distinguish them from proper TyVars
    ap@(AppE p1 p2) -> case prettyApp ap of
      Just pap -> parens pap
      Nothing -> "App" <+> pretty p1 <+> pretty p2
    LabelE p1 p2 -> pretty p1 <+> ":=" <+> pretty p2
    DecE nm args body -> case nm of
      LitE (Name n) -> case expList args of
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
      prettyField :: forall a. Exp -> Maybe (Doc a)
      prettyField = \case
        LabelE (LitE (Name l)) t -> Just $ pretty l <+> "::" <+> pretty t
        _ -> Nothing

      prettyConstr :: forall a. Exp -> Maybe (Doc a)
      prettyConstr = \case
        LabelE (LitE (Name l)) (ProdE NilE) -> Just $ pretty l
        LabelE (LitE (Name l)) t -> Just $ pretty l <+> pretty t
        _ -> Nothing

      -- this is kind of annoying to get right, don't think this is it
      prettyApp :: forall a. Exp -> Maybe (Doc a)
      prettyApp = \case
        AppE p1 p2 -> (pretty p1 <+>) <$> prettyApp p2
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
