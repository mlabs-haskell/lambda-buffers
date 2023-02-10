{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}

module LambdaBuffers.Compiler.TypeClassCheck (detectSuperclassCycles, detectSuperclassCycles') where

import Control.Lens.Combinators (view)
import Control.Lens.Operators ((^.))
import Data.Generics.Labels ()
import Data.List (foldl')
import Data.Map qualified as M
import Data.Text (Text)
import LambdaBuffers.Compiler.ProtoCompat.Types (
  ClassDef (),
  ForeignClassRef (ForeignClassRef),
  LocalClassRef (LocalClassRef),
  TyClassRef (ForeignCI, LocalCI),
 )
import Prettyprinter (
  Doc,
  Pretty (pretty),
  hcat,
  indent,
  line,
  punctuate,
  vcat,
 )

data ClassInfo = ClassInfo {ciName :: Text, ciSupers :: [Text]}
  deriving stock (Show, Eq, Ord)

detectSuperclassCycles' :: [ClassDef] -> [[Text]]
detectSuperclassCycles' = detectCycles . mkClassGraph . map defToClassInfo
  where
    defToClassInfo :: ClassDef -> ClassInfo
    defToClassInfo cd =
      ClassInfo (cd ^. #className . #name) $
        map (extractName . view #classRef) (cd ^. #supers)
      where
        extractName = \case
          LocalCI (LocalClassRef nm _) -> nm ^. #name
          ForeignCI (ForeignClassRef nm _ _) -> nm ^. #name

    mkClassGraph :: [ClassInfo] -> M.Map Text [Text]
    mkClassGraph = foldl' (\acc (ClassInfo nm sups) -> M.insert nm sups acc) M.empty

    detectCycles :: forall k. Ord k => M.Map k [k] -> [[k]]
    detectCycles m = concatMap (detect []) (M.keys m)
      where
        detect :: [k] -> k -> [[k]]
        detect visited x = case M.lookup x m of
          Nothing -> []
          Just xs ->
            if x `elem` visited
              then [x : visited]
              else concatMap (detect (x : visited)) xs

detectSuperclassCycles :: forall a. [ClassDef] -> Maybe (Doc a)
detectSuperclassCycles cds = case detectSuperclassCycles' cds of
  [] -> Nothing
  xs ->
    Just $
      "Error: Superclass cycle(s) detected"
        <> line
        <> indent 2 (vcat $ map format xs)
  where
    format :: [Text] -> Doc a
    format = hcat . punctuate " => " . map pretty
