{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}

module LambdaBuffers.Compiler.TypeClassCheck (detectSuperclassCycles) where

import Control.Lens.Operators ((^.))
import Data.Generics.Labels ()
import Data.List (foldl')
import Data.Map qualified as M
import Data.Text (Text)
import LambdaBuffers.Common.ProtoCompat (
  ClassDef (ClassDef),
  ClassName (ClassName),
  Constraint (Constraint),
  Kind (Kind),
  KindRefType (KType),
  KindType (KindRef),
  LocalRef (LocalRef),
  SourceInfo (SourceInfo),
  SourcePosition (SourcePosition),
  Ty (TyRefI),
  TyArg (TyArg),
  TyName (TyName),
  TyRef (LocalI),
  VarName (VarName),
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
        map (\x -> x ^. #className . #name) (cd ^. #supers)

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

-- Unit tests. Really need to get that frontend working!
star :: Kind
star = Kind (KindRef KType) si

si :: SourceInfo
tv :: TyArg
si = SourceInfo "" (SourcePosition 0 0) (SourcePosition 0 0)

tv = TyArg (VarName "x" si) star si

mkclass :: Text -> [Text] -> ClassDef
mkclass nm sups = ClassDef (ClassName nm si) tv (map constraint sups) "" si

constraint :: Text -> Constraint
constraint nm = Constraint (ClassName nm si) (TyRefI (LocalI $ LocalRef (TyName "" si) si)) si

cycles :: [ClassDef]
cycles =
  [ mkclass "Foo" ["Bar", "Baz", "Beep"]
  , mkclass "Bar" ["Bip", "Bop"]
  , mkclass "Bop" ["Foo"]
  ]

nocycles :: [ClassDef]
nocycles =
  [ mkclass "Functor" []
  , mkclass "Applicative" ["Functor"]
  , mkclass "Monad" ["Applicative"]
  , mkclass "Traversable" ["Foldable", "Functor"]
  ]
