{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module LambdaBuffers.Compiler.TypeClass.Solve where

import Control.Lens.Operators ((^.))
import Data.Generics.Labels ()
import Data.List (foldl', sortBy)
import Data.Text (Text)
import Prettyprinter

import LambdaBuffers.Compiler.TH (mkLBTypes)
import LambdaBuffers.Compiler.TypeClass.Compat (defToPat')
import LambdaBuffers.Compiler.TypeClass.Match
import LambdaBuffers.Compiler.TypeClass.Pat
import LambdaBuffers.Compiler.TypeClass.Rules

import Data.Map qualified as M
import Data.Set qualified as S
import Data.Text qualified as T

import LambdaBuffers.Compiler.ProtoCompat qualified as P

{- Variable substitution. Given a string that represents a variable name,
   and a type to instantiate variables with that name to, performs the
   instantiation
-}
subV :: Text -> Pat -> Pat -> Pat
subV varNm t = \case
  var@(VarP v) -> if v == varNm then t else var
  List x -> List $ subV varNm t x
  Maybe x -> Maybe $ subV varNm t x
  x :* xs -> subV varNm t x :* subV varNm t xs
  l := x -> subV varNm t l := subV varNm t x
  Map k v -> Map (subV varNm t k) (subV varNm t v)
  Either l r -> Either (subV varNm t l) (subV varNm t r)
  ProdP xs -> ProdP (subV varNm t xs)
  RecP xs -> RecP (subV varNm t xs)
  SumP xs -> SumP (subV varNm t xs)
  AppP t1 t2 -> AppP (subV varNm t t1) (subV varNm t t2)
  RefP x -> RefP (subV varNm t x)
  DecP a b c -> DecP (subV varNm t a) (subV varNm t b) (subV varNm t c)
  other -> other

{- Performs substitution on an entire instance (the first argument) given the
   concrete types from a Pat (the second argument).

   Note that ONLY PatVars which occur in the Instance *HEAD* are replaced, though they
   are replaced in the instance superclasses as well (if they occur there).
-}
subst :: Rule -> Pat -> Rule
subst cst@(C _ t :<= _) ty = mapPat (go (getSubs t ty)) cst
  where
    go :: [(Text, Pat)] -> Pat -> Pat
    go subs tty = foldl' (flip . uncurry $ subV) tty subs

{- Given two types (which are hopefully structurally similar), gather a list of all substitutions
   from the PatVars in the first argument to the concrete types (hopefully!) in the second argument
-}
getSubs :: Pat -> Pat -> [(Text, Pat)] -- should be a set, whatever
getSubs (VarP s) t = [(s, t)]
getSubs (List t) (List t') = getSubs t t'
getSubs (Maybe t) (Maybe t') = getSubs t t'
getSubs (x :* xs) (x' :* xs') = getSubs x x' <> getSubs xs xs'
getSubs (l := t) (l' := t') = getSubs l l' <> getSubs t t'
getSubs (Map k v) (Map k' v') = getSubs k k' <> getSubs v v'
getSubs (Either l r) (Either l' r') = getSubs l l' <> getSubs r r'
getSubs (ProdP xs) (ProdP xs') = getSubs xs xs'
getSubs (RecP xs) (RecP xs') = getSubs xs xs'
getSubs (SumP xs) (SumP xs') = getSubs xs xs'
getSubs (AppP t1 t2) (AppP t1' t2') = getSubs t1 t1' <> getSubs t2 t2'
getSubs (RefP t) (RefP t') = getSubs t t'
getSubs (DecP a b c) (DecP a' b' c') = getSubs a a' <> getSubs b b' <> getSubs c c'
getSubs _ _ = []

normalize :: Ord a => [a] -> [a]
normalize = S.toList . S.fromList

-- TODO: integrate the specificity stuff to make `solve` a total function
isSubstitutionOf :: Pat -> Pat -> Bool
isSubstitutionOf = flip matches

compareSpecificity :: Pat -> Pat -> Ordering
compareSpecificity p1 p2
  | p1
      `isSubstitutionOf` p2
      && p2
      `isSubstitutionOf` p1 =
      EQ
  | p1 `isSubstitutionOf` p2 = LT
  | otherwise = GT

sortOnSpecificity :: Pat -> [Pat] -> [Pat]
sortOnSpecificity p ps = sortBy compareSpecificity $ filter (`matches` p) ps

mostSpecificInstance :: Pat -> [Pat] -> Maybe Pat
mostSpecificInstance p ps = case sortOnSpecificity p ps of
  [] -> Nothing
  (x : _) -> Just x

{- Given a list of instances (the initial scope), determines whether we can derive
   an instance of the Class argument for the Pat argument. A result of [] indicates that there are
   no remaining subgoals and that the constraint has been solved.

   NOTE: At the moment this handles superclasses differently than you might expect -
         instead of assuming that the superclasses for all in-scope classes are defined,
         we check that those constraints can be solved before affirmatively judging that the
         target constraint has been solved. I *think* that makes sense in this context (whereas in Haskell
         it doesn't b/c it's *impossible* to have `instance Foo X` if the definition of Foo is
         `class Bar y => Foo y` without an `instance Bar X`)

   TODO: This needs *extensive* testing, which is somewhat complicated to implement.
-}
solve ::
  [Instance] -> -- all instances in scope. WE ASSUME THESE HAVE ALREADY BEEN GENERATED, SOMEWHERE
  Constraint ->
  [Constraint]
solve inScope cst@(C c pat) =
  case flip subst pat <$> matchHeads inScope of
    [] -> [cst]
    [C _ p :<= []] -> case supers c of
      [] -> []
      xs -> normalize . concat $ traverse (solve inScope . flip C p) xs
    [C _ _ :<= is] -> case normalize $ goConstraints is of
      [] -> normalize . concat $ traverse (solve inScope . flip C pat) (supers c)
      xs -> xs
    xs -> error ("Multiple matches, coherence broken: " <> show xs) -- TODO: Use specificity stuff to eliminate this branch
  where
    goConstraints :: [Constraint] -> [Constraint]
    goConstraints [] = []
    goConstraints (cx : rest) =
      let xs = solve inScope cx
       in xs <> goConstraints rest

    matchHeads :: [Instance] -> [Instance]
    matchHeads xs = flip filter xs $ \(C c' t' :<= _) -> c == c' && matches t' pat

{-
--output is too damn hard to read

-}

si = P.SourceInfo "" (P.SourcePosition 0 0) (P.SourcePosition 0 0)

matchInstance :: Constraint -> Instance -> Bool
matchInstance (C cc cp) (C c p :<= _) = cc == c && matches p cp

classA, classB, classC :: Class
classA = Class (CRef (P.ClassName "A" si) (P.ModuleName [] si)) []
classB = Class (CRef (P.ClassName "B" si) (P.ModuleName [] si)) [classA]
classC = Class (CRef (P.ClassName "C" si) (P.ModuleName [] si)) [classB]

mkManyStructuralRules :: [Class] -> [Instance]
mkManyStructuralRules = concatMap mkStructuralRules

mkStructuralRules :: Class -> [Instance]
mkStructuralRules c =
  [ C c Nil :<= [] -- I'm not sure whether this really has any meaning
  , C c (_x :* _xs) :<= [C c _x, C c _xs]
  , C c (_l := _x) :<= [C c _x]
  , C c (RecP _xs) :<= [C c _xs]
  , C c (ProdP _xs) :<= [C c _xs]
  , C c (SumP _xs) :<= [C c _xs]
  , C c (DecP _name _vars _body) :<= [C c _body]
  ]

data Bop = Bop

data Baz = Baz {bazInt :: Int, bazString :: String}

data Bar = Bar {barBaz :: Baz, barMString :: Maybe String, barBop :: Bop}

mkTyScope :: [P.TyDef] -> M.Map Text Pat
mkTyScope = flip foldl' M.empty $ \acc x ->
  M.insert (x ^. #tyName . #name) (defToPat' x) acc

testClsScope = mkManyStructuralRules [classA, classB, classC]

expandInstances ::
  M.Map Text Pat ->
  [Class] ->
  Constraint ->
  [Instance]
expandInstances tyScope clsScope targ =
  concatMap go $ solve structRules targ
  where
    structRules = mkManyStructuralRules clsScope

    go :: Constraint -> [Instance]
    go cst@(C cls (RefP (Name nm))) = case M.lookup nm tyScope of
      Nothing -> [cst :<= []] -- error $ "no type named " <> T.unpack nm <> " in scope"
      Just p -> expandInstances tyScope clsScope (C cls p)
    go other = case filter (/= other) $ solve structRules other of
      [] -> [other :<= []]
      more -> (other :<= []) : concatMap (expandInstances tyScope clsScope) more
