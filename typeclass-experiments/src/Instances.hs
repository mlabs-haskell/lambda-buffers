{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Instances where

import Data.List.NonEmpty (NonEmpty (..))
import Data.Set (Set)
import Data.Set qualified as S
import Data.Text (Text)
import Data.Text qualified as T
import Solver
import SourceTy
import Types

-- reusable type variables to increase clarity

k, v, a, l, x, xs :: Pat
k = VarP "k"
v = VarP "v"
a = VarP "a"
l = VarP "l"
x = VarP "x"
xs = VarP "xs"

gen :: t -> Gen t
gen = Gen (pure . T.pack . show) []

rule :: t -> Pat -> Rule (Gen t)
rule = Rule . gen

eqInt :: RuleGen Text
eqInt = rule "Eq" IntP

eqList :: RuleGen Text
eqList = rule "Eq" (List a) :<= rule "Eq" a

noDeps :: t -> (Pat -> MatchResult) -> Gen t
noDeps t f = Gen f [] t

rustInt :: RuleGen Text
rustInt = Rule intGen IntP
  where
    intGen =
      Gen
        { runGen = const . pure $ "i64"
        , deps = []
        , genID = "rust"
        }

rustString :: RuleGen Text
rustString = Rule strGen StringP
  where
    strGen = noDeps "rust" (const . pure $ "String")

rustBool :: RuleGen Text
rustBool = Rule boolGen BoolP
  where
    boolGen = noDeps "rust" (const . pure $ "bool")

brackets :: Text -> Text
brackets t = "{\n" <> t <> "\n}"

indent :: Text -> Text
indent t = "  " <> t

rustRef :: RuleGen Text
rustRef = Rule refGen (RefP x)
  where
    refGen = noDeps "rust" $ \case
      RefP (Name t) -> pure t
      other -> Left $ MatchFailure other (RefP x)

goGen ::
  forall t.
  Eq t =>
  Set (RuleGen t) ->
  t ->
  Pat ->
  Either MatchFailure Text
goGen sc t p = case generate sc t p of
  Left _ -> error $ show p
  Right (_, z) -> pure z

rustRecBody :: Set (RuleGen Text) -> RuleGen Text
rustRecBody scope = Rule recGen (RecP $ (l := x) :* xs)
  where
    recGen = noDeps "rust" go
    go = \case
      RecP ((Name n := t) :* Nil) -> do
        t' <- goGen scope "rust" t
        pure $ indent $ n <> ": " <> t' <> ","
      RecP ((Name n := t) :* ys) -> do
        let scope' = S.insert (rustRecBody scope) scope
        t' <- goGen scope' "rust" t
        rest <- goGen scope' "rust" (RecP ys)
        pure $ indent $ n <> ": " <> t' <> "," <> "\n" <> rest
      other -> Left $ MatchFailure (RecP xs) other

rustStruct :: Set (RuleGen Text) -> RuleGen Text
rustStruct scope = Rule (noDeps "rust" structGen) (SumP $ (l := RecP xs) :* Nil)
  where
    structGen = \case
      (SumP ((Name nm := RecP flds) :* Nil)) -> do
        body <- goGen scope "rust" (RecP flds)
        pure $ "struct " <> nm <> " " <> brackets body <> "\n"
      other -> Left $ MatchFailure (SumP $ (l := RecP xs) :* Nil) other

rustMaybe' :: Set (RuleGen Text) -> RuleGen Text
rustMaybe' scope = Rule (noDeps "rust" maybeGen) (Maybe x)
  where
    maybeGen = \case
      Maybe z -> do
        let scope' = S.insert (rustMaybe' scope) scope
        z' <- goGen scope' "rust" z
        pure $ "Option<" <> z' <> ">"
      other -> Left $ MatchFailure (Maybe x) other

rustList' :: Set (RuleGen Text) -> RuleGen Text
rustList' scope = Rule (noDeps "rust" listGen) (List x)
  where
    listGen = \case
      List z -> do
        let scope' = S.insert (rustList' scope) scope
        z' <- goGen scope' "rust" z
        pure $ "Vec<" <> z' <> ">"
      other -> Left $ MatchFailure (Maybe x) other

prims :: Set (RuleGen Text)
prims = S.fromList [rustInt, rustString, rustBool, rustRef]

rustScope :: Set (RuleGen Text)
rustScope =
  let l1 = rustList' prims
      m1 = rustMaybe' prims

      s = S.fromList [l1, m1] <> prims

      l2 = rustList' s
      m2 = rustMaybe' s

      s' = S.fromList [l2, m2] <> prims

      s'' = S.fromList [rustList' s', rustMaybe' s'] <> prims

      s''' = S.insert (rustRecBody s'') s''

      s'''' = S.insert (rustStruct s''') s'''
   in s''''

testRec :: TyDef
testRec =
  TyDef
    { tyDefName = "TestStruct"
    , tyDefKind = TKind []
    , tyDefBody = Sum (("Test", myRec) :| [])
    }
  where
    myRec :: Product
    myRec =
      Record $
        ("boolField", PrimT $ TP0 TBool)
          :| [ ("intField", PrimT $ TP0 TInt)
             , ("maybeInt", PrimT (TP1 TMaybe) `TApp` PrimT (TP0 TInt))
             ]

{-
noSup :: String -> Class
noSup = flip Class []

showC :: Class
showC = noSup "Show"

intC :: Class -> Instance
intC = flip Inst IntT

stringC :: Class -> Instance
stringC = flip Inst StringT

boolC :: Class -> Instance
boolC = flip Inst BoolT

primCs :: Class -> [Instance]
primCs c = map ($ c) [intC, stringC, boolC]

showPrims :: [Instance]
showPrims = primCs showC

eqC :: Class
eqC = noSup "Eq"

eqPrims :: [Instance]
eqPrims = primCs eqC

ordC :: Class
ordC = Class "Ord" [eqC]

ordPrims :: [Instance]
ordPrims = primCs ordC

listSchemaC :: Class -> Instance
listSchemaC c = Inst c (List a) :<= Inst c a

mapSchemaC :: Class -> Instance
mapSchemaC c =
  Inst c (Map k v)
    :<= Inst c k
    :<= Inst c v

prodNilSchemaC :: Class -> Instance
prodNilSchemaC c = Inst c (ProdT Nil)

sumNilSchemaC :: Class -> Instance
sumNilSchemaC c = Inst c (SumT Nil)

prodSchemaC :: Class -> Instance
prodSchemaC c =
  Inst c (ProdT (x :* xs))
    :<= Inst c x
    :<= Inst c (ProdT xs)

sumSchemaC :: Class -> Instance
sumSchemaC c =
  Inst c (SumT (x :* xs))
    :<= Inst c x
    :<= Inst c (SumT xs)

-- for testing via superclass resolution

aC, bC, cC :: Class
aC = noSup "A"
bC = Class "B" [aC]
cC = Class "C" [bC]

testInstances :: [Instance]
testInstances =
  primCs aC
    <> primCs bC
    <> primCs cC
    <> [listSchemaC cC]

inScope :: [Instance]
inScope =
  showPrims
    <> eqPrims
    <> ordPrims
    <> [ listSchemaC eqC
       , listSchemaC showC
       , listSchemaC ordC
       , mapSchemaC eqC
       , mapSchemaC showC
       , mapSchemaC ordC
       , prodNilSchemaC eqC
       , prodNilSchemaC showC
       , prodNilSchemaC ordC
       , sumNilSchemaC eqC
       , sumNilSchemaC showC
       , sumNilSchemaC ordC
       , prodSchemaC eqC
       , prodSchemaC showC
       , prodSchemaC ordC
       , sumSchemaC eqC
       , sumSchemaC showC
       , sumSchemaC ordC
       ]

 set of test instances (inScope):

instance Show Int
instance Show String
instance Show Bool

instance Eq Int
instance Eq String
instance Eq Bool

instance Ord Int
instance Ord String
instance Ord Bool

instance Eq a => Eq [a]
instance Show a => Show [a]
instance Ord a => Ord [a]

instance (Eq k, Eq v) => Eq (Map k v)
instance (Show k, Show v) => Show (Map k v)
instance (Ord k, Ord v) => Ord (Map k v)

-}
