module Test.LambdaBuffers.Runtime.Plutus.Generators.Correct
  ( genA
  , genB
  , genC
  , genD
  , genDay
  , genEither
  , genBool
  , genFInt
  , genFooComplicated
  , genFooProd
  , genFooRec
  , genFooSum
  , genFreeDay
  , genGInt
  , genList
  , genMaybe
  , genWorkDay
  ) where

import Prelude
import Control.Alternative ((<|>))
import JS.BigInt (BigInt)
import JS.BigInt as BigInt
import Data.Either (Either(Left, Right))
import Data.Maybe (Maybe(Nothing, Just))
import LambdaBuffers.Days (Day(Day'Friday, Day'Monday, Day'Saturday, Day'Sunday, Day'Thursday, Day'Tuesday, Day'Wednesday))
import LambdaBuffers.DayTypes (FreeDay(FreeDay), WorkDay(WorkDay))
import LambdaBuffers.Foo (A(A), B(B), C(C), D(D), FInt(..), GInt(..))
import LambdaBuffers.Foo.Bar (F(..), FooComplicated(FooComplicated), FooProd(FooProd), FooRec(FooRec), FooSum(FooSum'Bar, FooSum'Baz, FooSum'Faz, FooSum'Foo, FooSum'Qax), G(..))
import Test.LambdaBuffers.Plutus.Generators.Correct as Lbr
import Test.QuickCheck.Gen as Q

genA :: Q.Gen A
genA = A <$> genFooSum Lbr.genAddress Lbr.genValue Lbr.genDatum

genB :: Q.Gen B
genB = B <$> genFooProd Lbr.genAddress Lbr.genValue Lbr.genDatum

genC :: Q.Gen C
genC = C <$> genFooRec Lbr.genAddress Lbr.genValue Lbr.genDatum

genD :: Q.Gen D
genD = D <$> genFooComplicated Lbr.genAddress Lbr.genValue Lbr.genDatum

genFInt :: Q.Gen FInt
genFInt = pure (FInt F'Nil) <|> pure (FInt $ F'Rec G'Nil)

genGInt :: Q.Gen GInt
genGInt = pure (GInt G'Nil) <|> pure (GInt $ G'Rec F'Nil)

genInteger :: Q.Gen BigInt
genInteger = BigInt.fromInt <$> Q.chooseInt 0 10

genFooSum :: forall a b c. Q.Gen a -> Q.Gen b -> Q.Gen c -> Q.Gen (FooSum a b c)
genFooSum genx geny genz =
  FooSum'Foo <$> genx <*> geny <*> genz
    <|> FooSum'Bar
    <$> genx
    <*> geny
    <|> FooSum'Baz
    <$> geny
    <|> pure FooSum'Qax
    <|> FooSum'Faz
    <$> genInteger

genFooProd :: forall a b c. Q.Gen a -> Q.Gen b -> Q.Gen c -> Q.Gen (FooProd a b c)
genFooProd genx geny genz = FooProd <$> genx <*> geny <*> genz <*> genInteger

genFooRec :: forall a b c. Q.Gen a -> Q.Gen b -> Q.Gen c -> Q.Gen (FooRec a b c)
genFooRec genx geny genz = FooRec <$> ({ fooA: _, fooB: _, fooC: _, fooInt: _ } <$> genx <*> geny <*> genz <*> genInteger)

genFooComplicated :: forall a b c. Q.Gen a -> Q.Gen b -> Q.Gen c -> Q.Gen (FooComplicated a b c)
genFooComplicated genx geny genz = FooComplicated <$> ({ sum: _, prod: _, rec: _ } <$> genFooSum genx geny genz <*> genFooProd genx geny genz <*> genFooRec genx geny genz)

genDay :: Q.Gen Day
genDay =
  pure Day'Monday
    <|> pure Day'Tuesday
    <|> pure Day'Wednesday
    <|> pure Day'Thursday
    <|> pure Day'Friday
    <|> pure Day'Saturday
    <|> pure Day'Sunday

genWorkDay :: Q.Gen WorkDay
genWorkDay = WorkDay <$> genDay

genFreeDay :: Q.Gen FreeDay
genFreeDay = FreeDay <$> ({ day: _ } <$> genDay)

genBool :: Q.Gen Boolean
genBool =
  pure false
    <|> pure true

genMaybe :: Q.Gen (Maybe Boolean)
genMaybe =
  pure Nothing
    <|> pure (Just true)

genEither :: Q.Gen (Either Boolean Boolean)
genEither =
  pure (Left false)
    <|> pure (Right true)

genList :: Q.Gen (Array Boolean)
genList =
  pure []
    <|> pure [ true, false ]
