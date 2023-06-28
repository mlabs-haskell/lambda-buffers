module Test.LambdaBuffers.Runtime.Plutus.Generators.Correct
  ( genFooSum
  , genFooProd
  , genFooRec
  , genFooComplicated
  , genDay
  , genFreeDay
  , genWorkDay
  , genA
  , genB
  , genC
  , genD
  ) where

import Prelude
import Control.Alternative ((<|>))
import Data.BigInt (BigInt)
import Data.BigInt as BigInt
import LambdaBuffers.Days (Day(Day'Friday, Day'Monday, Day'Saturday, Day'Sunday, Day'Thursday, Day'Tuesday, Day'Wednesday), FreeDay(FreeDay), WorkDay(WorkDay))
import LambdaBuffers.Foo (A(A), B(B), C(C), D(D))
import LambdaBuffers.Foo.Bar (FooComplicated(FooComplicated), FooProd(FooProd), FooRec(FooRec), FooSum(FooSum'Bar, FooSum'Baz, FooSum'Faz, FooSum'Foo, FooSum'Qax))
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
