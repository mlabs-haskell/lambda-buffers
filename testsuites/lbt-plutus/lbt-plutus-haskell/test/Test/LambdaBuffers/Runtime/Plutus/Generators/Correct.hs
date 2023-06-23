module Test.LambdaBuffers.Runtime.Plutus.Generators.Correct (
  genFooSum,
  genFooProd,
  genFooRec,
  genFooComplicated,
  genDay,
  genFreeDay,
  genWorkDay,
  genA,
  genB,
  genC,
  genD,
) where

import Hedgehog qualified as H
import Hedgehog.Gen qualified as H
import Hedgehog.Range qualified as HR
import LambdaBuffers.Days (Day (Day'Friday, Day'Monday, Day'Saturday, Day'Sunday, Day'Thursday, Day'Tuesday, Day'Wednesday), FreeDay (FreeDay), WorkDay (WorkDay))
import LambdaBuffers.Foo (A (A), B (B), C (C), D (D))
import LambdaBuffers.Foo.Bar (FooComplicated (FooComplicated), FooProd (FooProd), FooRec (FooRec), FooSum (FooSum'Bar, FooSum'Baz, FooSum'Faz, FooSum'Foo, FooSum'Qax))
import Test.LambdaBuffers.Plutus.Generators.Correct qualified as Lbr

genA :: H.Gen A
genA = A <$> genFooSum Lbr.genAddress Lbr.genValue Lbr.genDatum

genB :: H.Gen B
genB = B <$> genFooProd Lbr.genAddress Lbr.genValue Lbr.genDatum

genC :: H.Gen C
genC = C <$> genFooRec Lbr.genAddress Lbr.genValue Lbr.genDatum

genD :: H.Gen D
genD = D <$> genFooComplicated Lbr.genAddress Lbr.genValue Lbr.genDatum

genInteger :: H.Gen Integer
genInteger = H.integral (HR.constant 0 10)

genFooSum :: H.Gen a -> H.Gen b -> H.Gen c -> H.Gen (FooSum a b c)
genFooSum genx geny genz =
  H.choice
    [ FooSum'Foo <$> genx <*> geny <*> genz
    , FooSum'Bar <$> genx <*> geny
    , FooSum'Baz <$> geny
    , return FooSum'Qax
    , FooSum'Faz <$> genInteger
    ]

genFooProd :: H.Gen a -> H.Gen b -> H.Gen c -> H.Gen (FooProd a b c)
genFooProd genx geny genz = FooProd <$> genx <*> geny <*> genz <*> genInteger

genFooRec :: H.Gen a -> H.Gen b -> H.Gen c -> H.Gen (FooRec a b c)
genFooRec genx geny genz = FooRec <$> genx <*> geny <*> genz <*> genInteger

genFooComplicated :: H.Gen a -> H.Gen b -> H.Gen c -> H.Gen (FooComplicated a b c)
genFooComplicated genx geny genz = FooComplicated <$> genFooSum genx geny genz <*> genFooProd genx geny genz <*> genFooRec genx geny genz

genDay :: H.Gen Day
genDay =
  H.choice $
    return
      <$> [ Day'Monday
          , Day'Tuesday
          , Day'Wednesday
          , Day'Thursday
          , Day'Friday
          , Day'Saturday
          , Day'Sunday
          ]

genWorkDay :: H.Gen WorkDay
genWorkDay = WorkDay <$> genDay

genFreeDay :: H.Gen FreeDay
genFreeDay = FreeDay <$> genDay