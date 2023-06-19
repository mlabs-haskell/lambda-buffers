module Test.LambdaBuffers.Runtime.Prelude.Generators.Correct (genFooSum, genFooProd, genFooRec, genFooComplicated, genDay, genWeekDay, genWorkDay) where

import Hedgehog qualified as H
import Hedgehog.Gen qualified as H
import LambdaBuffers.Days (Day (Day'Friday, Day'Monday, Day'Saturday, Day'Sunday, Day'Thursday, Day'Tuesday, Day'Wednesday), WeekDay (WeekDay), WorkDay (WorkDay))
import LambdaBuffers.Foo.Bar (FooComplicated (FooComplicated), FooProd (FooProd), FooRec (FooRec), FooSum (FooSum'Bar, FooSum'Baz, FooSum'Faz, FooSum'Foo, FooSum'Qax))
import LambdaBuffers.Runtime.Prelude.Generators.Correct qualified as Lbr

genFooSum :: H.Gen a -> H.Gen b -> H.Gen c -> H.Gen (FooSum a b c)
genFooSum genx geny genz =
  H.choice
    [ FooSum'Foo <$> genx <*> geny <*> genz
    , FooSum'Bar <$> genx <*> geny
    , FooSum'Baz <$> geny
    , return FooSum'Qax
    , FooSum'Faz <$> Lbr.genInteger
    ]

genFooProd :: H.Gen a -> H.Gen b -> H.Gen c -> H.Gen (FooProd a b c)
genFooProd genx geny genz = FooProd <$> genx <*> geny <*> genz <*> Lbr.genInteger

genFooRec :: H.Gen a -> H.Gen b -> H.Gen c -> H.Gen (FooRec a b c)
genFooRec genx geny genz = FooRec <$> genx <*> geny <*> genz <*> Lbr.genInteger

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

genWeekDay :: H.Gen WeekDay
genWeekDay = WeekDay <$> genDay
