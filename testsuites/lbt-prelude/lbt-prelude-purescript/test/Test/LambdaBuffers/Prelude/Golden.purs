module Test.LambdaBuffers.Prelude.Golden
  ( aGoldens
  , bGoldens
  , boolGoldens
  , charGoldens
  , bytesGoldens
  , cGoldens
  , dGoldens
  , dayGoldens
  , eitherGoldens
  , fooProdGoldens
  , fooRecGoldens
  , fooSumGoldens
  , freeDayGoldens
  , integerGoldens
  , listGoldens
  , mapGoldens
  , maybeGoldens
  , setGoldens
  , textGoldens
  , workDayGoldens
  ) where

import Prelude
import Data.BigInt (BigInt, pow)
import Data.BigInt as BigInt
import Data.Either (Either(..))
import Data.Enum (toEnumWithDefaults)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Set (Set)
import Data.Set as Set
import Data.String (CodePoint)
import Data.Tuple (Tuple(..))
import LambdaBuffers.Days (Day(..), FreeDay(FreeDay), WorkDay(WorkDay))
import LambdaBuffers.Foo (A(A), B(B), C(C), D(D))
import LambdaBuffers.Foo.Bar (FooComplicated(FooComplicated), FooProd(FooProd), FooRec(FooRec), FooSum(FooSum'Bar, FooSum'Baz, FooSum'Faz, FooSum'Foo, FooSum'Qax))
import LambdaBuffers.Runtime.Prelude (Bytes)
import LambdaBuffers.Runtime.Prelude as Bytes

bi :: Int -> BigInt
bi = BigInt.fromInt

someBytes :: Bytes
someBytes = Bytes.fromIntArray [ 115, 111, 109, 101, 32, 98, 121, 116, 101, 115 ]

fooSumGoldens :: forall a b c. a -> b -> c -> Array (FooSum a b c)
fooSumGoldens x y z =
  [ FooSum'Foo x y z
  , FooSum'Bar x y
  , FooSum'Baz y
  , FooSum'Qax
  , FooSum'Faz (bi 0)
  ]

aGoldens :: Array A
aGoldens = A <$> fooSumGoldens (bi 1337) false someBytes

fooProdGoldens :: forall a b c. a -> b -> c -> Array (FooProd a b c)
fooProdGoldens x y z = [ FooProd x y z (bi 1337) ]

bGoldens :: Array B
bGoldens = B <$> fooProdGoldens (bi 1337) false someBytes

fooRecGoldens :: forall a b c. a -> b -> c -> Array (FooRec a b c)
fooRecGoldens x y z = [ FooRec { fooA: x, fooB: y, fooC: z, fooInt: bi 1337 } ]

cGoldens :: Array C
cGoldens = C <$> fooRecGoldens (bi 1337) false someBytes

dGoldens :: Array D
dGoldens = do
  fooSum <- fooSumGoldens (bi 1337) false someBytes
  fooProd <- fooProdGoldens (bi 1337) false someBytes
  fooRec <- fooRecGoldens (bi 1337) false someBytes
  pure (D $ FooComplicated { sum: fooSum, prod: fooProd, rec: fooRec })

dayGoldens :: Array Day
dayGoldens = [ Day'Monday, Day'Tuesday, Day'Wednesday, Day'Thursday, Day'Friday, Day'Saturday, Day'Sunday ]

workDayGoldens :: Array WorkDay
workDayGoldens = WorkDay <$> [ Day'Monday, Day'Tuesday, Day'Wednesday, Day'Thursday, Day'Friday ]

freeDayGoldens :: Array FreeDay
freeDayGoldens = [ FreeDay { day: Day'Saturday }, FreeDay { day: Day'Sunday } ]

boolGoldens :: Array Boolean
boolGoldens = [ true, false ]

integerGoldens :: Array BigInt
integerGoldens =
  [ bi 0
  , bi 1
  , bi (-1)
  , pow (bi 2) (bi 32)
  , (bi (-1)) * pow (bi 2) (bi 32)
  , pow (bi 2) (bi 64)
  , (bi (-1)) * pow (bi 2) (bi 64)
  , pow (bi 2) (bi 128)
  , (bi (-1)) * pow (bi 2) (bi 128)
  ]

bytesGoldens :: Array Bytes
bytesGoldens = [ Bytes.fromIntArray [], Bytes.fromIntArray [ 0 ], someBytes ]

charGoldens :: Array CodePoint
charGoldens = toEnumWithDefaults bottom top <$> [ 0x0, 0xA, 0x1F643 ]

textGoldens :: Array String
textGoldens = [ "", "\n", "dražen popović" ]

maybeGoldens :: Array (Maybe Boolean)
maybeGoldens = [ Nothing, Just true, Just false ]

eitherGoldens :: Array (Either Boolean String)
eitherGoldens = [ Left true, Left false, Right "this is right" ]

listGoldens :: Array (Array Boolean)
listGoldens = [ [], [ true ], [ false ], [ true, true, false, false ] ]

setGoldens :: Array (Set Boolean)
setGoldens = [ Set.empty, Set.singleton true, Set.fromFoldable [ true, false ] ]

mapGoldens :: Array (Map Boolean Boolean)
mapGoldens = [ Map.empty, Map.singleton true true, Map.fromFoldable [ Tuple true true, Tuple false false ] ]
