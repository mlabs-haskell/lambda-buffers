{-# LANGUAGE OverloadedStrings #-}

module Instances where

import Types

-- reusable type variables to increase clarity

k, v, a, x, xs :: Ty
k = VarT "k"
v = VarT "v"
a = VarT "a"
x = VarT "x"
xs = VarT "xs"

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

{- set of test instances (inScope):

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
