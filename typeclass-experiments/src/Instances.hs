module Instances where

import Types

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
listSchemaC c = Inst c (ListT $ VarT "a") :<= Inst c (VarT "a")

mapSchemaC :: Class -> Instance
mapSchemaC c =
  Inst c (MapT (VarT "k") (VarT "v"))
    :<= Inst c (VarT "k")
    :<= Inst c (VarT "v")

prodNilSchemaC :: Class -> Instance
prodNilSchemaC c = Inst c (ProdT Nil)

sumNilSchemaC :: Class -> Instance
sumNilSchemaC c = Inst c (SumT Nil)

prodSchemaC :: Class -> Instance
prodSchemaC c =
  Inst c (ProdT (VarT "x" :* VarT "xs"))
    :<= Inst c (VarT "x")
    :<= Inst c (ProdT (VarT "xs"))

sumSchemaC :: Class -> Instance
sumSchemaC c =
  Inst c (SumT (VarT "x" :* VarT "xs"))
    :<= Inst c (VarT "x")
    :<= Inst c (SumT (VarT "xs"))

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
