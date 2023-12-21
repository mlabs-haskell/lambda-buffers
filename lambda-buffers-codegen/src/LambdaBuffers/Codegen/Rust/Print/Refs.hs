module LambdaBuffers.Codegen.Rust.Print.Refs (
  caseIntERef,
  bigInt,
  vecAsSlice,
  vecMacro,
  boxNew,
  phantomData,
  debugTrait,
  fromU32Trait,
  fromStrTrait,
  cloneTrait,
  partialEqTrait,
  eqTrait,
  isPlutusDataTrait,
  jsonTrait,
  toOwnedTrait,
)
where

import LambdaBuffers.Codegen.Rust.Print.Syntax qualified as R

caseIntERef :: R.QValName
caseIntERef = R.qForeignRef R.MkValueName "lbr-prelude" ["lamval"] "case_int"

bigInt :: R.QValName
bigInt = R.qForeignRef R.MkValueName "num-bigint" [] "BigInt"

vecAsSlice :: R.QValName
vecAsSlice = R.qForeignRef R.MkValueName "std" ["vec", "Vec"] "as_slice"

vecMacro :: R.QValName
vecMacro = R.qForeignRef R.MkValueName "std" [] "vec!"

boxNew :: R.QValName
boxNew = R.qForeignRef R.MkValueName "std" ["boxed", "Box"] "new"

phantomData :: R.QTyName
phantomData = R.qForeignRef R.MkTyName "std" ["marker"] "PhantomData"

debugTrait :: R.QTraitName
debugTrait = R.qForeignRef R.MkTraitName "std" ["fmt"] "Debug"

fromU32Trait :: R.QTraitName
fromU32Trait = R.qForeignRef R.MkTraitName "std" ["convert"] "From<u32>"

fromStrTrait :: R.QTraitName
fromStrTrait = R.qForeignRef R.MkTraitName "std" ["convert"] "From<&str>"

cloneTrait :: R.QTraitName
cloneTrait = R.qForeignRef R.MkTraitName "std" ["clone"] "Clone"

partialEqTrait :: R.QTraitName
partialEqTrait = R.qForeignRef R.MkTraitName "std" ["cmp"] "PartialEq"

eqTrait :: R.QTraitName
eqTrait = R.qForeignRef R.MkTraitName "std" ["cmp"] "Eq"

isPlutusDataTrait :: R.QTraitName
isPlutusDataTrait = R.qForeignRef R.MkTraitName "plutus-ledger-api" ["plutus_data"] "IsPlutusData"

jsonTrait :: R.QTraitName
jsonTrait = R.qForeignRef R.MkTraitName "lbr-prelude" ["json"] "Json"

toOwnedTrait :: R.QTraitName
toOwnedTrait = R.qForeignRef R.MkTraitName "std" ["borrow"] "ToOwned"
