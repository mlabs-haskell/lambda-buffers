module Test.Utils.Module (
  module'maybe,
  module'incoherent,
  module'undefinedVar,
  module'undefinedLocalTyRef,
  module'undefinedForeignTyRef,
  module'either,
  module'recDef,
  module'classEq,
  module'unboundEq,
  module'classOrd,
  module'IntEqInstance,
  module'Int,
) where

import LambdaBuffers.Compiler.ProtoCompat qualified as PC
import Test.Utils.Constructors (_Module, _ModuleName)
import Test.Utils.TyDef (
  tyDef'Int,
  tyDef'either,
  tyDef'incoherent,
  tyDef'maybe,
  tyDef'recDef,
  tyDef'undefinedForeignTyRef,
  tyDef'undefinedLocalTyRef,
  tyDef'undefinedVar,
 )

import Test.Utils.ClassDef (classDef'Eq, classDef'Ord, classInstance'IntEq)

-- _Module mn tds cds ins =

module'maybe :: PC.Module
module'maybe = _Module (_ModuleName ["Prelude"]) [tyDef'maybe] mempty mempty

module'either :: PC.Module
module'either = _Module (_ModuleName ["Prelude"]) [tyDef'either] mempty mempty

module'recDef :: PC.Module
module'recDef = _Module (_ModuleName ["Prelude"]) [tyDef'recDef] mempty mempty

{- | 1 Module containing
  Maybe = ...

  and adding:
  B a = B Maybe

 Should fail as B a defaults to B :: Type -> Type and Maybe is inferred as
 Type -> Type. This is an inconsistency failure.
-}
module'incoherent :: PC.Module
module'incoherent = _Module (_ModuleName ["Module"]) [tyDef'maybe, tyDef'incoherent] mempty mempty

{- | 1 Module containing
  Foo = Bar b

  Should fail as b is undefined.
-}
module'undefinedVar :: PC.Module
module'undefinedVar = _Module (_ModuleName ["Module"]) [tyDef'undefinedVar] mempty mempty

{- | 1 Module containing
  Foo b = Bar Baz b
              ^^^
  Should fail as Baz is a local undefined Ty Ref.
-}
module'undefinedLocalTyRef :: PC.Module
module'undefinedLocalTyRef = _Module (_ModuleName ["Module"]) [tyDef'undefinedLocalTyRef] mempty mempty

{- | 1 Module containing
  Foo b = Bar Module.Foreign.Baz b
              ^^^^^^^^^^^^^^^^^^
  Should fail as Baz is a foreign undefined Ty Ref.
-}
module'undefinedForeignTyRef :: PC.Module
module'undefinedForeignTyRef = _Module (_ModuleName ["Module"]) [tyDef'undefinedForeignTyRef] mempty mempty

{- | 1 Module containing:
  class Eq a.
-}
module'classEq :: PC.Module
module'classEq = _Module (_ModuleName ["Module"]) mempty [classDef'Eq] mempty

{- | 1 Module containing:
  class Eq a.
  class Eq a => Ord a.
-}
module'classOrd :: PC.Module
module'classOrd = _Module (_ModuleName ["Module"]) mempty [classDef'Eq, classDef'Ord] mempty

module'unboundEq :: PC.Module
module'unboundEq = _Module (_ModuleName ["Module"]) mempty [classDef'Ord] mempty

module'Int :: PC.Module
module'Int = _Module (_ModuleName ["Module"]) [] mempty mempty

{- | 1 Module containing:
  instance Eq Int.
-}
module'IntEqInstance :: PC.Module
module'IntEqInstance = _Module (_ModuleName ["Module"]) [tyDef'Int] [classDef'Eq] [classInstance'IntEq]
