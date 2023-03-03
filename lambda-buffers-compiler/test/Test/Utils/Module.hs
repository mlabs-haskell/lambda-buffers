module Test.Utils.Module (
  module'maybe,
  module'incoherent,
  module'undefinedVar,
  module'undefinedLocalTyRef,
  module'undefinedForeignTyRef,
  module'either,
  module'recDef,
  module'newTypeEither,
  module'newTypeEither',
  module'newTypeEither'',
) where

import LambdaBuffers.Compiler.ProtoCompat qualified as P
import Test.Utils.Constructors (_Module, _ModuleName)
import Test.Utils.TyDef (
  tyDef'Int,
  tyDef'either,
  tyDef'either'opaque,
  tyDef'incoherent,
  tyDef'maybe,
  tyDef'ntEither,
  tyDef'ntEither'saturated,
  tyDef'recDef,
  tyDef'undefinedForeignTyRef,
  tyDef'undefinedLocalTyRef,
  tyDef'undefinedVar,
 )

-- _Module mn tds cds ins =

module'maybe :: P.Module
module'maybe = _Module (_ModuleName ["Prelude"]) [tyDef'maybe] mempty mempty

module'either :: P.Module
module'either = _Module (_ModuleName ["Prelude"]) [tyDef'either] mempty mempty

module'newTypeEither :: P.Module
module'newTypeEither = _Module (_ModuleName ["Prelude"]) [tyDef'either, tyDef'ntEither] mempty mempty

module'newTypeEither' :: P.Module
module'newTypeEither' = _Module (_ModuleName ["Prelude"]) [tyDef'either'opaque, tyDef'ntEither] mempty mempty

module'newTypeEither'' :: P.Module
module'newTypeEither'' = _Module (_ModuleName ["Prelude"]) [tyDef'either'opaque, tyDef'Int, tyDef'ntEither'saturated] mempty mempty

module'recDef :: P.Module
module'recDef = _Module (_ModuleName ["Prelude"]) [tyDef'recDef] mempty mempty

{- | 1 Module containing
  Maybe = ...

  and adding:
  B a = B Maybe

 Should fail as B a defaults to B :: Type -> Type and Maybe is inferred as
 Type -> Type. This is an inconsistency failure.
-}
module'incoherent :: P.Module
module'incoherent = _Module (_ModuleName ["Module"]) [tyDef'maybe, tyDef'incoherent] mempty mempty

{- | 1 Module containing
  Foo = Bar b

  Should fail as b is undefined.
-}
module'undefinedVar :: P.Module
module'undefinedVar = _Module (_ModuleName ["Module"]) [tyDef'undefinedVar] mempty mempty

{- | 1 Module containing
  Foo b = Bar Baz b
              ^^^
  Should fail as Baz is a local undefined Ty Ref.
-}
module'undefinedLocalTyRef :: P.Module
module'undefinedLocalTyRef = _Module (_ModuleName ["Module"]) [tyDef'undefinedLocalTyRef] mempty mempty

{- | 1 Module containing
  Foo b = Bar Module.Foreign.Baz b
              ^^^^^^^^^^^^^^^^^^
  Should fail as Baz is a foreign undefined Ty Ref.
-}
module'undefinedForeignTyRef :: P.Module
module'undefinedForeignTyRef = _Module (_ModuleName ["Module"]) [tyDef'undefinedForeignTyRef] mempty mempty
