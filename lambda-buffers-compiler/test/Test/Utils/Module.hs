module Test.Utils.Module (module'maybe, module'incoherent, module'undefinedVar, module'undefinedLocalTyRef, module'undefinedForeignTyRef) where

import LambdaBuffers.Compiler.ProtoCompat qualified as P
import Test.Utils.Constructors (_Module, _ModuleName)
import Test.Utils.TyDef (tyDef'incoherent, tyDef'maybe, tyDef'undefinedForeignTyRef, tyDef'undefinedLocalTyRef, tyDef'undefinedVar)

-- _Module mn tds cds ins =

module'maybe :: P.Module
module'maybe = _Module (_ModuleName ["Module"]) [tyDef'maybe] mempty mempty

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
