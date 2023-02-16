module Test.Utils.Module (module'maybe, module'incoherent, module'undefinedVar) where

import LambdaBuffers.Compiler.ProtoCompat qualified as P
import Test.Utils.Constructors (_Module, _ModuleName)
import Test.Utils.TyDef (tyDef'incoherent, tyDef'maybe, tyDef'undefinedVar)

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
