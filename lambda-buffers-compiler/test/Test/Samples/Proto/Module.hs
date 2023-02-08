module Test.Samples.Proto.Module (module'maybe, module'incoherent) where

import Control.Lens ((%~), (&))
import LambdaBuffers.Compiler.ProtoCompat qualified as P
import Test.Samples.Proto.SourceInfo (sourceInfo'empty)
import Test.Samples.Proto.TyDef (tyDef'incoherent, tyDef'maybe)

module'maybe :: P.Module
module'maybe =
  P.Module
    { P.moduleName =
        P.ModuleName
          { P.parts = [P.ModuleNamePart "Module" sourceInfo'empty]
          , P.sourceInfo = sourceInfo'empty
          }
    , P.typeDefs = [tyDef'maybe]
    , P.classDefs = mempty
    , P.instances = mempty
    , P.sourceInfo = sourceInfo'empty
    , P.imports = mempty
    }

{- | 1 Module containing
  Maybe = ...

  and adding:
  B a = B Maybe

 Should fail as B a defaults to B :: Type -> Type and Maybe is inferred as
 Type -> Type. This is an inconsistency failure.
-}
module'incoherent :: P.Module
module'incoherent = module'maybe & #typeDefs %~ (<> [tyDef'incoherent])
