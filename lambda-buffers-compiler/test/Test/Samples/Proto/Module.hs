module Test.Samples.Proto.Module (module'maybe, module'incoherent, _Module) where

import Control.Lens ((%~), (&))
import LambdaBuffers.Compiler.ProtoCompat qualified as P
import Test.Samples.Proto.SourceInfo (sourceInfo'empty)
import Test.Samples.Proto.TyDef (tyDef'incoherent, tyDef'maybe)

_Module :: P.ModuleName -> [P.TyDef] -> [P.ClassDef] -> [P.InstanceClause] -> P.Module
_Module mn tds cds ins =
  P.Module
    { P.moduleName = mn
    , P.typeDefs = tds
    , P.classDefs = cds
    , P.instances = ins
    , P.sourceInfo = sourceInfo'empty
    }

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
