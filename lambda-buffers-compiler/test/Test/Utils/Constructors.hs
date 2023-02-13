module Test.Utils.Constructors (
  _tyName,
  _varName,
  _tyVar,
  _TyVarI,
  _TupleI,
  _Constructor,
  _ConstrName,
  _Sum,
  _TyAbs,
  _TyArg,
  _Type,
  _TyDef,
  _TyRefILocal,
  _Module,
  _CompilerInput,
  _ModuleName,
  _ModuleNamePart,
) where

import Control.Lens ((^.))
import Data.Map qualified as Map
import Data.Text (Text)
import LambdaBuffers.Compiler.ProtoCompat qualified as P
import Test.Utils.SourceInfo (sourceInfo'empty)

_CompilerInput :: [P.Module] -> P.CompilerInput
_CompilerInput ms =
  P.CompilerInput
    { P.modules = Map.fromList [(m ^. #moduleName, m) | m <- ms]
    }

_Module :: P.ModuleName -> [P.TyDef] -> [P.ClassDef] -> [P.InstanceClause] -> P.Module
_Module mn tds cds ins =
  P.Module
    { P.moduleName = mn
    , P.typeDefs = Map.fromList [(td ^. #tyName, td) | td <- tds]
    , P.classDefs = Map.fromList [(cd ^. #className, cd) | cd <- cds]
    , P.instances = ins
    , P.imports = mempty
    , P.sourceInfo = sourceInfo'empty
    }

_ModuleName :: [Text] -> P.ModuleName
_ModuleName ps =
  P.ModuleName
    { P.parts = _ModuleNamePart <$> ps
    , P.sourceInfo = sourceInfo'empty
    }

_ModuleNamePart :: Text -> P.ModuleNamePart
_ModuleNamePart n = P.ModuleNamePart n sourceInfo'empty

_tyName :: Text -> P.TyName
_tyName x = P.TyName x sourceInfo'empty

_varName :: Text -> P.VarName
_varName x = P.VarName {P.name = x, P.sourceInfo = sourceInfo'empty}

_tyVar :: Text -> P.TyVar
_tyVar x = P.TyVar {P.varName = _varName x, P.sourceInfo = sourceInfo'empty}

_TyVarI :: Text -> P.Ty
_TyVarI x = P.TyVarI $ P.TyVar {P.varName = _varName x, P.sourceInfo = sourceInfo'empty}

_TupleI :: [P.Ty] -> P.Product
_TupleI x =
  P.TupleI $
    P.Tuple
      { P.fields = x
      , P.sourceInfo = sourceInfo'empty
      }

_Constructor :: Text -> P.Product -> P.Constructor
_Constructor c f =
  P.Constructor
    { P.constrName = _ConstrName c
    , P.product = f
    }

_ConstrName :: Text -> P.ConstrName
_ConstrName x =
  P.ConstrName
    { P.name = x
    , P.sourceInfo = sourceInfo'empty
    }

_Sum :: [(Text, P.Product)] -> P.TyBody
_Sum cs =
  P.SumI $
    P.Sum
      { constructors = Map.fromList [(ctor ^. #constrName, ctor) | (cn, cp) <- cs, ctor <- [_Constructor cn cp]]
      , sourceInfo = sourceInfo'empty
      }

_TyAbs :: [(Text, P.KindType)] -> [(Text, P.Product)] -> P.TyAbs
_TyAbs args body =
  P.TyAbs
    { P.tyArgs = Map.fromList [(ta ^. #argName, ta) | ta <- _TyArg <$> args]
    , P.tyBody = _Sum body
    , sourceInfo = sourceInfo'empty
    }

_TyArg :: (Text, P.KindType) -> P.TyArg
_TyArg (a, k) =
  P.TyArg
    { P.argName = P.VarName a sourceInfo'empty
    , P.argKind = P.Kind {P.kind = k}
    , P.sourceInfo = sourceInfo'empty
    }

_Type :: P.KindType
_Type = P.KindRef P.KType

_TyDef :: P.TyName -> P.TyAbs -> P.TyDef
_TyDef name ab = P.TyDef {P.tyName = name, P.tyAbs = ab, sourceInfo = sourceInfo'empty}

_TyRefILocal :: Text -> P.Ty
_TyRefILocal x =
  P.TyRefI $
    P.LocalI $
      P.LocalRef
        { P.tyName = P.TyName {P.name = x, P.sourceInfo = sourceInfo'empty}
        , P.sourceInfo = sourceInfo'empty
        }
