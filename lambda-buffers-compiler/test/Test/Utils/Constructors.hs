module Test.Utils.Constructors (
  _TyName,
  _VarName,
  _TyVar,
  _TyVar',
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
  _TyVarI',
  _SourceInfo,
) where

import Control.Lens ((^.))
import Data.Map qualified as Map
import Data.Text (Text)
import LambdaBuffers.Compiler.ProtoCompat qualified as P
import LambdaBuffers.Compiler.ProtoCompat.Types (SourceInfo)

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
    , P.sourceInfo = P.defSourceInfo
    }

_ModuleName :: [Text] -> P.ModuleName
_ModuleName ps =
  P.ModuleName
    { P.parts = _ModuleNamePart <$> ps
    , P.sourceInfo = P.defSourceInfo
    }

_ModuleNamePart :: Text -> P.ModuleNamePart
_ModuleNamePart n = P.ModuleNamePart n P.defSourceInfo

_TyName :: Text -> P.TyName
_TyName x = P.TyName x P.defSourceInfo

_VarName :: Text -> P.VarName
_VarName x = P.VarName {P.name = x, P.sourceInfo = P.defSourceInfo}

_TyVar :: Text -> P.TyVar
_TyVar x = P.TyVar {P.varName = _VarName x, P.sourceInfo = P.defSourceInfo}

-- | TyVar with Source Info - for error precision testing.
_TyVar' :: Text -> SourceInfo -> P.TyVar
_TyVar' x s = P.TyVar {P.varName = _VarName x, P.sourceInfo = s}

_TyVarI :: Text -> P.Ty
_TyVarI x = P.TyVarI $ P.TyVar {P.varName = _VarName x, P.sourceInfo = P.defSourceInfo}

-- | TyVar with Source Info - for error precision testing.
_TyVarI' :: Text -> SourceInfo -> P.Ty
_TyVarI' x s = P.TyVarI $ P.TyVar {P.varName = _VarName x, P.sourceInfo = s}

_SourceInfo :: Int -> Int -> P.SourceInfo
_SourceInfo x y = P.SourceInfo {P.file = "DefaultFile", P.posFrom = _SourcePosition x, P.posTo = _SourcePosition y}

_SourcePosition :: Int -> P.SourcePosition
_SourcePosition x = P.SourcePosition x (x + 1)

_TupleI :: [P.Ty] -> P.Product
_TupleI x =
  P.TupleI $
    P.Tuple
      { P.fields = x
      , P.sourceInfo = P.defSourceInfo
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
    , P.sourceInfo = P.defSourceInfo
    }

_Sum :: [(Text, P.Product)] -> P.TyBody
_Sum cs =
  P.SumI $
    P.Sum
      { constructors = Map.fromList [(ctor ^. #constrName, ctor) | (cn, cp) <- cs, ctor <- [_Constructor cn cp]]
      , sourceInfo = P.defSourceInfo
      }

_TyAbs :: [(Text, P.KindType)] -> [(Text, P.Product)] -> P.TyAbs
_TyAbs args body =
  P.TyAbs
    { P.tyArgs = Map.fromList [(ta ^. #argName, ta) | ta <- _TyArg <$> args]
    , P.tyBody = _Sum body
    , sourceInfo = P.defSourceInfo
    }

_TyArg :: (Text, P.KindType) -> P.TyArg
_TyArg (a, k) =
  P.TyArg
    { P.argName = P.VarName a P.defSourceInfo
    , P.argKind = P.Kind {P.kind = k}
    , P.sourceInfo = P.defSourceInfo
    }

_Type :: P.KindType
_Type = P.KindRef P.KType

_TyDef :: P.TyName -> P.TyAbs -> P.TyDef
_TyDef name ab = P.TyDef {P.tyName = name, P.tyAbs = ab, sourceInfo = P.defSourceInfo}

_TyRefILocal :: Text -> P.Ty
_TyRefILocal x =
  P.TyRefI $
    P.LocalI $
      P.LocalRef
        { P.tyName = P.TyName {P.name = x, sourceInfo = P.defSourceInfo}
        , P.sourceInfo = P.defSourceInfo
        }
