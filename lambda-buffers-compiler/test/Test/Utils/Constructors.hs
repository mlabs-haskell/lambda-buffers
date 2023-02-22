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
  _LocalRef,
  _LocalRef',
  _ForeignRef',
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
_VarName = flip _VarName' P.defSourceInfo

_VarName' :: Text -> P.SourceInfo -> P.VarName
_VarName' x s = P.VarName {P.name = x, P.sourceInfo = s}

_TyVar :: Text -> P.TyVar
_TyVar = P.TyVar . _VarName

-- | TyVar with Source Info - for error precision testing.
_TyVar' :: Text -> P.SourceInfo -> P.TyVar
_TyVar' x s = P.TyVar {P.varName = _VarName' x s}

_TyVarI :: Text -> P.Ty
_TyVarI = P.TyVarI . _TyVar

-- | TyVar with Source Info - for error precision testing.
_TyVarI' :: Text -> SourceInfo -> P.Ty
_TyVarI' x s = P.TyVarI $ P.TyVar {P.varName = _VarName' x s}

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
_TyRefILocal x = P.TyRefI $ P.LocalI $ _LocalRef x

_LocalRef :: Text -> P.LocalRef
_LocalRef = flip _LocalRef' P.defSourceInfo

-- | LocalRef with Source Info - for error precision testing.
_LocalRef' :: Text -> P.SourceInfo -> P.LocalRef
_LocalRef' x s =
  P.LocalRef
    { P.tyName = P.TyName {P.name = x, sourceInfo = s}
    , P.sourceInfo = s
    }

_ForeignRef :: Text -> [Text] -> P.ForeignRef
_ForeignRef n m = _ForeignRef' n (_ModuleName m) P.defSourceInfo

_ForeignRef' :: Text -> P.ModuleName -> P.SourceInfo -> P.ForeignRef
_ForeignRef' x m s =
  P.ForeignRef
    { P.tyName = P.TyName {P.name = x, sourceInfo = s}
    , P.moduleName = m
    , P.sourceInfo = s
    }
