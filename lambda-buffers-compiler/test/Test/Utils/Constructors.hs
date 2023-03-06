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
  _TyApp,

  -- * Class related.
  _ClassDef,
  _ClassName,
  _LocalClassRef,
  _ForeignCI,
  _Constraint,
) where

import Control.Lens ((^.))
import Data.Default (def)
import Data.Map qualified as Map
import Data.Text (Text)
import LambdaBuffers.Compiler.ProtoCompat qualified as PC
import LambdaBuffers.Compiler.ProtoCompat.Types (SourceInfo)
import Proto.Compiler_Fields ()

_CompilerInput :: [PC.Module] -> PC.CompilerInput
_CompilerInput ms =
  PC.CompilerInput {PC.modules = Map.fromList [(m ^. #moduleName, m) | m <- ms]}

_Module :: PC.ModuleName -> [PC.TyDef] -> [PC.ClassDef] -> [PC.InstanceClause] -> PC.Module
_Module mn tds cds ins =
  PC.Module
    { PC.moduleName = mn
    , PC.typeDefs = Map.fromList [(td ^. #tyName, td) | td <- tds]
    , PC.classDefs = Map.fromList [(cd ^. #className, cd) | cd <- cds]
    , PC.instances = ins
    , PC.imports = mempty
    , PC.sourceInfo = PC.defSourceInfo
    }

_ModuleName :: [Text] -> PC.ModuleName
_ModuleName ps =
  PC.ModuleName
    { PC.parts = _ModuleNamePart <$> ps
    , PC.sourceInfo = PC.defSourceInfo
    }

_ModuleNamePart :: Text -> PC.ModuleNamePart
_ModuleNamePart n = PC.ModuleNamePart n PC.defSourceInfo

_TyName :: Text -> PC.TyName
_TyName x = PC.TyName x PC.defSourceInfo

_VarName :: Text -> PC.VarName
_VarName = flip _VarName' PC.defSourceInfo

_VarName' :: Text -> PC.SourceInfo -> PC.VarName
_VarName' x s = PC.VarName {PC.name = x, PC.sourceInfo = s}

_TyVar :: Text -> PC.TyVar
_TyVar = PC.TyVar . _VarName

-- | TyVar with Source Info - for error precision testing.
_TyVar' :: Text -> PC.SourceInfo -> PC.TyVar
_TyVar' x s = PC.TyVar {PC.varName = _VarName' x s}

_TyVarI :: Text -> PC.Ty
_TyVarI = PC.TyVarI . _TyVar

-- | TyVar with Source Info - for error precision testing.
_TyVarI' :: Text -> SourceInfo -> PC.Ty
_TyVarI' x s = PC.TyVarI $ PC.TyVar {PC.varName = _VarName' x s}

_SourceInfo :: Int -> Int -> PC.SourceInfo
_SourceInfo x y = PC.SourceInfo {PC.file = "DefaultFile", PC.posFrom = _SourcePosition x, PC.posTo = _SourcePosition y}

_SourcePosition :: Int -> PC.SourcePosition
_SourcePosition x = PC.SourcePosition x (x + 1)

_TupleI :: [PC.Ty] -> PC.Product
_TupleI x =
  PC.TupleI $
    PC.Tuple
      { PC.fields = x
      , PC.sourceInfo = PC.defSourceInfo
      }

_Constructor :: Text -> PC.Product -> PC.Constructor
_Constructor c f =
  PC.Constructor
    { PC.constrName = _ConstrName c
    , PC.product = f
    }

_ConstrName :: Text -> PC.ConstrName
_ConstrName x =
  PC.ConstrName
    { PC.name = x
    , PC.sourceInfo = PC.defSourceInfo
    }

_Sum :: [(Text, PC.Product)] -> PC.TyBody
_Sum cs =
  PC.SumI $
    PC.Sum
      { constructors = Map.fromList [(ctor ^. #constrName, ctor) | (cn, cp) <- cs, ctor <- [_Constructor cn cp]]
      , sourceInfo = PC.defSourceInfo
      }

_TyApp :: PC.Ty -> PC.Ty -> PC.Ty
_TyApp ty1 ty2 =
  PC.TyAppI $
    PC.TyApp
      { PC.tyFunc = ty1
      , PC.tyArgs = [ty2]
      , PC.sourceInfo = def
      }

_TyAbs :: [(Text, PC.KindType)] -> [(Text, PC.Product)] -> PC.TyAbs
_TyAbs args body =
  PC.TyAbs
    { PC.tyArgs = Map.fromList [(ta ^. #argName, ta) | ta <- _TyArg <$> args]
    , PC.tyBody = _Sum body
    , sourceInfo = PC.defSourceInfo
    }

_TyArg :: (Text, PC.KindType) -> PC.TyArg
_TyArg (a, k) =
  PC.TyArg
    { PC.argName = PC.VarName a PC.defSourceInfo
    , PC.argKind = PC.Kind {PC.kind = k}
    , PC.sourceInfo = PC.defSourceInfo
    }

_Type :: PC.KindType
_Type = PC.KindRef PC.KType

_TyDef :: PC.TyName -> PC.TyAbs -> PC.TyDef
_TyDef name ab = PC.TyDef {PC.tyName = name, PC.tyAbs = ab, sourceInfo = PC.defSourceInfo}

_TyRefILocal :: Text -> PC.Ty
_TyRefILocal x = PC.TyRefI $ PC.LocalI $ _LocalRef x

_LocalRef :: Text -> PC.LocalRef
_LocalRef = flip _LocalRef' PC.defSourceInfo

-- | LocalRef with Source Info - for error precision testing.
_LocalRef' :: Text -> PC.SourceInfo -> PC.LocalRef
_LocalRef' x s =
  PC.LocalRef
    { PC.tyName = PC.TyName {PC.name = x, sourceInfo = s}
    , PC.sourceInfo = s
    }

_ForeignRef :: Text -> [Text] -> PC.ForeignRef
_ForeignRef n m = _ForeignRef' n (_ModuleName m) PC.defSourceInfo

_ForeignRef' :: Text -> PC.ModuleName -> PC.SourceInfo -> PC.ForeignRef
_ForeignRef' x m s =
  PC.ForeignRef
    { PC.tyName = PC.TyName {PC.name = x, sourceInfo = s}
    , PC.moduleName = m
    , PC.sourceInfo = s
    }

--------------------------------------------------------------------------------
-- Class related.

_ClassDef :: Text -> (Text, PC.KindType) -> [PC.Constraint] -> PC.ClassDef
_ClassDef n tyArg cs =
  PC.ClassDef
    { className = _ClassName n
    , classArgs = _TyArg tyArg
    , supers = cs
    , documentation = mempty
    , sourceInfo = def
    }

_ClassName :: Text -> PC.ClassName
_ClassName n = PC.ClassName {name = n, sourceInfo = def}

_Constraint :: PC.TyClassRef -> PC.Ty -> PC.Constraint
_Constraint cr t =
  PC.Constraint
    { classRef = cr
    , argument = t
    , sourceInfo = def
    }

_LocalClassRef :: Text -> PC.TyClassRef
_LocalClassRef n =
  PC.LocalCI $ PC.LocalClassRef {className = _ClassName n, sourceInfo = def}

_ForeignCI :: Text -> PC.ModuleName -> PC.TyClassRef
_ForeignCI n mn =
  PC.ForeignCI $
    PC.ForeignClassRef
      { className = _ClassName n
      , moduleName = mn
      , sourceInfo = def
      }
