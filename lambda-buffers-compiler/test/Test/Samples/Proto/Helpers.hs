module Test.Samples.Proto.Helpers where

import Data.List.NonEmpty (NonEmpty ((:|)), fromList)
import Data.Text (Text)
import LambdaBuffers.Compiler.ProtoCompat qualified as P
import Test.Samples.Proto.SourceInfo (sourceInfo'empty)

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

_Constructor :: P.ConstrName -> P.Product -> P.Constructor
_Constructor c f =
  P.Constructor
    { P.constrName = c
    , P.product = f
    }

_ConstrName :: Text -> P.ConstrName
_ConstrName x =
  P.ConstrName
    { P.name = x
    , P.sourceInfo = sourceInfo'empty
    }

_Sum :: [P.Constructor] -> P.TyBody
_Sum cs =
  P.SumI $
    P.Sum
      { constructors = fromList cs
      , sourceInfo = sourceInfo'empty
      }

_TyAbs :: [P.TyArg] -> P.TyBody -> P.TyAbs
_TyAbs args body =
  P.TyAbs
    { P.tyArgs = args
    , P.tyBody = body
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