module LambdaBuffers.ProtoCompat.Types.Qualified (QTyName, QClassName) where

import LambdaBuffers.ProtoCompat.InfoLess (InfoLess)
import LambdaBuffers.ProtoCompat.Types.Lang (ClassName, ModuleName, TyName)

-- | Qualified type name mostly used in maintaining various contexts
type QTyName = (InfoLess ModuleName, InfoLess TyName)

-- | Qualified type class name mostly used in maintaining various contexts
type QClassName = (InfoLess ModuleName, InfoLess ClassName)
