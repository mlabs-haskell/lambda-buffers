{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module LambdaBuffers.ProtoCompat.Utils (prettyModuleName, localRef2ForeignRef) where

import Control.Lens (Getter, to, view, (^.))
import LambdaBuffers.ProtoCompat.Types qualified as PC
import Prettyprinter (Doc, Pretty (pretty), dot, encloseSep)

prettyModuleName :: PC.ModuleName -> Doc a
prettyModuleName mn = encloseSep mempty mempty dot $ pretty . view #name <$> mn ^. #parts

localRef2ForeignRef :: PC.ModuleName -> Getter PC.LocalRef PC.ForeignRef
localRef2ForeignRef modName =
  to
    ( \lr ->
        PC.ForeignRef
          { tyName = lr ^. #tyName
          , sourceInfo = lr ^. #sourceInfo
          , moduleName = modName
          }
    )
