module LambdaBuffers.Compiler.ProtoCompat.Utils (prettyModuleName) where

import Control.Lens (view, (^.))
import LambdaBuffers.Compiler.ProtoCompat.Types qualified as PC
import Prettyprinter (Doc, Pretty (pretty), dot, encloseSep)

prettyModuleName :: PC.ModuleName -> Doc a
prettyModuleName mn = encloseSep mempty mempty dot $ pretty . view #name <$> mn ^. #parts
