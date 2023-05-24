{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module LambdaBuffers.ProtoCompat.Utils (prettyModuleName, localRef2ForeignRef) where

import Control.Lens (Getter, to, view, (&), (.~), (^.))
import Data.ProtoLens (Message (defMessage))
import LambdaBuffers.ProtoCompat.Types qualified as PC
import Prettyprinter (Doc, Pretty (pretty), dot, encloseSep)
import Proto.Codegen qualified as Codegen
import Proto.Compiler qualified as Compiler
import Proto.Compiler_Fields qualified as Compiler

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

instance Monoid Compiler.Error where
  mempty = defMessage

instance Semigroup Compiler.Error where
  l <> r =
    defMessage
      & Compiler.protoParseErrors .~ l ^. Compiler.protoParseErrors <> r ^. Compiler.protoParseErrors
      & Compiler.namingErrors .~ l ^. Compiler.namingErrors <> r ^. Compiler.namingErrors
      & Compiler.kindCheckErrors .~ l ^. Compiler.kindCheckErrors <> r ^. Compiler.kindCheckErrors
      & Compiler.tyClassCheckErrors .~ l ^. Compiler.tyClassCheckErrors <> r ^. Compiler.tyClassCheckErrors
      & Compiler.internalErrors .~ l ^. Compiler.internalErrors <> r ^. Compiler.internalErrors

instance Monoid Codegen.Error where
  mempty = defMessage

instance Semigroup Codegen.Error where
  l <> r =
    defMessage
      & Compiler.internalErrors .~ l ^. Compiler.internalErrors <> r ^. Compiler.internalErrors
