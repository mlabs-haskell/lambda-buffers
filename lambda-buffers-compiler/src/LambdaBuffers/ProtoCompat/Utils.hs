{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module LambdaBuffers.ProtoCompat.Utils (prettyModuleName, prettyModuleName', localRef2ForeignRef, classClosure, filterClassInModule) where

import Control.Lens (Getter, to, view, (&), (.~), (^.))
import Data.Map qualified as Map
import Data.ProtoLens (Message (defMessage))
import Data.Set (Set)
import Data.Set qualified as Set
import LambdaBuffers.ProtoCompat.Indexing qualified as PC
import LambdaBuffers.ProtoCompat.IsCompat.FromProto qualified as PC
import LambdaBuffers.ProtoCompat.IsCompat.Lang ()
import LambdaBuffers.ProtoCompat.Types qualified as PC
import Prettyprinter (Doc, Pretty (pretty), dot, encloseSep)
import Proto.Codegen qualified as Codegen
import Proto.Codegen_Fields qualified as Codegen
import Proto.Compiler qualified as Compiler
import Proto.Compiler_Fields qualified as Compiler
import Proto.Lang qualified as Lang
import Proto.Lang_Fields qualified as Lang

prettyModuleName :: PC.ModuleName -> Doc a
prettyModuleName mn = prettyModuleName' $ PC.toProto mn -- IsCompat instances are in PC.Lang

prettyModuleName' :: Lang.ModuleName -> Doc a
prettyModuleName' mn = encloseSep mempty mempty dot $ pretty . view Lang.name <$> mn ^. Lang.parts

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
      & Codegen.internalErrors .~ l ^. Codegen.internalErrors <> r ^. Codegen.internalErrors
      & Codegen.unsupportedOpaqueErrors .~ l ^. Codegen.unsupportedOpaqueErrors <> r ^. Codegen.unsupportedOpaqueErrors
      & Codegen.unsupportedClassErrors .~ l ^. Codegen.unsupportedClassErrors <> r ^. Codegen.unsupportedClassErrors

-- | Class closure is used by lbg when restricting implementation printing to some user specified classes. The users are able to specify which classes should be printed, for example: print Eq but don't print Json. This is particularly useful when some backends don't support a certain type class.

-- | `classClosure classRels initialClasses` computes the full class closure reachable from `initialClasses`.
classClosure :: PC.ClassRels -> Set PC.QClassName -> Set PC.QClassName
classClosure classRels cls =
  let classRels' = Map.filterWithKey (\k _x -> k `Set.member` cls) classRels
      cls' = cls <> (Set.fromList . mconcat . Map.elems $ classRels')
   in if cls == cls'
        then cls
        else classClosure classRels cls'

filterClassInModule :: Set PC.QClassName -> PC.Module -> PC.Module
filterClassInModule cls m =
  m
    { PC.classDefs = Map.filter (filterClassDef cls m) (m ^. #classDefs)
    , PC.instances = [i | i <- m ^. #instances, filterInstance cls m i]
    , PC.derives = [d | d <- m ^. #derives, filterDerive cls m d]
    }

filterClassDef :: Set PC.QClassName -> PC.Module -> PC.ClassDef -> Bool
filterClassDef cls m clDef = PC.qualifyClassName (m ^. #moduleName) (clDef ^. #className) `Set.member` cls

filterInstance :: Set PC.QClassName -> PC.Module -> PC.InstanceClause -> Bool
filterInstance cls m inst = filterConstraint cls m (inst ^. #head)

filterDerive :: Set PC.QClassName -> PC.Module -> PC.Derive -> Bool
filterDerive cls m drv = filterConstraint cls m (drv ^. #constraint)

filterConstraint :: Set PC.QClassName -> PC.Module -> PC.Constraint -> Bool
filterConstraint cls m cnstr = PC.qualifyClassRef (m ^. #moduleName) (cnstr ^. #classRef) `Set.member` cls
