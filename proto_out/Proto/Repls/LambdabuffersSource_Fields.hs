{- This file was auto-generated from repls/lambdabuffers-source.proto by the proto-lens-protoc program. -}
{-# LANGUAGE ScopedTypeVariables, DataKinds, TypeFamilies, UndecidableInstances, GeneralizedNewtypeDeriving, MultiParamTypeClasses, FlexibleContexts, FlexibleInstances, PatternSynonyms, MagicHash, NoImplicitPrelude, DataKinds, BangPatterns, TypeApplications, OverloadedStrings, DerivingStrategies#-}
{-# OPTIONS_GHC -Wno-unused-imports#-}
{-# OPTIONS_GHC -Wno-duplicate-exports#-}
{-# OPTIONS_GHC -Wno-dodgy-exports#-}
module Proto.Repls.LambdabuffersSource_Fields where
import qualified Data.ProtoLens.Runtime.Prelude as Prelude
import qualified Data.ProtoLens.Runtime.Data.Int as Data.Int
import qualified Data.ProtoLens.Runtime.Data.Monoid as Data.Monoid
import qualified Data.ProtoLens.Runtime.Data.Word as Data.Word
import qualified Data.ProtoLens.Runtime.Data.ProtoLens as Data.ProtoLens
import qualified Data.ProtoLens.Runtime.Data.ProtoLens.Encoding.Bytes as Data.ProtoLens.Encoding.Bytes
import qualified Data.ProtoLens.Runtime.Data.ProtoLens.Encoding.Growing as Data.ProtoLens.Encoding.Growing
import qualified Data.ProtoLens.Runtime.Data.ProtoLens.Encoding.Parser.Unsafe as Data.ProtoLens.Encoding.Parser.Unsafe
import qualified Data.ProtoLens.Runtime.Data.ProtoLens.Encoding.Wire as Data.ProtoLens.Encoding.Wire
import qualified Data.ProtoLens.Runtime.Data.ProtoLens.Field as Data.ProtoLens.Field
import qualified Data.ProtoLens.Runtime.Data.ProtoLens.Message.Enum as Data.ProtoLens.Message.Enum
import qualified Data.ProtoLens.Runtime.Data.ProtoLens.Service.Types as Data.ProtoLens.Service.Types
import qualified Data.ProtoLens.Runtime.Lens.Family2 as Lens.Family2
import qualified Data.ProtoLens.Runtime.Lens.Family2.Unchecked as Lens.Family2.Unchecked
import qualified Data.ProtoLens.Runtime.Data.Text as Data.Text
import qualified Data.ProtoLens.Runtime.Data.Map as Data.Map
import qualified Data.ProtoLens.Runtime.Data.ByteString as Data.ByteString
import qualified Data.ProtoLens.Runtime.Data.ByteString.Char8 as Data.ByteString.Char8
import qualified Data.ProtoLens.Runtime.Data.Text.Encoding as Data.Text.Encoding
import qualified Data.ProtoLens.Runtime.Data.Vector as Data.Vector
import qualified Data.ProtoLens.Runtime.Data.Vector.Generic as Data.Vector.Generic
import qualified Data.ProtoLens.Runtime.Data.Vector.Unboxed as Data.Vector.Unboxed
import qualified Data.ProtoLens.Runtime.Text.Read as Text.Read
arguments ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "arguments" a) =>
  Lens.Family2.LensLike' f s a
arguments = Data.ProtoLens.Field.field @"arguments"
column ::
  forall f s a.
  (Prelude.Functor f, Data.ProtoLens.Field.HasField s "column" a) =>
  Lens.Family2.LensLike' f s a
column = Data.ProtoLens.Field.field @"column"
constraints ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "constraints" a) =>
  Lens.Family2.LensLike' f s a
constraints = Data.ProtoLens.Field.field @"constraints"
constructors ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "constructors" a) =>
  Lens.Family2.LensLike' f s a
constructors = Data.ProtoLens.Field.field @"constructors"
documentation ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "documentation" a) =>
  Lens.Family2.LensLike' f s a
documentation = Data.ProtoLens.Field.field @"documentation"
empty ::
  forall f s a.
  (Prelude.Functor f, Data.ProtoLens.Field.HasField s "empty" a) =>
  Lens.Family2.LensLike' f s a
empty = Data.ProtoLens.Field.field @"empty"
fields ::
  forall f s a.
  (Prelude.Functor f, Data.ProtoLens.Field.HasField s "fields" a) =>
  Lens.Family2.LensLike' f s a
fields = Data.ProtoLens.Field.field @"fields"
file ::
  forall f s a.
  (Prelude.Functor f, Data.ProtoLens.Field.HasField s "file" a) =>
  Lens.Family2.LensLike' f s a
file = Data.ProtoLens.Field.field @"file"
foreignTypeRef ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "foreignTypeRef" a) =>
  Lens.Family2.LensLike' f s a
foreignTypeRef = Data.ProtoLens.Field.field @"foreignTypeRef"
head ::
  forall f s a.
  (Prelude.Functor f, Data.ProtoLens.Field.HasField s "head" a) =>
  Lens.Family2.LensLike' f s a
head = Data.ProtoLens.Field.field @"head"
instances ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "instances" a) =>
  Lens.Family2.LensLike' f s a
instances = Data.ProtoLens.Field.field @"instances"
key ::
  forall f s a.
  (Prelude.Functor f, Data.ProtoLens.Field.HasField s "key" a) =>
  Lens.Family2.LensLike' f s a
key = Data.ProtoLens.Field.field @"key"
kind ::
  forall f s a.
  (Prelude.Functor f, Data.ProtoLens.Field.HasField s "kind" a) =>
  Lens.Family2.LensLike' f s a
kind = Data.ProtoLens.Field.field @"kind"
localTypeRef ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "localTypeRef" a) =>
  Lens.Family2.LensLike' f s a
localTypeRef = Data.ProtoLens.Field.field @"localTypeRef"
maybe'body ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'body" a) =>
  Lens.Family2.LensLike' f s a
maybe'body = Data.ProtoLens.Field.field @"maybe'body"
maybe'empty ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'empty" a) =>
  Lens.Family2.LensLike' f s a
maybe'empty = Data.ProtoLens.Field.field @"maybe'empty"
maybe'foreignTypeRef ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'foreignTypeRef" a) =>
  Lens.Family2.LensLike' f s a
maybe'foreignTypeRef
  = Data.ProtoLens.Field.field @"maybe'foreignTypeRef"
maybe'head ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'head" a) =>
  Lens.Family2.LensLike' f s a
maybe'head = Data.ProtoLens.Field.field @"maybe'head"
maybe'kind ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'kind" a) =>
  Lens.Family2.LensLike' f s a
maybe'kind = Data.ProtoLens.Field.field @"maybe'kind"
maybe'localTypeRef ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'localTypeRef" a) =>
  Lens.Family2.LensLike' f s a
maybe'localTypeRef
  = Data.ProtoLens.Field.field @"maybe'localTypeRef"
maybe'moduleName ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'moduleName" a) =>
  Lens.Family2.LensLike' f s a
maybe'moduleName = Data.ProtoLens.Field.field @"maybe'moduleName"
maybe'ntuple ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'ntuple" a) =>
  Lens.Family2.LensLike' f s a
maybe'ntuple = Data.ProtoLens.Field.field @"maybe'ntuple"
maybe'opaque ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'opaque" a) =>
  Lens.Family2.LensLike' f s a
maybe'opaque = Data.ProtoLens.Field.field @"maybe'opaque"
maybe'posFrom ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'posFrom" a) =>
  Lens.Family2.LensLike' f s a
maybe'posFrom = Data.ProtoLens.Field.field @"maybe'posFrom"
maybe'posTo ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'posTo" a) =>
  Lens.Family2.LensLike' f s a
maybe'posTo = Data.ProtoLens.Field.field @"maybe'posTo"
maybe'product ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'product" a) =>
  Lens.Family2.LensLike' f s a
maybe'product = Data.ProtoLens.Field.field @"maybe'product"
maybe'record ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'record" a) =>
  Lens.Family2.LensLike' f s a
maybe'record = Data.ProtoLens.Field.field @"maybe'record"
maybe'sourceInfo ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'sourceInfo" a) =>
  Lens.Family2.LensLike' f s a
maybe'sourceInfo = Data.ProtoLens.Field.field @"maybe'sourceInfo"
maybe'sourceInfog ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'sourceInfog" a) =>
  Lens.Family2.LensLike' f s a
maybe'sourceInfog = Data.ProtoLens.Field.field @"maybe'sourceInfog"
maybe'sum ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'sum" a) =>
  Lens.Family2.LensLike' f s a
maybe'sum = Data.ProtoLens.Field.field @"maybe'sum"
maybe'type' ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'type'" a) =>
  Lens.Family2.LensLike' f s a
maybe'type' = Data.ProtoLens.Field.field @"maybe'type'"
maybe'typeAbs ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'typeAbs" a) =>
  Lens.Family2.LensLike' f s a
maybe'typeAbs = Data.ProtoLens.Field.field @"maybe'typeAbs"
maybe'typeApp ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'typeApp" a) =>
  Lens.Family2.LensLike' f s a
maybe'typeApp = Data.ProtoLens.Field.field @"maybe'typeApp"
maybe'typeArg ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'typeArg" a) =>
  Lens.Family2.LensLike' f s a
maybe'typeArg = Data.ProtoLens.Field.field @"maybe'typeArg"
maybe'typeBody ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'typeBody" a) =>
  Lens.Family2.LensLike' f s a
maybe'typeBody = Data.ProtoLens.Field.field @"maybe'typeBody"
maybe'typeCon ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'typeCon" a) =>
  Lens.Family2.LensLike' f s a
maybe'typeCon = Data.ProtoLens.Field.field @"maybe'typeCon"
maybe'typeConName ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'typeConName" a) =>
  Lens.Family2.LensLike' f s a
maybe'typeConName = Data.ProtoLens.Field.field @"maybe'typeConName"
maybe'typeKind ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'typeKind" a) =>
  Lens.Family2.LensLike' f s a
maybe'typeKind = Data.ProtoLens.Field.field @"maybe'typeKind"
maybe'typeName ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'typeName" a) =>
  Lens.Family2.LensLike' f s a
maybe'typeName = Data.ProtoLens.Field.field @"maybe'typeName"
maybe'typeRef ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'typeRef" a) =>
  Lens.Family2.LensLike' f s a
maybe'typeRef = Data.ProtoLens.Field.field @"maybe'typeRef"
maybe'typeVar ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'typeVar" a) =>
  Lens.Family2.LensLike' f s a
maybe'typeVar = Data.ProtoLens.Field.field @"maybe'typeVar"
maybe'typeclassName ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'typeclassName" a) =>
  Lens.Family2.LensLike' f s a
maybe'typeclassName
  = Data.ProtoLens.Field.field @"maybe'typeclassName"
maybe'value ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'value" a) =>
  Lens.Family2.LensLike' f s a
maybe'value = Data.ProtoLens.Field.field @"maybe'value"
maybe'varName ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'varName" a) =>
  Lens.Family2.LensLike' f s a
maybe'varName = Data.ProtoLens.Field.field @"maybe'varName"
moduleName ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "moduleName" a) =>
  Lens.Family2.LensLike' f s a
moduleName = Data.ProtoLens.Field.field @"moduleName"
name ::
  forall f s a.
  (Prelude.Functor f, Data.ProtoLens.Field.HasField s "name" a) =>
  Lens.Family2.LensLike' f s a
name = Data.ProtoLens.Field.field @"name"
ntuple ::
  forall f s a.
  (Prelude.Functor f, Data.ProtoLens.Field.HasField s "ntuple" a) =>
  Lens.Family2.LensLike' f s a
ntuple = Data.ProtoLens.Field.field @"ntuple"
opaque ::
  forall f s a.
  (Prelude.Functor f, Data.ProtoLens.Field.HasField s "opaque" a) =>
  Lens.Family2.LensLike' f s a
opaque = Data.ProtoLens.Field.field @"opaque"
posFrom ::
  forall f s a.
  (Prelude.Functor f, Data.ProtoLens.Field.HasField s "posFrom" a) =>
  Lens.Family2.LensLike' f s a
posFrom = Data.ProtoLens.Field.field @"posFrom"
posTo ::
  forall f s a.
  (Prelude.Functor f, Data.ProtoLens.Field.HasField s "posTo" a) =>
  Lens.Family2.LensLike' f s a
posTo = Data.ProtoLens.Field.field @"posTo"
record ::
  forall f s a.
  (Prelude.Functor f, Data.ProtoLens.Field.HasField s "record" a) =>
  Lens.Family2.LensLike' f s a
record = Data.ProtoLens.Field.field @"record"
row ::
  forall f s a.
  (Prelude.Functor f, Data.ProtoLens.Field.HasField s "row" a) =>
  Lens.Family2.LensLike' f s a
row = Data.ProtoLens.Field.field @"row"
sourceInfo ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "sourceInfo" a) =>
  Lens.Family2.LensLike' f s a
sourceInfo = Data.ProtoLens.Field.field @"sourceInfo"
sourceInfog ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "sourceInfog" a) =>
  Lens.Family2.LensLike' f s a
sourceInfog = Data.ProtoLens.Field.field @"sourceInfog"
sum ::
  forall f s a.
  (Prelude.Functor f, Data.ProtoLens.Field.HasField s "sum" a) =>
  Lens.Family2.LensLike' f s a
sum = Data.ProtoLens.Field.field @"sum"
typeAbs ::
  forall f s a.
  (Prelude.Functor f, Data.ProtoLens.Field.HasField s "typeAbs" a) =>
  Lens.Family2.LensLike' f s a
typeAbs = Data.ProtoLens.Field.field @"typeAbs"
typeApp ::
  forall f s a.
  (Prelude.Functor f, Data.ProtoLens.Field.HasField s "typeApp" a) =>
  Lens.Family2.LensLike' f s a
typeApp = Data.ProtoLens.Field.field @"typeApp"
typeArg ::
  forall f s a.
  (Prelude.Functor f, Data.ProtoLens.Field.HasField s "typeArg" a) =>
  Lens.Family2.LensLike' f s a
typeArg = Data.ProtoLens.Field.field @"typeArg"
typeBody ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "typeBody" a) =>
  Lens.Family2.LensLike' f s a
typeBody = Data.ProtoLens.Field.field @"typeBody"
typeCon ::
  forall f s a.
  (Prelude.Functor f, Data.ProtoLens.Field.HasField s "typeCon" a) =>
  Lens.Family2.LensLike' f s a
typeCon = Data.ProtoLens.Field.field @"typeCon"
typeConName ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "typeConName" a) =>
  Lens.Family2.LensLike' f s a
typeConName = Data.ProtoLens.Field.field @"typeConName"
typeDefs ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "typeDefs" a) =>
  Lens.Family2.LensLike' f s a
typeDefs = Data.ProtoLens.Field.field @"typeDefs"
typeKind ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "typeKind" a) =>
  Lens.Family2.LensLike' f s a
typeKind = Data.ProtoLens.Field.field @"typeKind"
typeName ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "typeName" a) =>
  Lens.Family2.LensLike' f s a
typeName = Data.ProtoLens.Field.field @"typeName"
typeRef ::
  forall f s a.
  (Prelude.Functor f, Data.ProtoLens.Field.HasField s "typeRef" a) =>
  Lens.Family2.LensLike' f s a
typeRef = Data.ProtoLens.Field.field @"typeRef"
typeVar ::
  forall f s a.
  (Prelude.Functor f, Data.ProtoLens.Field.HasField s "typeVar" a) =>
  Lens.Family2.LensLike' f s a
typeVar = Data.ProtoLens.Field.field @"typeVar"
typeclassName ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "typeclassName" a) =>
  Lens.Family2.LensLike' f s a
typeclassName = Data.ProtoLens.Field.field @"typeclassName"
value ::
  forall f s a.
  (Prelude.Functor f, Data.ProtoLens.Field.HasField s "value" a) =>
  Lens.Family2.LensLike' f s a
value = Data.ProtoLens.Field.field @"value"
varName ::
  forall f s a.
  (Prelude.Functor f, Data.ProtoLens.Field.HasField s "varName" a) =>
  Lens.Family2.LensLike' f s a
varName = Data.ProtoLens.Field.field @"varName"
vec'arguments ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "vec'arguments" a) =>
  Lens.Family2.LensLike' f s a
vec'arguments = Data.ProtoLens.Field.field @"vec'arguments"
vec'constraints ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "vec'constraints" a) =>
  Lens.Family2.LensLike' f s a
vec'constraints = Data.ProtoLens.Field.field @"vec'constraints"
vec'fields ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "vec'fields" a) =>
  Lens.Family2.LensLike' f s a
vec'fields = Data.ProtoLens.Field.field @"vec'fields"
vec'instances ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "vec'instances" a) =>
  Lens.Family2.LensLike' f s a
vec'instances = Data.ProtoLens.Field.field @"vec'instances"
vec'typeDefs ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "vec'typeDefs" a) =>
  Lens.Family2.LensLike' f s a
vec'typeDefs = Data.ProtoLens.Field.field @"vec'typeDefs"
vec'varName ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "vec'varName" a) =>
  Lens.Family2.LensLike' f s a
vec'varName = Data.ProtoLens.Field.field @"vec'varName"