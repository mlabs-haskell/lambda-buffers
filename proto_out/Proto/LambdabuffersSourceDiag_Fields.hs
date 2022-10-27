{- This file was auto-generated from lambdabuffers-source-diag.proto by the proto-lens-protoc program. -}
{-# LANGUAGE ScopedTypeVariables, DataKinds, TypeFamilies, UndecidableInstances, GeneralizedNewtypeDeriving, MultiParamTypeClasses, FlexibleContexts, FlexibleInstances, PatternSynonyms, MagicHash, NoImplicitPrelude, DataKinds, BangPatterns, TypeApplications, OverloadedStrings, DerivingStrategies#-}
{-# OPTIONS_GHC -Wno-unused-imports#-}
{-# OPTIONS_GHC -Wno-duplicate-exports#-}
{-# OPTIONS_GHC -Wno-dodgy-exports#-}
module Proto.LambdabuffersSourceDiag_Fields where
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
import qualified Proto.LambdabuffersSource
issue ::
  forall f s a.
  (Prelude.Functor f, Data.ProtoLens.Field.HasField s "issue" a) =>
  Lens.Family2.LensLike' f s a
issue = Data.ProtoLens.Field.field @"issue"
level ::
  forall f s a.
  (Prelude.Functor f, Data.ProtoLens.Field.HasField s "level" a) =>
  Lens.Family2.LensLike' f s a
level = Data.ProtoLens.Field.field @"level"
maybe'msg ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'msg" a) =>
  Lens.Family2.LensLike' f s a
maybe'msg = Data.ProtoLens.Field.field @"maybe'msg"
maybe'sourceInfo ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'sourceInfo" a) =>
  Lens.Family2.LensLike' f s a
maybe'sourceInfo = Data.ProtoLens.Field.field @"maybe'sourceInfo"
msg ::
  forall f s a.
  (Prelude.Functor f, Data.ProtoLens.Field.HasField s "msg" a) =>
  Lens.Family2.LensLike' f s a
msg = Data.ProtoLens.Field.field @"msg"
recommendation ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "recommendation" a) =>
  Lens.Family2.LensLike' f s a
recommendation = Data.ProtoLens.Field.field @"recommendation"
sourceInfo ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "sourceInfo" a) =>
  Lens.Family2.LensLike' f s a
sourceInfo = Data.ProtoLens.Field.field @"sourceInfo"