{- This file was auto-generated from repls/lambdabuffers-source.proto by the proto-lens-protoc program. -}
{-# LANGUAGE ScopedTypeVariables, DataKinds, TypeFamilies, UndecidableInstances, GeneralizedNewtypeDeriving, MultiParamTypeClasses, FlexibleContexts, FlexibleInstances, PatternSynonyms, MagicHash, NoImplicitPrelude, DataKinds, BangPatterns, TypeApplications, OverloadedStrings, DerivingStrategies#-}
{-# OPTIONS_GHC -Wno-unused-imports#-}
{-# OPTIONS_GHC -Wno-duplicate-exports#-}
{-# OPTIONS_GHC -Wno-dodgy-exports#-}
module Proto.Repls.LambdabuffersSource (
        ConstrName(), Constraint(), Empty(), FieldName(), ForeignTypeRef(),
        InstanceClause(), Kind(), LocalTypeRef(), Module(), ModuleName(),
        NTuple(), Opaque(), Product(), Product'Product(..), _Product'Empty,
        _Product'Record, _Product'Ntuple, Record(), Record'FieldsEntry(),
        SourceInfo(), SourcePosition(), Sum(), Sum'ConstructorsEntry(),
        Type(), Type'Type(..), _Type'TypeVar, _Type'TypeCon, _Type'TypeApp,
        _Type'TypeRef, TypeApp(), TypeBody(), TypeBody'Body(..),
        _TypeBody'Opaque, _TypeBody'Sum, TypeCon(), TypeConName(),
        TypeDefinition(), TypeRef(), TypeRef'TypeRef(..),
        _TypeRef'LocalTypeRef, _TypeRef'ForeignTypeRef, TypeVar(),
        TypeclassDef(), TypeclassName(), VarName()
    ) where
import qualified Data.ProtoLens.Runtime.Control.DeepSeq as Control.DeepSeq
import qualified Data.ProtoLens.Runtime.Data.ProtoLens.Prism as Data.ProtoLens.Prism
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
{- | Fields :
     
         * 'Proto.Repls.LambdabuffersSource_Fields.name' @:: Lens' ConstrName Data.Text.Text@
         * 'Proto.Repls.LambdabuffersSource_Fields.sourceInfo' @:: Lens' ConstrName SourceInfo@
         * 'Proto.Repls.LambdabuffersSource_Fields.maybe'sourceInfo' @:: Lens' ConstrName (Prelude.Maybe SourceInfo)@ -}
data ConstrName
  = ConstrName'_constructor {_ConstrName'name :: !Data.Text.Text,
                             _ConstrName'sourceInfo :: !(Prelude.Maybe SourceInfo),
                             _ConstrName'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving stock (Prelude.Eq, Prelude.Ord)
instance Prelude.Show ConstrName where
  showsPrec _ __x __s
    = Prelude.showChar
        '{'
        (Prelude.showString
           (Data.ProtoLens.showMessageShort __x) (Prelude.showChar '}' __s))
instance Data.ProtoLens.Field.HasField ConstrName "name" Data.Text.Text where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ConstrName'name (\ x__ y__ -> x__ {_ConstrName'name = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField ConstrName "sourceInfo" SourceInfo where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ConstrName'sourceInfo
           (\ x__ y__ -> x__ {_ConstrName'sourceInfo = y__}))
        (Data.ProtoLens.maybeLens Data.ProtoLens.defMessage)
instance Data.ProtoLens.Field.HasField ConstrName "maybe'sourceInfo" (Prelude.Maybe SourceInfo) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ConstrName'sourceInfo
           (\ x__ y__ -> x__ {_ConstrName'sourceInfo = y__}))
        Prelude.id
instance Data.ProtoLens.Message ConstrName where
  messageName _ = Data.Text.pack "lambdabuffers.source.ConstrName"
  packedMessageDescriptor _
    = "\n\
      \\n\
      \ConstrName\DC2\DC2\n\
      \\EOTname\CAN\SOH \SOH(\tR\EOTname\DC2A\n\
      \\vsource_info\CAN\STX \SOH(\v2 .lambdabuffers.source.SourceInfoR\n\
      \sourceInfo"
  packedFileDescriptor _ = packedFileDescriptor
  fieldsByTag
    = let
        name__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "name"
              (Data.ProtoLens.ScalarField Data.ProtoLens.StringField ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Text.Text)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional (Data.ProtoLens.Field.field @"name")) ::
              Data.ProtoLens.FieldDescriptor ConstrName
        sourceInfo__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "source_info"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor SourceInfo)
              (Data.ProtoLens.OptionalField
                 (Data.ProtoLens.Field.field @"maybe'sourceInfo")) ::
              Data.ProtoLens.FieldDescriptor ConstrName
      in
        Data.Map.fromList
          [(Data.ProtoLens.Tag 1, name__field_descriptor),
           (Data.ProtoLens.Tag 2, sourceInfo__field_descriptor)]
  unknownFields
    = Lens.Family2.Unchecked.lens
        _ConstrName'_unknownFields
        (\ x__ y__ -> x__ {_ConstrName'_unknownFields = y__})
  defMessage
    = ConstrName'_constructor
        {_ConstrName'name = Data.ProtoLens.fieldDefault,
         _ConstrName'sourceInfo = Prelude.Nothing,
         _ConstrName'_unknownFields = []}
  parseMessage
    = let
        loop ::
          ConstrName -> Data.ProtoLens.Encoding.Bytes.Parser ConstrName
        loop x
          = do end <- Data.ProtoLens.Encoding.Bytes.atEnd
               if end then
                   do (let missing = []
                       in
                         if Prelude.null missing then
                             Prelude.return ()
                         else
                             Prelude.fail
                               ((Prelude.++)
                                  "Missing required fields: "
                                  (Prelude.show (missing :: [Prelude.String]))))
                      Prelude.return
                        (Lens.Family2.over
                           Data.ProtoLens.unknownFields (\ !t -> Prelude.reverse t) x)
               else
                   do tag <- Data.ProtoLens.Encoding.Bytes.getVarInt
                      case tag of
                        10
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do value <- do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                                       Data.ProtoLens.Encoding.Bytes.getBytes
                                                         (Prelude.fromIntegral len)
                                           Data.ProtoLens.Encoding.Bytes.runEither
                                             (case Data.Text.Encoding.decodeUtf8' value of
                                                (Prelude.Left err)
                                                  -> Prelude.Left (Prelude.show err)
                                                (Prelude.Right r) -> Prelude.Right r))
                                       "name"
                                loop (Lens.Family2.set (Data.ProtoLens.Field.field @"name") y x)
                        18
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                           Data.ProtoLens.Encoding.Bytes.isolate
                                             (Prelude.fromIntegral len) Data.ProtoLens.parseMessage)
                                       "source_info"
                                loop
                                  (Lens.Family2.set (Data.ProtoLens.Field.field @"sourceInfo") y x)
                        wire
                          -> do !y <- Data.ProtoLens.Encoding.Wire.parseTaggedValueFromWire
                                        wire
                                loop
                                  (Lens.Family2.over
                                     Data.ProtoLens.unknownFields (\ !t -> (:) y t) x)
      in
        (Data.ProtoLens.Encoding.Bytes.<?>)
          (do loop Data.ProtoLens.defMessage) "ConstrName"
  buildMessage
    = \ _x
        -> (Data.Monoid.<>)
             (let _v = Lens.Family2.view (Data.ProtoLens.Field.field @"name") _x
              in
                if (Prelude.==) _v Data.ProtoLens.fieldDefault then
                    Data.Monoid.mempty
                else
                    (Data.Monoid.<>)
                      (Data.ProtoLens.Encoding.Bytes.putVarInt 10)
                      ((Prelude..)
                         (\ bs
                            -> (Data.Monoid.<>)
                                 (Data.ProtoLens.Encoding.Bytes.putVarInt
                                    (Prelude.fromIntegral (Data.ByteString.length bs)))
                                 (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                         Data.Text.Encoding.encodeUtf8 _v))
             ((Data.Monoid.<>)
                (case
                     Lens.Family2.view
                       (Data.ProtoLens.Field.field @"maybe'sourceInfo") _x
                 of
                   Prelude.Nothing -> Data.Monoid.mempty
                   (Prelude.Just _v)
                     -> (Data.Monoid.<>)
                          (Data.ProtoLens.Encoding.Bytes.putVarInt 18)
                          ((Prelude..)
                             (\ bs
                                -> (Data.Monoid.<>)
                                     (Data.ProtoLens.Encoding.Bytes.putVarInt
                                        (Prelude.fromIntegral (Data.ByteString.length bs)))
                                     (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                             Data.ProtoLens.encodeMessage _v))
                (Data.ProtoLens.Encoding.Wire.buildFieldSet
                   (Lens.Family2.view Data.ProtoLens.unknownFields _x)))
instance Control.DeepSeq.NFData ConstrName where
  rnf
    = \ x__
        -> Control.DeepSeq.deepseq
             (_ConstrName'_unknownFields x__)
             (Control.DeepSeq.deepseq
                (_ConstrName'name x__)
                (Control.DeepSeq.deepseq (_ConstrName'sourceInfo x__) ()))
{- | Fields :
     
         * 'Proto.Repls.LambdabuffersSource_Fields.typeclassName' @:: Lens' Constraint TypeclassName@
         * 'Proto.Repls.LambdabuffersSource_Fields.maybe'typeclassName' @:: Lens' Constraint (Prelude.Maybe TypeclassName)@
         * 'Proto.Repls.LambdabuffersSource_Fields.arguments' @:: Lens' Constraint [Type]@
         * 'Proto.Repls.LambdabuffersSource_Fields.vec'arguments' @:: Lens' Constraint (Data.Vector.Vector Type)@
         * 'Proto.Repls.LambdabuffersSource_Fields.sourceInfo' @:: Lens' Constraint SourceInfo@
         * 'Proto.Repls.LambdabuffersSource_Fields.maybe'sourceInfo' @:: Lens' Constraint (Prelude.Maybe SourceInfo)@ -}
data Constraint
  = Constraint'_constructor {_Constraint'typeclassName :: !(Prelude.Maybe TypeclassName),
                             _Constraint'arguments :: !(Data.Vector.Vector Type),
                             _Constraint'sourceInfo :: !(Prelude.Maybe SourceInfo),
                             _Constraint'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving stock (Prelude.Eq, Prelude.Ord)
instance Prelude.Show Constraint where
  showsPrec _ __x __s
    = Prelude.showChar
        '{'
        (Prelude.showString
           (Data.ProtoLens.showMessageShort __x) (Prelude.showChar '}' __s))
instance Data.ProtoLens.Field.HasField Constraint "typeclassName" TypeclassName where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _Constraint'typeclassName
           (\ x__ y__ -> x__ {_Constraint'typeclassName = y__}))
        (Data.ProtoLens.maybeLens Data.ProtoLens.defMessage)
instance Data.ProtoLens.Field.HasField Constraint "maybe'typeclassName" (Prelude.Maybe TypeclassName) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _Constraint'typeclassName
           (\ x__ y__ -> x__ {_Constraint'typeclassName = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField Constraint "arguments" [Type] where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _Constraint'arguments
           (\ x__ y__ -> x__ {_Constraint'arguments = y__}))
        (Lens.Family2.Unchecked.lens
           Data.Vector.Generic.toList
           (\ _ y__ -> Data.Vector.Generic.fromList y__))
instance Data.ProtoLens.Field.HasField Constraint "vec'arguments" (Data.Vector.Vector Type) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _Constraint'arguments
           (\ x__ y__ -> x__ {_Constraint'arguments = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField Constraint "sourceInfo" SourceInfo where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _Constraint'sourceInfo
           (\ x__ y__ -> x__ {_Constraint'sourceInfo = y__}))
        (Data.ProtoLens.maybeLens Data.ProtoLens.defMessage)
instance Data.ProtoLens.Field.HasField Constraint "maybe'sourceInfo" (Prelude.Maybe SourceInfo) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _Constraint'sourceInfo
           (\ x__ y__ -> x__ {_Constraint'sourceInfo = y__}))
        Prelude.id
instance Data.ProtoLens.Message Constraint where
  messageName _ = Data.Text.pack "lambdabuffers.source.Constraint"
  packedMessageDescriptor _
    = "\n\
      \\n\
      \Constraint\DC2J\n\
      \\SOtypeclass_name\CAN\SOH \SOH(\v2#.lambdabuffers.source.TypeclassNameR\rtypeclassName\DC28\n\
      \\targuments\CAN\STX \ETX(\v2\SUB.lambdabuffers.source.TypeR\targuments\DC2A\n\
      \\vsource_info\CAN\ETX \SOH(\v2 .lambdabuffers.source.SourceInfoR\n\
      \sourceInfo"
  packedFileDescriptor _ = packedFileDescriptor
  fieldsByTag
    = let
        typeclassName__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "typeclass_name"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor TypeclassName)
              (Data.ProtoLens.OptionalField
                 (Data.ProtoLens.Field.field @"maybe'typeclassName")) ::
              Data.ProtoLens.FieldDescriptor Constraint
        arguments__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "arguments"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor Type)
              (Data.ProtoLens.RepeatedField
                 Data.ProtoLens.Unpacked
                 (Data.ProtoLens.Field.field @"arguments")) ::
              Data.ProtoLens.FieldDescriptor Constraint
        sourceInfo__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "source_info"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor SourceInfo)
              (Data.ProtoLens.OptionalField
                 (Data.ProtoLens.Field.field @"maybe'sourceInfo")) ::
              Data.ProtoLens.FieldDescriptor Constraint
      in
        Data.Map.fromList
          [(Data.ProtoLens.Tag 1, typeclassName__field_descriptor),
           (Data.ProtoLens.Tag 2, arguments__field_descriptor),
           (Data.ProtoLens.Tag 3, sourceInfo__field_descriptor)]
  unknownFields
    = Lens.Family2.Unchecked.lens
        _Constraint'_unknownFields
        (\ x__ y__ -> x__ {_Constraint'_unknownFields = y__})
  defMessage
    = Constraint'_constructor
        {_Constraint'typeclassName = Prelude.Nothing,
         _Constraint'arguments = Data.Vector.Generic.empty,
         _Constraint'sourceInfo = Prelude.Nothing,
         _Constraint'_unknownFields = []}
  parseMessage
    = let
        loop ::
          Constraint
          -> Data.ProtoLens.Encoding.Growing.Growing Data.Vector.Vector Data.ProtoLens.Encoding.Growing.RealWorld Type
             -> Data.ProtoLens.Encoding.Bytes.Parser Constraint
        loop x mutable'arguments
          = do end <- Data.ProtoLens.Encoding.Bytes.atEnd
               if end then
                   do frozen'arguments <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                            (Data.ProtoLens.Encoding.Growing.unsafeFreeze
                                               mutable'arguments)
                      (let missing = []
                       in
                         if Prelude.null missing then
                             Prelude.return ()
                         else
                             Prelude.fail
                               ((Prelude.++)
                                  "Missing required fields: "
                                  (Prelude.show (missing :: [Prelude.String]))))
                      Prelude.return
                        (Lens.Family2.over
                           Data.ProtoLens.unknownFields (\ !t -> Prelude.reverse t)
                           (Lens.Family2.set
                              (Data.ProtoLens.Field.field @"vec'arguments") frozen'arguments x))
               else
                   do tag <- Data.ProtoLens.Encoding.Bytes.getVarInt
                      case tag of
                        10
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                           Data.ProtoLens.Encoding.Bytes.isolate
                                             (Prelude.fromIntegral len) Data.ProtoLens.parseMessage)
                                       "typeclass_name"
                                loop
                                  (Lens.Family2.set
                                     (Data.ProtoLens.Field.field @"typeclassName") y x)
                                  mutable'arguments
                        18
                          -> do !y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                        (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                            Data.ProtoLens.Encoding.Bytes.isolate
                                              (Prelude.fromIntegral len)
                                              Data.ProtoLens.parseMessage)
                                        "arguments"
                                v <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                       (Data.ProtoLens.Encoding.Growing.append mutable'arguments y)
                                loop x v
                        26
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                           Data.ProtoLens.Encoding.Bytes.isolate
                                             (Prelude.fromIntegral len) Data.ProtoLens.parseMessage)
                                       "source_info"
                                loop
                                  (Lens.Family2.set (Data.ProtoLens.Field.field @"sourceInfo") y x)
                                  mutable'arguments
                        wire
                          -> do !y <- Data.ProtoLens.Encoding.Wire.parseTaggedValueFromWire
                                        wire
                                loop
                                  (Lens.Family2.over
                                     Data.ProtoLens.unknownFields (\ !t -> (:) y t) x)
                                  mutable'arguments
      in
        (Data.ProtoLens.Encoding.Bytes.<?>)
          (do mutable'arguments <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                     Data.ProtoLens.Encoding.Growing.new
              loop Data.ProtoLens.defMessage mutable'arguments)
          "Constraint"
  buildMessage
    = \ _x
        -> (Data.Monoid.<>)
             (case
                  Lens.Family2.view
                    (Data.ProtoLens.Field.field @"maybe'typeclassName") _x
              of
                Prelude.Nothing -> Data.Monoid.mempty
                (Prelude.Just _v)
                  -> (Data.Monoid.<>)
                       (Data.ProtoLens.Encoding.Bytes.putVarInt 10)
                       ((Prelude..)
                          (\ bs
                             -> (Data.Monoid.<>)
                                  (Data.ProtoLens.Encoding.Bytes.putVarInt
                                     (Prelude.fromIntegral (Data.ByteString.length bs)))
                                  (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                          Data.ProtoLens.encodeMessage _v))
             ((Data.Monoid.<>)
                (Data.ProtoLens.Encoding.Bytes.foldMapBuilder
                   (\ _v
                      -> (Data.Monoid.<>)
                           (Data.ProtoLens.Encoding.Bytes.putVarInt 18)
                           ((Prelude..)
                              (\ bs
                                 -> (Data.Monoid.<>)
                                      (Data.ProtoLens.Encoding.Bytes.putVarInt
                                         (Prelude.fromIntegral (Data.ByteString.length bs)))
                                      (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                              Data.ProtoLens.encodeMessage _v))
                   (Lens.Family2.view
                      (Data.ProtoLens.Field.field @"vec'arguments") _x))
                ((Data.Monoid.<>)
                   (case
                        Lens.Family2.view
                          (Data.ProtoLens.Field.field @"maybe'sourceInfo") _x
                    of
                      Prelude.Nothing -> Data.Monoid.mempty
                      (Prelude.Just _v)
                        -> (Data.Monoid.<>)
                             (Data.ProtoLens.Encoding.Bytes.putVarInt 26)
                             ((Prelude..)
                                (\ bs
                                   -> (Data.Monoid.<>)
                                        (Data.ProtoLens.Encoding.Bytes.putVarInt
                                           (Prelude.fromIntegral (Data.ByteString.length bs)))
                                        (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                                Data.ProtoLens.encodeMessage _v))
                   (Data.ProtoLens.Encoding.Wire.buildFieldSet
                      (Lens.Family2.view Data.ProtoLens.unknownFields _x))))
instance Control.DeepSeq.NFData Constraint where
  rnf
    = \ x__
        -> Control.DeepSeq.deepseq
             (_Constraint'_unknownFields x__)
             (Control.DeepSeq.deepseq
                (_Constraint'typeclassName x__)
                (Control.DeepSeq.deepseq
                   (_Constraint'arguments x__)
                   (Control.DeepSeq.deepseq (_Constraint'sourceInfo x__) ())))
{- | Fields :
     
         * 'Proto.Repls.LambdabuffersSource_Fields.sourceInfo' @:: Lens' Empty SourceInfo@
         * 'Proto.Repls.LambdabuffersSource_Fields.maybe'sourceInfo' @:: Lens' Empty (Prelude.Maybe SourceInfo)@ -}
data Empty
  = Empty'_constructor {_Empty'sourceInfo :: !(Prelude.Maybe SourceInfo),
                        _Empty'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving stock (Prelude.Eq, Prelude.Ord)
instance Prelude.Show Empty where
  showsPrec _ __x __s
    = Prelude.showChar
        '{'
        (Prelude.showString
           (Data.ProtoLens.showMessageShort __x) (Prelude.showChar '}' __s))
instance Data.ProtoLens.Field.HasField Empty "sourceInfo" SourceInfo where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _Empty'sourceInfo (\ x__ y__ -> x__ {_Empty'sourceInfo = y__}))
        (Data.ProtoLens.maybeLens Data.ProtoLens.defMessage)
instance Data.ProtoLens.Field.HasField Empty "maybe'sourceInfo" (Prelude.Maybe SourceInfo) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _Empty'sourceInfo (\ x__ y__ -> x__ {_Empty'sourceInfo = y__}))
        Prelude.id
instance Data.ProtoLens.Message Empty where
  messageName _ = Data.Text.pack "lambdabuffers.source.Empty"
  packedMessageDescriptor _
    = "\n\
      \\ENQEmpty\DC2A\n\
      \\vsource_info\CAN\SOH \SOH(\v2 .lambdabuffers.source.SourceInfoR\n\
      \sourceInfo"
  packedFileDescriptor _ = packedFileDescriptor
  fieldsByTag
    = let
        sourceInfo__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "source_info"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor SourceInfo)
              (Data.ProtoLens.OptionalField
                 (Data.ProtoLens.Field.field @"maybe'sourceInfo")) ::
              Data.ProtoLens.FieldDescriptor Empty
      in
        Data.Map.fromList
          [(Data.ProtoLens.Tag 1, sourceInfo__field_descriptor)]
  unknownFields
    = Lens.Family2.Unchecked.lens
        _Empty'_unknownFields
        (\ x__ y__ -> x__ {_Empty'_unknownFields = y__})
  defMessage
    = Empty'_constructor
        {_Empty'sourceInfo = Prelude.Nothing, _Empty'_unknownFields = []}
  parseMessage
    = let
        loop :: Empty -> Data.ProtoLens.Encoding.Bytes.Parser Empty
        loop x
          = do end <- Data.ProtoLens.Encoding.Bytes.atEnd
               if end then
                   do (let missing = []
                       in
                         if Prelude.null missing then
                             Prelude.return ()
                         else
                             Prelude.fail
                               ((Prelude.++)
                                  "Missing required fields: "
                                  (Prelude.show (missing :: [Prelude.String]))))
                      Prelude.return
                        (Lens.Family2.over
                           Data.ProtoLens.unknownFields (\ !t -> Prelude.reverse t) x)
               else
                   do tag <- Data.ProtoLens.Encoding.Bytes.getVarInt
                      case tag of
                        10
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                           Data.ProtoLens.Encoding.Bytes.isolate
                                             (Prelude.fromIntegral len) Data.ProtoLens.parseMessage)
                                       "source_info"
                                loop
                                  (Lens.Family2.set (Data.ProtoLens.Field.field @"sourceInfo") y x)
                        wire
                          -> do !y <- Data.ProtoLens.Encoding.Wire.parseTaggedValueFromWire
                                        wire
                                loop
                                  (Lens.Family2.over
                                     Data.ProtoLens.unknownFields (\ !t -> (:) y t) x)
      in
        (Data.ProtoLens.Encoding.Bytes.<?>)
          (do loop Data.ProtoLens.defMessage) "Empty"
  buildMessage
    = \ _x
        -> (Data.Monoid.<>)
             (case
                  Lens.Family2.view
                    (Data.ProtoLens.Field.field @"maybe'sourceInfo") _x
              of
                Prelude.Nothing -> Data.Monoid.mempty
                (Prelude.Just _v)
                  -> (Data.Monoid.<>)
                       (Data.ProtoLens.Encoding.Bytes.putVarInt 10)
                       ((Prelude..)
                          (\ bs
                             -> (Data.Monoid.<>)
                                  (Data.ProtoLens.Encoding.Bytes.putVarInt
                                     (Prelude.fromIntegral (Data.ByteString.length bs)))
                                  (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                          Data.ProtoLens.encodeMessage _v))
             (Data.ProtoLens.Encoding.Wire.buildFieldSet
                (Lens.Family2.view Data.ProtoLens.unknownFields _x))
instance Control.DeepSeq.NFData Empty where
  rnf
    = \ x__
        -> Control.DeepSeq.deepseq
             (_Empty'_unknownFields x__)
             (Control.DeepSeq.deepseq (_Empty'sourceInfo x__) ())
{- | Fields :
     
         * 'Proto.Repls.LambdabuffersSource_Fields.name' @:: Lens' FieldName Data.Text.Text@
         * 'Proto.Repls.LambdabuffersSource_Fields.sourceInfo' @:: Lens' FieldName SourceInfo@
         * 'Proto.Repls.LambdabuffersSource_Fields.maybe'sourceInfo' @:: Lens' FieldName (Prelude.Maybe SourceInfo)@ -}
data FieldName
  = FieldName'_constructor {_FieldName'name :: !Data.Text.Text,
                            _FieldName'sourceInfo :: !(Prelude.Maybe SourceInfo),
                            _FieldName'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving stock (Prelude.Eq, Prelude.Ord)
instance Prelude.Show FieldName where
  showsPrec _ __x __s
    = Prelude.showChar
        '{'
        (Prelude.showString
           (Data.ProtoLens.showMessageShort __x) (Prelude.showChar '}' __s))
instance Data.ProtoLens.Field.HasField FieldName "name" Data.Text.Text where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _FieldName'name (\ x__ y__ -> x__ {_FieldName'name = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField FieldName "sourceInfo" SourceInfo where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _FieldName'sourceInfo
           (\ x__ y__ -> x__ {_FieldName'sourceInfo = y__}))
        (Data.ProtoLens.maybeLens Data.ProtoLens.defMessage)
instance Data.ProtoLens.Field.HasField FieldName "maybe'sourceInfo" (Prelude.Maybe SourceInfo) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _FieldName'sourceInfo
           (\ x__ y__ -> x__ {_FieldName'sourceInfo = y__}))
        Prelude.id
instance Data.ProtoLens.Message FieldName where
  messageName _ = Data.Text.pack "lambdabuffers.source.FieldName"
  packedMessageDescriptor _
    = "\n\
      \\tFieldName\DC2\DC2\n\
      \\EOTname\CAN\SOH \SOH(\tR\EOTname\DC2A\n\
      \\vsource_info\CAN\STX \SOH(\v2 .lambdabuffers.source.SourceInfoR\n\
      \sourceInfo"
  packedFileDescriptor _ = packedFileDescriptor
  fieldsByTag
    = let
        name__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "name"
              (Data.ProtoLens.ScalarField Data.ProtoLens.StringField ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Text.Text)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional (Data.ProtoLens.Field.field @"name")) ::
              Data.ProtoLens.FieldDescriptor FieldName
        sourceInfo__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "source_info"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor SourceInfo)
              (Data.ProtoLens.OptionalField
                 (Data.ProtoLens.Field.field @"maybe'sourceInfo")) ::
              Data.ProtoLens.FieldDescriptor FieldName
      in
        Data.Map.fromList
          [(Data.ProtoLens.Tag 1, name__field_descriptor),
           (Data.ProtoLens.Tag 2, sourceInfo__field_descriptor)]
  unknownFields
    = Lens.Family2.Unchecked.lens
        _FieldName'_unknownFields
        (\ x__ y__ -> x__ {_FieldName'_unknownFields = y__})
  defMessage
    = FieldName'_constructor
        {_FieldName'name = Data.ProtoLens.fieldDefault,
         _FieldName'sourceInfo = Prelude.Nothing,
         _FieldName'_unknownFields = []}
  parseMessage
    = let
        loop :: FieldName -> Data.ProtoLens.Encoding.Bytes.Parser FieldName
        loop x
          = do end <- Data.ProtoLens.Encoding.Bytes.atEnd
               if end then
                   do (let missing = []
                       in
                         if Prelude.null missing then
                             Prelude.return ()
                         else
                             Prelude.fail
                               ((Prelude.++)
                                  "Missing required fields: "
                                  (Prelude.show (missing :: [Prelude.String]))))
                      Prelude.return
                        (Lens.Family2.over
                           Data.ProtoLens.unknownFields (\ !t -> Prelude.reverse t) x)
               else
                   do tag <- Data.ProtoLens.Encoding.Bytes.getVarInt
                      case tag of
                        10
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do value <- do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                                       Data.ProtoLens.Encoding.Bytes.getBytes
                                                         (Prelude.fromIntegral len)
                                           Data.ProtoLens.Encoding.Bytes.runEither
                                             (case Data.Text.Encoding.decodeUtf8' value of
                                                (Prelude.Left err)
                                                  -> Prelude.Left (Prelude.show err)
                                                (Prelude.Right r) -> Prelude.Right r))
                                       "name"
                                loop (Lens.Family2.set (Data.ProtoLens.Field.field @"name") y x)
                        18
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                           Data.ProtoLens.Encoding.Bytes.isolate
                                             (Prelude.fromIntegral len) Data.ProtoLens.parseMessage)
                                       "source_info"
                                loop
                                  (Lens.Family2.set (Data.ProtoLens.Field.field @"sourceInfo") y x)
                        wire
                          -> do !y <- Data.ProtoLens.Encoding.Wire.parseTaggedValueFromWire
                                        wire
                                loop
                                  (Lens.Family2.over
                                     Data.ProtoLens.unknownFields (\ !t -> (:) y t) x)
      in
        (Data.ProtoLens.Encoding.Bytes.<?>)
          (do loop Data.ProtoLens.defMessage) "FieldName"
  buildMessage
    = \ _x
        -> (Data.Monoid.<>)
             (let _v = Lens.Family2.view (Data.ProtoLens.Field.field @"name") _x
              in
                if (Prelude.==) _v Data.ProtoLens.fieldDefault then
                    Data.Monoid.mempty
                else
                    (Data.Monoid.<>)
                      (Data.ProtoLens.Encoding.Bytes.putVarInt 10)
                      ((Prelude..)
                         (\ bs
                            -> (Data.Monoid.<>)
                                 (Data.ProtoLens.Encoding.Bytes.putVarInt
                                    (Prelude.fromIntegral (Data.ByteString.length bs)))
                                 (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                         Data.Text.Encoding.encodeUtf8 _v))
             ((Data.Monoid.<>)
                (case
                     Lens.Family2.view
                       (Data.ProtoLens.Field.field @"maybe'sourceInfo") _x
                 of
                   Prelude.Nothing -> Data.Monoid.mempty
                   (Prelude.Just _v)
                     -> (Data.Monoid.<>)
                          (Data.ProtoLens.Encoding.Bytes.putVarInt 18)
                          ((Prelude..)
                             (\ bs
                                -> (Data.Monoid.<>)
                                     (Data.ProtoLens.Encoding.Bytes.putVarInt
                                        (Prelude.fromIntegral (Data.ByteString.length bs)))
                                     (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                             Data.ProtoLens.encodeMessage _v))
                (Data.ProtoLens.Encoding.Wire.buildFieldSet
                   (Lens.Family2.view Data.ProtoLens.unknownFields _x)))
instance Control.DeepSeq.NFData FieldName where
  rnf
    = \ x__
        -> Control.DeepSeq.deepseq
             (_FieldName'_unknownFields x__)
             (Control.DeepSeq.deepseq
                (_FieldName'name x__)
                (Control.DeepSeq.deepseq (_FieldName'sourceInfo x__) ()))
{- | Fields :
     
         * 'Proto.Repls.LambdabuffersSource_Fields.moduleName' @:: Lens' ForeignTypeRef ModuleName@
         * 'Proto.Repls.LambdabuffersSource_Fields.maybe'moduleName' @:: Lens' ForeignTypeRef (Prelude.Maybe ModuleName)@
         * 'Proto.Repls.LambdabuffersSource_Fields.typeConName' @:: Lens' ForeignTypeRef TypeConName@
         * 'Proto.Repls.LambdabuffersSource_Fields.maybe'typeConName' @:: Lens' ForeignTypeRef (Prelude.Maybe TypeConName)@
         * 'Proto.Repls.LambdabuffersSource_Fields.sourceInfo' @:: Lens' ForeignTypeRef SourceInfo@
         * 'Proto.Repls.LambdabuffersSource_Fields.maybe'sourceInfo' @:: Lens' ForeignTypeRef (Prelude.Maybe SourceInfo)@ -}
data ForeignTypeRef
  = ForeignTypeRef'_constructor {_ForeignTypeRef'moduleName :: !(Prelude.Maybe ModuleName),
                                 _ForeignTypeRef'typeConName :: !(Prelude.Maybe TypeConName),
                                 _ForeignTypeRef'sourceInfo :: !(Prelude.Maybe SourceInfo),
                                 _ForeignTypeRef'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving stock (Prelude.Eq, Prelude.Ord)
instance Prelude.Show ForeignTypeRef where
  showsPrec _ __x __s
    = Prelude.showChar
        '{'
        (Prelude.showString
           (Data.ProtoLens.showMessageShort __x) (Prelude.showChar '}' __s))
instance Data.ProtoLens.Field.HasField ForeignTypeRef "moduleName" ModuleName where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ForeignTypeRef'moduleName
           (\ x__ y__ -> x__ {_ForeignTypeRef'moduleName = y__}))
        (Data.ProtoLens.maybeLens Data.ProtoLens.defMessage)
instance Data.ProtoLens.Field.HasField ForeignTypeRef "maybe'moduleName" (Prelude.Maybe ModuleName) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ForeignTypeRef'moduleName
           (\ x__ y__ -> x__ {_ForeignTypeRef'moduleName = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField ForeignTypeRef "typeConName" TypeConName where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ForeignTypeRef'typeConName
           (\ x__ y__ -> x__ {_ForeignTypeRef'typeConName = y__}))
        (Data.ProtoLens.maybeLens Data.ProtoLens.defMessage)
instance Data.ProtoLens.Field.HasField ForeignTypeRef "maybe'typeConName" (Prelude.Maybe TypeConName) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ForeignTypeRef'typeConName
           (\ x__ y__ -> x__ {_ForeignTypeRef'typeConName = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField ForeignTypeRef "sourceInfo" SourceInfo where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ForeignTypeRef'sourceInfo
           (\ x__ y__ -> x__ {_ForeignTypeRef'sourceInfo = y__}))
        (Data.ProtoLens.maybeLens Data.ProtoLens.defMessage)
instance Data.ProtoLens.Field.HasField ForeignTypeRef "maybe'sourceInfo" (Prelude.Maybe SourceInfo) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ForeignTypeRef'sourceInfo
           (\ x__ y__ -> x__ {_ForeignTypeRef'sourceInfo = y__}))
        Prelude.id
instance Data.ProtoLens.Message ForeignTypeRef where
  messageName _
    = Data.Text.pack "lambdabuffers.source.ForeignTypeRef"
  packedMessageDescriptor _
    = "\n\
      \\SOForeignTypeRef\DC2A\n\
      \\vmodule_name\CAN\SOH \SOH(\v2 .lambdabuffers.source.ModuleNameR\n\
      \moduleName\DC2E\n\
      \\rtype_con_name\CAN\STX \SOH(\v2!.lambdabuffers.source.TypeConNameR\vtypeConName\DC2A\n\
      \\vsource_info\CAN\ETX \SOH(\v2 .lambdabuffers.source.SourceInfoR\n\
      \sourceInfo"
  packedFileDescriptor _ = packedFileDescriptor
  fieldsByTag
    = let
        moduleName__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "module_name"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor ModuleName)
              (Data.ProtoLens.OptionalField
                 (Data.ProtoLens.Field.field @"maybe'moduleName")) ::
              Data.ProtoLens.FieldDescriptor ForeignTypeRef
        typeConName__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "type_con_name"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor TypeConName)
              (Data.ProtoLens.OptionalField
                 (Data.ProtoLens.Field.field @"maybe'typeConName")) ::
              Data.ProtoLens.FieldDescriptor ForeignTypeRef
        sourceInfo__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "source_info"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor SourceInfo)
              (Data.ProtoLens.OptionalField
                 (Data.ProtoLens.Field.field @"maybe'sourceInfo")) ::
              Data.ProtoLens.FieldDescriptor ForeignTypeRef
      in
        Data.Map.fromList
          [(Data.ProtoLens.Tag 1, moduleName__field_descriptor),
           (Data.ProtoLens.Tag 2, typeConName__field_descriptor),
           (Data.ProtoLens.Tag 3, sourceInfo__field_descriptor)]
  unknownFields
    = Lens.Family2.Unchecked.lens
        _ForeignTypeRef'_unknownFields
        (\ x__ y__ -> x__ {_ForeignTypeRef'_unknownFields = y__})
  defMessage
    = ForeignTypeRef'_constructor
        {_ForeignTypeRef'moduleName = Prelude.Nothing,
         _ForeignTypeRef'typeConName = Prelude.Nothing,
         _ForeignTypeRef'sourceInfo = Prelude.Nothing,
         _ForeignTypeRef'_unknownFields = []}
  parseMessage
    = let
        loop ::
          ForeignTypeRef
          -> Data.ProtoLens.Encoding.Bytes.Parser ForeignTypeRef
        loop x
          = do end <- Data.ProtoLens.Encoding.Bytes.atEnd
               if end then
                   do (let missing = []
                       in
                         if Prelude.null missing then
                             Prelude.return ()
                         else
                             Prelude.fail
                               ((Prelude.++)
                                  "Missing required fields: "
                                  (Prelude.show (missing :: [Prelude.String]))))
                      Prelude.return
                        (Lens.Family2.over
                           Data.ProtoLens.unknownFields (\ !t -> Prelude.reverse t) x)
               else
                   do tag <- Data.ProtoLens.Encoding.Bytes.getVarInt
                      case tag of
                        10
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                           Data.ProtoLens.Encoding.Bytes.isolate
                                             (Prelude.fromIntegral len) Data.ProtoLens.parseMessage)
                                       "module_name"
                                loop
                                  (Lens.Family2.set (Data.ProtoLens.Field.field @"moduleName") y x)
                        18
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                           Data.ProtoLens.Encoding.Bytes.isolate
                                             (Prelude.fromIntegral len) Data.ProtoLens.parseMessage)
                                       "type_con_name"
                                loop
                                  (Lens.Family2.set (Data.ProtoLens.Field.field @"typeConName") y x)
                        26
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                           Data.ProtoLens.Encoding.Bytes.isolate
                                             (Prelude.fromIntegral len) Data.ProtoLens.parseMessage)
                                       "source_info"
                                loop
                                  (Lens.Family2.set (Data.ProtoLens.Field.field @"sourceInfo") y x)
                        wire
                          -> do !y <- Data.ProtoLens.Encoding.Wire.parseTaggedValueFromWire
                                        wire
                                loop
                                  (Lens.Family2.over
                                     Data.ProtoLens.unknownFields (\ !t -> (:) y t) x)
      in
        (Data.ProtoLens.Encoding.Bytes.<?>)
          (do loop Data.ProtoLens.defMessage) "ForeignTypeRef"
  buildMessage
    = \ _x
        -> (Data.Monoid.<>)
             (case
                  Lens.Family2.view
                    (Data.ProtoLens.Field.field @"maybe'moduleName") _x
              of
                Prelude.Nothing -> Data.Monoid.mempty
                (Prelude.Just _v)
                  -> (Data.Monoid.<>)
                       (Data.ProtoLens.Encoding.Bytes.putVarInt 10)
                       ((Prelude..)
                          (\ bs
                             -> (Data.Monoid.<>)
                                  (Data.ProtoLens.Encoding.Bytes.putVarInt
                                     (Prelude.fromIntegral (Data.ByteString.length bs)))
                                  (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                          Data.ProtoLens.encodeMessage _v))
             ((Data.Monoid.<>)
                (case
                     Lens.Family2.view
                       (Data.ProtoLens.Field.field @"maybe'typeConName") _x
                 of
                   Prelude.Nothing -> Data.Monoid.mempty
                   (Prelude.Just _v)
                     -> (Data.Monoid.<>)
                          (Data.ProtoLens.Encoding.Bytes.putVarInt 18)
                          ((Prelude..)
                             (\ bs
                                -> (Data.Monoid.<>)
                                     (Data.ProtoLens.Encoding.Bytes.putVarInt
                                        (Prelude.fromIntegral (Data.ByteString.length bs)))
                                     (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                             Data.ProtoLens.encodeMessage _v))
                ((Data.Monoid.<>)
                   (case
                        Lens.Family2.view
                          (Data.ProtoLens.Field.field @"maybe'sourceInfo") _x
                    of
                      Prelude.Nothing -> Data.Monoid.mempty
                      (Prelude.Just _v)
                        -> (Data.Monoid.<>)
                             (Data.ProtoLens.Encoding.Bytes.putVarInt 26)
                             ((Prelude..)
                                (\ bs
                                   -> (Data.Monoid.<>)
                                        (Data.ProtoLens.Encoding.Bytes.putVarInt
                                           (Prelude.fromIntegral (Data.ByteString.length bs)))
                                        (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                                Data.ProtoLens.encodeMessage _v))
                   (Data.ProtoLens.Encoding.Wire.buildFieldSet
                      (Lens.Family2.view Data.ProtoLens.unknownFields _x))))
instance Control.DeepSeq.NFData ForeignTypeRef where
  rnf
    = \ x__
        -> Control.DeepSeq.deepseq
             (_ForeignTypeRef'_unknownFields x__)
             (Control.DeepSeq.deepseq
                (_ForeignTypeRef'moduleName x__)
                (Control.DeepSeq.deepseq
                   (_ForeignTypeRef'typeConName x__)
                   (Control.DeepSeq.deepseq (_ForeignTypeRef'sourceInfo x__) ())))
{- | Fields :
     
         * 'Proto.Repls.LambdabuffersSource_Fields.typeclassName' @:: Lens' InstanceClause TypeclassName@
         * 'Proto.Repls.LambdabuffersSource_Fields.maybe'typeclassName' @:: Lens' InstanceClause (Prelude.Maybe TypeclassName)@
         * 'Proto.Repls.LambdabuffersSource_Fields.head' @:: Lens' InstanceClause Type@
         * 'Proto.Repls.LambdabuffersSource_Fields.maybe'head' @:: Lens' InstanceClause (Prelude.Maybe Type)@
         * 'Proto.Repls.LambdabuffersSource_Fields.constraints' @:: Lens' InstanceClause [Constraint]@
         * 'Proto.Repls.LambdabuffersSource_Fields.vec'constraints' @:: Lens' InstanceClause (Data.Vector.Vector Constraint)@
         * 'Proto.Repls.LambdabuffersSource_Fields.sourceInfo' @:: Lens' InstanceClause SourceInfo@
         * 'Proto.Repls.LambdabuffersSource_Fields.maybe'sourceInfo' @:: Lens' InstanceClause (Prelude.Maybe SourceInfo)@ -}
data InstanceClause
  = InstanceClause'_constructor {_InstanceClause'typeclassName :: !(Prelude.Maybe TypeclassName),
                                 _InstanceClause'head :: !(Prelude.Maybe Type),
                                 _InstanceClause'constraints :: !(Data.Vector.Vector Constraint),
                                 _InstanceClause'sourceInfo :: !(Prelude.Maybe SourceInfo),
                                 _InstanceClause'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving stock (Prelude.Eq, Prelude.Ord)
instance Prelude.Show InstanceClause where
  showsPrec _ __x __s
    = Prelude.showChar
        '{'
        (Prelude.showString
           (Data.ProtoLens.showMessageShort __x) (Prelude.showChar '}' __s))
instance Data.ProtoLens.Field.HasField InstanceClause "typeclassName" TypeclassName where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _InstanceClause'typeclassName
           (\ x__ y__ -> x__ {_InstanceClause'typeclassName = y__}))
        (Data.ProtoLens.maybeLens Data.ProtoLens.defMessage)
instance Data.ProtoLens.Field.HasField InstanceClause "maybe'typeclassName" (Prelude.Maybe TypeclassName) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _InstanceClause'typeclassName
           (\ x__ y__ -> x__ {_InstanceClause'typeclassName = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField InstanceClause "head" Type where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _InstanceClause'head
           (\ x__ y__ -> x__ {_InstanceClause'head = y__}))
        (Data.ProtoLens.maybeLens Data.ProtoLens.defMessage)
instance Data.ProtoLens.Field.HasField InstanceClause "maybe'head" (Prelude.Maybe Type) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _InstanceClause'head
           (\ x__ y__ -> x__ {_InstanceClause'head = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField InstanceClause "constraints" [Constraint] where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _InstanceClause'constraints
           (\ x__ y__ -> x__ {_InstanceClause'constraints = y__}))
        (Lens.Family2.Unchecked.lens
           Data.Vector.Generic.toList
           (\ _ y__ -> Data.Vector.Generic.fromList y__))
instance Data.ProtoLens.Field.HasField InstanceClause "vec'constraints" (Data.Vector.Vector Constraint) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _InstanceClause'constraints
           (\ x__ y__ -> x__ {_InstanceClause'constraints = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField InstanceClause "sourceInfo" SourceInfo where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _InstanceClause'sourceInfo
           (\ x__ y__ -> x__ {_InstanceClause'sourceInfo = y__}))
        (Data.ProtoLens.maybeLens Data.ProtoLens.defMessage)
instance Data.ProtoLens.Field.HasField InstanceClause "maybe'sourceInfo" (Prelude.Maybe SourceInfo) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _InstanceClause'sourceInfo
           (\ x__ y__ -> x__ {_InstanceClause'sourceInfo = y__}))
        Prelude.id
instance Data.ProtoLens.Message InstanceClause where
  messageName _
    = Data.Text.pack "lambdabuffers.source.InstanceClause"
  packedMessageDescriptor _
    = "\n\
      \\SOInstanceClause\DC2J\n\
      \\SOtypeclass_name\CAN\SOH \SOH(\v2#.lambdabuffers.source.TypeclassNameR\rtypeclassName\DC2.\n\
      \\EOThead\CAN\STX \SOH(\v2\SUB.lambdabuffers.source.TypeR\EOThead\DC2B\n\
      \\vconstraints\CAN\ETX \ETX(\v2 .lambdabuffers.source.ConstraintR\vconstraints\DC2A\n\
      \\vsource_info\CAN\EOT \SOH(\v2 .lambdabuffers.source.SourceInfoR\n\
      \sourceInfo"
  packedFileDescriptor _ = packedFileDescriptor
  fieldsByTag
    = let
        typeclassName__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "typeclass_name"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor TypeclassName)
              (Data.ProtoLens.OptionalField
                 (Data.ProtoLens.Field.field @"maybe'typeclassName")) ::
              Data.ProtoLens.FieldDescriptor InstanceClause
        head__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "head"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor Type)
              (Data.ProtoLens.OptionalField
                 (Data.ProtoLens.Field.field @"maybe'head")) ::
              Data.ProtoLens.FieldDescriptor InstanceClause
        constraints__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "constraints"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor Constraint)
              (Data.ProtoLens.RepeatedField
                 Data.ProtoLens.Unpacked
                 (Data.ProtoLens.Field.field @"constraints")) ::
              Data.ProtoLens.FieldDescriptor InstanceClause
        sourceInfo__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "source_info"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor SourceInfo)
              (Data.ProtoLens.OptionalField
                 (Data.ProtoLens.Field.field @"maybe'sourceInfo")) ::
              Data.ProtoLens.FieldDescriptor InstanceClause
      in
        Data.Map.fromList
          [(Data.ProtoLens.Tag 1, typeclassName__field_descriptor),
           (Data.ProtoLens.Tag 2, head__field_descriptor),
           (Data.ProtoLens.Tag 3, constraints__field_descriptor),
           (Data.ProtoLens.Tag 4, sourceInfo__field_descriptor)]
  unknownFields
    = Lens.Family2.Unchecked.lens
        _InstanceClause'_unknownFields
        (\ x__ y__ -> x__ {_InstanceClause'_unknownFields = y__})
  defMessage
    = InstanceClause'_constructor
        {_InstanceClause'typeclassName = Prelude.Nothing,
         _InstanceClause'head = Prelude.Nothing,
         _InstanceClause'constraints = Data.Vector.Generic.empty,
         _InstanceClause'sourceInfo = Prelude.Nothing,
         _InstanceClause'_unknownFields = []}
  parseMessage
    = let
        loop ::
          InstanceClause
          -> Data.ProtoLens.Encoding.Growing.Growing Data.Vector.Vector Data.ProtoLens.Encoding.Growing.RealWorld Constraint
             -> Data.ProtoLens.Encoding.Bytes.Parser InstanceClause
        loop x mutable'constraints
          = do end <- Data.ProtoLens.Encoding.Bytes.atEnd
               if end then
                   do frozen'constraints <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                              (Data.ProtoLens.Encoding.Growing.unsafeFreeze
                                                 mutable'constraints)
                      (let missing = []
                       in
                         if Prelude.null missing then
                             Prelude.return ()
                         else
                             Prelude.fail
                               ((Prelude.++)
                                  "Missing required fields: "
                                  (Prelude.show (missing :: [Prelude.String]))))
                      Prelude.return
                        (Lens.Family2.over
                           Data.ProtoLens.unknownFields (\ !t -> Prelude.reverse t)
                           (Lens.Family2.set
                              (Data.ProtoLens.Field.field @"vec'constraints") frozen'constraints
                              x))
               else
                   do tag <- Data.ProtoLens.Encoding.Bytes.getVarInt
                      case tag of
                        10
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                           Data.ProtoLens.Encoding.Bytes.isolate
                                             (Prelude.fromIntegral len) Data.ProtoLens.parseMessage)
                                       "typeclass_name"
                                loop
                                  (Lens.Family2.set
                                     (Data.ProtoLens.Field.field @"typeclassName") y x)
                                  mutable'constraints
                        18
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                           Data.ProtoLens.Encoding.Bytes.isolate
                                             (Prelude.fromIntegral len) Data.ProtoLens.parseMessage)
                                       "head"
                                loop
                                  (Lens.Family2.set (Data.ProtoLens.Field.field @"head") y x)
                                  mutable'constraints
                        26
                          -> do !y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                        (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                            Data.ProtoLens.Encoding.Bytes.isolate
                                              (Prelude.fromIntegral len)
                                              Data.ProtoLens.parseMessage)
                                        "constraints"
                                v <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                       (Data.ProtoLens.Encoding.Growing.append
                                          mutable'constraints y)
                                loop x v
                        34
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                           Data.ProtoLens.Encoding.Bytes.isolate
                                             (Prelude.fromIntegral len) Data.ProtoLens.parseMessage)
                                       "source_info"
                                loop
                                  (Lens.Family2.set (Data.ProtoLens.Field.field @"sourceInfo") y x)
                                  mutable'constraints
                        wire
                          -> do !y <- Data.ProtoLens.Encoding.Wire.parseTaggedValueFromWire
                                        wire
                                loop
                                  (Lens.Family2.over
                                     Data.ProtoLens.unknownFields (\ !t -> (:) y t) x)
                                  mutable'constraints
      in
        (Data.ProtoLens.Encoding.Bytes.<?>)
          (do mutable'constraints <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                       Data.ProtoLens.Encoding.Growing.new
              loop Data.ProtoLens.defMessage mutable'constraints)
          "InstanceClause"
  buildMessage
    = \ _x
        -> (Data.Monoid.<>)
             (case
                  Lens.Family2.view
                    (Data.ProtoLens.Field.field @"maybe'typeclassName") _x
              of
                Prelude.Nothing -> Data.Monoid.mempty
                (Prelude.Just _v)
                  -> (Data.Monoid.<>)
                       (Data.ProtoLens.Encoding.Bytes.putVarInt 10)
                       ((Prelude..)
                          (\ bs
                             -> (Data.Monoid.<>)
                                  (Data.ProtoLens.Encoding.Bytes.putVarInt
                                     (Prelude.fromIntegral (Data.ByteString.length bs)))
                                  (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                          Data.ProtoLens.encodeMessage _v))
             ((Data.Monoid.<>)
                (case
                     Lens.Family2.view (Data.ProtoLens.Field.field @"maybe'head") _x
                 of
                   Prelude.Nothing -> Data.Monoid.mempty
                   (Prelude.Just _v)
                     -> (Data.Monoid.<>)
                          (Data.ProtoLens.Encoding.Bytes.putVarInt 18)
                          ((Prelude..)
                             (\ bs
                                -> (Data.Monoid.<>)
                                     (Data.ProtoLens.Encoding.Bytes.putVarInt
                                        (Prelude.fromIntegral (Data.ByteString.length bs)))
                                     (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                             Data.ProtoLens.encodeMessage _v))
                ((Data.Monoid.<>)
                   (Data.ProtoLens.Encoding.Bytes.foldMapBuilder
                      (\ _v
                         -> (Data.Monoid.<>)
                              (Data.ProtoLens.Encoding.Bytes.putVarInt 26)
                              ((Prelude..)
                                 (\ bs
                                    -> (Data.Monoid.<>)
                                         (Data.ProtoLens.Encoding.Bytes.putVarInt
                                            (Prelude.fromIntegral (Data.ByteString.length bs)))
                                         (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                                 Data.ProtoLens.encodeMessage _v))
                      (Lens.Family2.view
                         (Data.ProtoLens.Field.field @"vec'constraints") _x))
                   ((Data.Monoid.<>)
                      (case
                           Lens.Family2.view
                             (Data.ProtoLens.Field.field @"maybe'sourceInfo") _x
                       of
                         Prelude.Nothing -> Data.Monoid.mempty
                         (Prelude.Just _v)
                           -> (Data.Monoid.<>)
                                (Data.ProtoLens.Encoding.Bytes.putVarInt 34)
                                ((Prelude..)
                                   (\ bs
                                      -> (Data.Monoid.<>)
                                           (Data.ProtoLens.Encoding.Bytes.putVarInt
                                              (Prelude.fromIntegral (Data.ByteString.length bs)))
                                           (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                                   Data.ProtoLens.encodeMessage _v))
                      (Data.ProtoLens.Encoding.Wire.buildFieldSet
                         (Lens.Family2.view Data.ProtoLens.unknownFields _x)))))
instance Control.DeepSeq.NFData InstanceClause where
  rnf
    = \ x__
        -> Control.DeepSeq.deepseq
             (_InstanceClause'_unknownFields x__)
             (Control.DeepSeq.deepseq
                (_InstanceClause'typeclassName x__)
                (Control.DeepSeq.deepseq
                   (_InstanceClause'head x__)
                   (Control.DeepSeq.deepseq
                      (_InstanceClause'constraints x__)
                      (Control.DeepSeq.deepseq (_InstanceClause'sourceInfo x__) ()))))
{- | Fields :
     
         * 'Proto.Repls.LambdabuffersSource_Fields.varName' @:: Lens' Kind [Data.Text.Text]@
         * 'Proto.Repls.LambdabuffersSource_Fields.vec'varName' @:: Lens' Kind (Data.Vector.Vector Data.Text.Text)@
         * 'Proto.Repls.LambdabuffersSource_Fields.sourceInfo' @:: Lens' Kind SourceInfo@
         * 'Proto.Repls.LambdabuffersSource_Fields.maybe'sourceInfo' @:: Lens' Kind (Prelude.Maybe SourceInfo)@ -}
data Kind
  = Kind'_constructor {_Kind'varName :: !(Data.Vector.Vector Data.Text.Text),
                       _Kind'sourceInfo :: !(Prelude.Maybe SourceInfo),
                       _Kind'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving stock (Prelude.Eq, Prelude.Ord)
instance Prelude.Show Kind where
  showsPrec _ __x __s
    = Prelude.showChar
        '{'
        (Prelude.showString
           (Data.ProtoLens.showMessageShort __x) (Prelude.showChar '}' __s))
instance Data.ProtoLens.Field.HasField Kind "varName" [Data.Text.Text] where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _Kind'varName (\ x__ y__ -> x__ {_Kind'varName = y__}))
        (Lens.Family2.Unchecked.lens
           Data.Vector.Generic.toList
           (\ _ y__ -> Data.Vector.Generic.fromList y__))
instance Data.ProtoLens.Field.HasField Kind "vec'varName" (Data.Vector.Vector Data.Text.Text) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _Kind'varName (\ x__ y__ -> x__ {_Kind'varName = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField Kind "sourceInfo" SourceInfo where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _Kind'sourceInfo (\ x__ y__ -> x__ {_Kind'sourceInfo = y__}))
        (Data.ProtoLens.maybeLens Data.ProtoLens.defMessage)
instance Data.ProtoLens.Field.HasField Kind "maybe'sourceInfo" (Prelude.Maybe SourceInfo) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _Kind'sourceInfo (\ x__ y__ -> x__ {_Kind'sourceInfo = y__}))
        Prelude.id
instance Data.ProtoLens.Message Kind where
  messageName _ = Data.Text.pack "lambdabuffers.source.Kind"
  packedMessageDescriptor _
    = "\n\
      \\EOTKind\DC2\CAN\n\
      \\aVarName\CAN\SOH \ETX(\tR\aVarName\DC2A\n\
      \\vsource_info\CAN\STX \SOH(\v2 .lambdabuffers.source.SourceInfoR\n\
      \sourceInfo"
  packedFileDescriptor _ = packedFileDescriptor
  fieldsByTag
    = let
        varName__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "VarName"
              (Data.ProtoLens.ScalarField Data.ProtoLens.StringField ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Text.Text)
              (Data.ProtoLens.RepeatedField
                 Data.ProtoLens.Unpacked (Data.ProtoLens.Field.field @"varName")) ::
              Data.ProtoLens.FieldDescriptor Kind
        sourceInfo__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "source_info"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor SourceInfo)
              (Data.ProtoLens.OptionalField
                 (Data.ProtoLens.Field.field @"maybe'sourceInfo")) ::
              Data.ProtoLens.FieldDescriptor Kind
      in
        Data.Map.fromList
          [(Data.ProtoLens.Tag 1, varName__field_descriptor),
           (Data.ProtoLens.Tag 2, sourceInfo__field_descriptor)]
  unknownFields
    = Lens.Family2.Unchecked.lens
        _Kind'_unknownFields
        (\ x__ y__ -> x__ {_Kind'_unknownFields = y__})
  defMessage
    = Kind'_constructor
        {_Kind'varName = Data.Vector.Generic.empty,
         _Kind'sourceInfo = Prelude.Nothing, _Kind'_unknownFields = []}
  parseMessage
    = let
        loop ::
          Kind
          -> Data.ProtoLens.Encoding.Growing.Growing Data.Vector.Vector Data.ProtoLens.Encoding.Growing.RealWorld Data.Text.Text
             -> Data.ProtoLens.Encoding.Bytes.Parser Kind
        loop x mutable'varName
          = do end <- Data.ProtoLens.Encoding.Bytes.atEnd
               if end then
                   do frozen'varName <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                          (Data.ProtoLens.Encoding.Growing.unsafeFreeze
                                             mutable'varName)
                      (let missing = []
                       in
                         if Prelude.null missing then
                             Prelude.return ()
                         else
                             Prelude.fail
                               ((Prelude.++)
                                  "Missing required fields: "
                                  (Prelude.show (missing :: [Prelude.String]))))
                      Prelude.return
                        (Lens.Family2.over
                           Data.ProtoLens.unknownFields (\ !t -> Prelude.reverse t)
                           (Lens.Family2.set
                              (Data.ProtoLens.Field.field @"vec'varName") frozen'varName x))
               else
                   do tag <- Data.ProtoLens.Encoding.Bytes.getVarInt
                      case tag of
                        10
                          -> do !y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                        (do value <- do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                                        Data.ProtoLens.Encoding.Bytes.getBytes
                                                          (Prelude.fromIntegral len)
                                            Data.ProtoLens.Encoding.Bytes.runEither
                                              (case Data.Text.Encoding.decodeUtf8' value of
                                                 (Prelude.Left err)
                                                   -> Prelude.Left (Prelude.show err)
                                                 (Prelude.Right r) -> Prelude.Right r))
                                        "VarName"
                                v <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                       (Data.ProtoLens.Encoding.Growing.append mutable'varName y)
                                loop x v
                        18
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                           Data.ProtoLens.Encoding.Bytes.isolate
                                             (Prelude.fromIntegral len) Data.ProtoLens.parseMessage)
                                       "source_info"
                                loop
                                  (Lens.Family2.set (Data.ProtoLens.Field.field @"sourceInfo") y x)
                                  mutable'varName
                        wire
                          -> do !y <- Data.ProtoLens.Encoding.Wire.parseTaggedValueFromWire
                                        wire
                                loop
                                  (Lens.Family2.over
                                     Data.ProtoLens.unknownFields (\ !t -> (:) y t) x)
                                  mutable'varName
      in
        (Data.ProtoLens.Encoding.Bytes.<?>)
          (do mutable'varName <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                   Data.ProtoLens.Encoding.Growing.new
              loop Data.ProtoLens.defMessage mutable'varName)
          "Kind"
  buildMessage
    = \ _x
        -> (Data.Monoid.<>)
             (Data.ProtoLens.Encoding.Bytes.foldMapBuilder
                (\ _v
                   -> (Data.Monoid.<>)
                        (Data.ProtoLens.Encoding.Bytes.putVarInt 10)
                        ((Prelude..)
                           (\ bs
                              -> (Data.Monoid.<>)
                                   (Data.ProtoLens.Encoding.Bytes.putVarInt
                                      (Prelude.fromIntegral (Data.ByteString.length bs)))
                                   (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                           Data.Text.Encoding.encodeUtf8 _v))
                (Lens.Family2.view (Data.ProtoLens.Field.field @"vec'varName") _x))
             ((Data.Monoid.<>)
                (case
                     Lens.Family2.view
                       (Data.ProtoLens.Field.field @"maybe'sourceInfo") _x
                 of
                   Prelude.Nothing -> Data.Monoid.mempty
                   (Prelude.Just _v)
                     -> (Data.Monoid.<>)
                          (Data.ProtoLens.Encoding.Bytes.putVarInt 18)
                          ((Prelude..)
                             (\ bs
                                -> (Data.Monoid.<>)
                                     (Data.ProtoLens.Encoding.Bytes.putVarInt
                                        (Prelude.fromIntegral (Data.ByteString.length bs)))
                                     (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                             Data.ProtoLens.encodeMessage _v))
                (Data.ProtoLens.Encoding.Wire.buildFieldSet
                   (Lens.Family2.view Data.ProtoLens.unknownFields _x)))
instance Control.DeepSeq.NFData Kind where
  rnf
    = \ x__
        -> Control.DeepSeq.deepseq
             (_Kind'_unknownFields x__)
             (Control.DeepSeq.deepseq
                (_Kind'varName x__)
                (Control.DeepSeq.deepseq (_Kind'sourceInfo x__) ()))
{- | Fields :
     
         * 'Proto.Repls.LambdabuffersSource_Fields.typeConName' @:: Lens' LocalTypeRef TypeConName@
         * 'Proto.Repls.LambdabuffersSource_Fields.maybe'typeConName' @:: Lens' LocalTypeRef (Prelude.Maybe TypeConName)@
         * 'Proto.Repls.LambdabuffersSource_Fields.sourceInfo' @:: Lens' LocalTypeRef SourceInfo@
         * 'Proto.Repls.LambdabuffersSource_Fields.maybe'sourceInfo' @:: Lens' LocalTypeRef (Prelude.Maybe SourceInfo)@ -}
data LocalTypeRef
  = LocalTypeRef'_constructor {_LocalTypeRef'typeConName :: !(Prelude.Maybe TypeConName),
                               _LocalTypeRef'sourceInfo :: !(Prelude.Maybe SourceInfo),
                               _LocalTypeRef'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving stock (Prelude.Eq, Prelude.Ord)
instance Prelude.Show LocalTypeRef where
  showsPrec _ __x __s
    = Prelude.showChar
        '{'
        (Prelude.showString
           (Data.ProtoLens.showMessageShort __x) (Prelude.showChar '}' __s))
instance Data.ProtoLens.Field.HasField LocalTypeRef "typeConName" TypeConName where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _LocalTypeRef'typeConName
           (\ x__ y__ -> x__ {_LocalTypeRef'typeConName = y__}))
        (Data.ProtoLens.maybeLens Data.ProtoLens.defMessage)
instance Data.ProtoLens.Field.HasField LocalTypeRef "maybe'typeConName" (Prelude.Maybe TypeConName) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _LocalTypeRef'typeConName
           (\ x__ y__ -> x__ {_LocalTypeRef'typeConName = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField LocalTypeRef "sourceInfo" SourceInfo where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _LocalTypeRef'sourceInfo
           (\ x__ y__ -> x__ {_LocalTypeRef'sourceInfo = y__}))
        (Data.ProtoLens.maybeLens Data.ProtoLens.defMessage)
instance Data.ProtoLens.Field.HasField LocalTypeRef "maybe'sourceInfo" (Prelude.Maybe SourceInfo) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _LocalTypeRef'sourceInfo
           (\ x__ y__ -> x__ {_LocalTypeRef'sourceInfo = y__}))
        Prelude.id
instance Data.ProtoLens.Message LocalTypeRef where
  messageName _ = Data.Text.pack "lambdabuffers.source.LocalTypeRef"
  packedMessageDescriptor _
    = "\n\
      \\fLocalTypeRef\DC2E\n\
      \\rtype_con_name\CAN\SOH \SOH(\v2!.lambdabuffers.source.TypeConNameR\vtypeConName\DC2A\n\
      \\vsource_info\CAN\STX \SOH(\v2 .lambdabuffers.source.SourceInfoR\n\
      \sourceInfo"
  packedFileDescriptor _ = packedFileDescriptor
  fieldsByTag
    = let
        typeConName__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "type_con_name"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor TypeConName)
              (Data.ProtoLens.OptionalField
                 (Data.ProtoLens.Field.field @"maybe'typeConName")) ::
              Data.ProtoLens.FieldDescriptor LocalTypeRef
        sourceInfo__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "source_info"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor SourceInfo)
              (Data.ProtoLens.OptionalField
                 (Data.ProtoLens.Field.field @"maybe'sourceInfo")) ::
              Data.ProtoLens.FieldDescriptor LocalTypeRef
      in
        Data.Map.fromList
          [(Data.ProtoLens.Tag 1, typeConName__field_descriptor),
           (Data.ProtoLens.Tag 2, sourceInfo__field_descriptor)]
  unknownFields
    = Lens.Family2.Unchecked.lens
        _LocalTypeRef'_unknownFields
        (\ x__ y__ -> x__ {_LocalTypeRef'_unknownFields = y__})
  defMessage
    = LocalTypeRef'_constructor
        {_LocalTypeRef'typeConName = Prelude.Nothing,
         _LocalTypeRef'sourceInfo = Prelude.Nothing,
         _LocalTypeRef'_unknownFields = []}
  parseMessage
    = let
        loop ::
          LocalTypeRef -> Data.ProtoLens.Encoding.Bytes.Parser LocalTypeRef
        loop x
          = do end <- Data.ProtoLens.Encoding.Bytes.atEnd
               if end then
                   do (let missing = []
                       in
                         if Prelude.null missing then
                             Prelude.return ()
                         else
                             Prelude.fail
                               ((Prelude.++)
                                  "Missing required fields: "
                                  (Prelude.show (missing :: [Prelude.String]))))
                      Prelude.return
                        (Lens.Family2.over
                           Data.ProtoLens.unknownFields (\ !t -> Prelude.reverse t) x)
               else
                   do tag <- Data.ProtoLens.Encoding.Bytes.getVarInt
                      case tag of
                        10
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                           Data.ProtoLens.Encoding.Bytes.isolate
                                             (Prelude.fromIntegral len) Data.ProtoLens.parseMessage)
                                       "type_con_name"
                                loop
                                  (Lens.Family2.set (Data.ProtoLens.Field.field @"typeConName") y x)
                        18
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                           Data.ProtoLens.Encoding.Bytes.isolate
                                             (Prelude.fromIntegral len) Data.ProtoLens.parseMessage)
                                       "source_info"
                                loop
                                  (Lens.Family2.set (Data.ProtoLens.Field.field @"sourceInfo") y x)
                        wire
                          -> do !y <- Data.ProtoLens.Encoding.Wire.parseTaggedValueFromWire
                                        wire
                                loop
                                  (Lens.Family2.over
                                     Data.ProtoLens.unknownFields (\ !t -> (:) y t) x)
      in
        (Data.ProtoLens.Encoding.Bytes.<?>)
          (do loop Data.ProtoLens.defMessage) "LocalTypeRef"
  buildMessage
    = \ _x
        -> (Data.Monoid.<>)
             (case
                  Lens.Family2.view
                    (Data.ProtoLens.Field.field @"maybe'typeConName") _x
              of
                Prelude.Nothing -> Data.Monoid.mempty
                (Prelude.Just _v)
                  -> (Data.Monoid.<>)
                       (Data.ProtoLens.Encoding.Bytes.putVarInt 10)
                       ((Prelude..)
                          (\ bs
                             -> (Data.Monoid.<>)
                                  (Data.ProtoLens.Encoding.Bytes.putVarInt
                                     (Prelude.fromIntegral (Data.ByteString.length bs)))
                                  (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                          Data.ProtoLens.encodeMessage _v))
             ((Data.Monoid.<>)
                (case
                     Lens.Family2.view
                       (Data.ProtoLens.Field.field @"maybe'sourceInfo") _x
                 of
                   Prelude.Nothing -> Data.Monoid.mempty
                   (Prelude.Just _v)
                     -> (Data.Monoid.<>)
                          (Data.ProtoLens.Encoding.Bytes.putVarInt 18)
                          ((Prelude..)
                             (\ bs
                                -> (Data.Monoid.<>)
                                     (Data.ProtoLens.Encoding.Bytes.putVarInt
                                        (Prelude.fromIntegral (Data.ByteString.length bs)))
                                     (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                             Data.ProtoLens.encodeMessage _v))
                (Data.ProtoLens.Encoding.Wire.buildFieldSet
                   (Lens.Family2.view Data.ProtoLens.unknownFields _x)))
instance Control.DeepSeq.NFData LocalTypeRef where
  rnf
    = \ x__
        -> Control.DeepSeq.deepseq
             (_LocalTypeRef'_unknownFields x__)
             (Control.DeepSeq.deepseq
                (_LocalTypeRef'typeConName x__)
                (Control.DeepSeq.deepseq (_LocalTypeRef'sourceInfo x__) ()))
{- | Fields :
     
         * 'Proto.Repls.LambdabuffersSource_Fields.moduleName' @:: Lens' Module ModuleName@
         * 'Proto.Repls.LambdabuffersSource_Fields.maybe'moduleName' @:: Lens' Module (Prelude.Maybe ModuleName)@
         * 'Proto.Repls.LambdabuffersSource_Fields.typeDefs' @:: Lens' Module [TypeDefinition]@
         * 'Proto.Repls.LambdabuffersSource_Fields.vec'typeDefs' @:: Lens' Module (Data.Vector.Vector TypeDefinition)@
         * 'Proto.Repls.LambdabuffersSource_Fields.instances' @:: Lens' Module [InstanceClause]@
         * 'Proto.Repls.LambdabuffersSource_Fields.vec'instances' @:: Lens' Module (Data.Vector.Vector InstanceClause)@
         * 'Proto.Repls.LambdabuffersSource_Fields.sourceInfo' @:: Lens' Module SourceInfo@
         * 'Proto.Repls.LambdabuffersSource_Fields.maybe'sourceInfo' @:: Lens' Module (Prelude.Maybe SourceInfo)@ -}
data Module
  = Module'_constructor {_Module'moduleName :: !(Prelude.Maybe ModuleName),
                         _Module'typeDefs :: !(Data.Vector.Vector TypeDefinition),
                         _Module'instances :: !(Data.Vector.Vector InstanceClause),
                         _Module'sourceInfo :: !(Prelude.Maybe SourceInfo),
                         _Module'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving stock (Prelude.Eq, Prelude.Ord)
instance Prelude.Show Module where
  showsPrec _ __x __s
    = Prelude.showChar
        '{'
        (Prelude.showString
           (Data.ProtoLens.showMessageShort __x) (Prelude.showChar '}' __s))
instance Data.ProtoLens.Field.HasField Module "moduleName" ModuleName where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _Module'moduleName (\ x__ y__ -> x__ {_Module'moduleName = y__}))
        (Data.ProtoLens.maybeLens Data.ProtoLens.defMessage)
instance Data.ProtoLens.Field.HasField Module "maybe'moduleName" (Prelude.Maybe ModuleName) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _Module'moduleName (\ x__ y__ -> x__ {_Module'moduleName = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField Module "typeDefs" [TypeDefinition] where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _Module'typeDefs (\ x__ y__ -> x__ {_Module'typeDefs = y__}))
        (Lens.Family2.Unchecked.lens
           Data.Vector.Generic.toList
           (\ _ y__ -> Data.Vector.Generic.fromList y__))
instance Data.ProtoLens.Field.HasField Module "vec'typeDefs" (Data.Vector.Vector TypeDefinition) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _Module'typeDefs (\ x__ y__ -> x__ {_Module'typeDefs = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField Module "instances" [InstanceClause] where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _Module'instances (\ x__ y__ -> x__ {_Module'instances = y__}))
        (Lens.Family2.Unchecked.lens
           Data.Vector.Generic.toList
           (\ _ y__ -> Data.Vector.Generic.fromList y__))
instance Data.ProtoLens.Field.HasField Module "vec'instances" (Data.Vector.Vector InstanceClause) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _Module'instances (\ x__ y__ -> x__ {_Module'instances = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField Module "sourceInfo" SourceInfo where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _Module'sourceInfo (\ x__ y__ -> x__ {_Module'sourceInfo = y__}))
        (Data.ProtoLens.maybeLens Data.ProtoLens.defMessage)
instance Data.ProtoLens.Field.HasField Module "maybe'sourceInfo" (Prelude.Maybe SourceInfo) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _Module'sourceInfo (\ x__ y__ -> x__ {_Module'sourceInfo = y__}))
        Prelude.id
instance Data.ProtoLens.Message Module where
  messageName _ = Data.Text.pack "lambdabuffers.source.Module"
  packedMessageDescriptor _
    = "\n\
      \\ACKModule\DC2A\n\
      \\vmodule_name\CAN\SOH \SOH(\v2 .lambdabuffers.source.ModuleNameR\n\
      \moduleName\DC2A\n\
      \\ttype_defs\CAN\STX \ETX(\v2$.lambdabuffers.source.TypeDefinitionR\btypeDefs\DC2B\n\
      \\tinstances\CAN\ETX \ETX(\v2$.lambdabuffers.source.InstanceClauseR\tinstances\DC2A\n\
      \\vsource_info\CAN\EOT \SOH(\v2 .lambdabuffers.source.SourceInfoR\n\
      \sourceInfo"
  packedFileDescriptor _ = packedFileDescriptor
  fieldsByTag
    = let
        moduleName__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "module_name"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor ModuleName)
              (Data.ProtoLens.OptionalField
                 (Data.ProtoLens.Field.field @"maybe'moduleName")) ::
              Data.ProtoLens.FieldDescriptor Module
        typeDefs__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "type_defs"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor TypeDefinition)
              (Data.ProtoLens.RepeatedField
                 Data.ProtoLens.Unpacked
                 (Data.ProtoLens.Field.field @"typeDefs")) ::
              Data.ProtoLens.FieldDescriptor Module
        instances__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "instances"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor InstanceClause)
              (Data.ProtoLens.RepeatedField
                 Data.ProtoLens.Unpacked
                 (Data.ProtoLens.Field.field @"instances")) ::
              Data.ProtoLens.FieldDescriptor Module
        sourceInfo__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "source_info"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor SourceInfo)
              (Data.ProtoLens.OptionalField
                 (Data.ProtoLens.Field.field @"maybe'sourceInfo")) ::
              Data.ProtoLens.FieldDescriptor Module
      in
        Data.Map.fromList
          [(Data.ProtoLens.Tag 1, moduleName__field_descriptor),
           (Data.ProtoLens.Tag 2, typeDefs__field_descriptor),
           (Data.ProtoLens.Tag 3, instances__field_descriptor),
           (Data.ProtoLens.Tag 4, sourceInfo__field_descriptor)]
  unknownFields
    = Lens.Family2.Unchecked.lens
        _Module'_unknownFields
        (\ x__ y__ -> x__ {_Module'_unknownFields = y__})
  defMessage
    = Module'_constructor
        {_Module'moduleName = Prelude.Nothing,
         _Module'typeDefs = Data.Vector.Generic.empty,
         _Module'instances = Data.Vector.Generic.empty,
         _Module'sourceInfo = Prelude.Nothing, _Module'_unknownFields = []}
  parseMessage
    = let
        loop ::
          Module
          -> Data.ProtoLens.Encoding.Growing.Growing Data.Vector.Vector Data.ProtoLens.Encoding.Growing.RealWorld InstanceClause
             -> Data.ProtoLens.Encoding.Growing.Growing Data.Vector.Vector Data.ProtoLens.Encoding.Growing.RealWorld TypeDefinition
                -> Data.ProtoLens.Encoding.Bytes.Parser Module
        loop x mutable'instances mutable'typeDefs
          = do end <- Data.ProtoLens.Encoding.Bytes.atEnd
               if end then
                   do frozen'instances <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                            (Data.ProtoLens.Encoding.Growing.unsafeFreeze
                                               mutable'instances)
                      frozen'typeDefs <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                           (Data.ProtoLens.Encoding.Growing.unsafeFreeze
                                              mutable'typeDefs)
                      (let missing = []
                       in
                         if Prelude.null missing then
                             Prelude.return ()
                         else
                             Prelude.fail
                               ((Prelude.++)
                                  "Missing required fields: "
                                  (Prelude.show (missing :: [Prelude.String]))))
                      Prelude.return
                        (Lens.Family2.over
                           Data.ProtoLens.unknownFields (\ !t -> Prelude.reverse t)
                           (Lens.Family2.set
                              (Data.ProtoLens.Field.field @"vec'instances") frozen'instances
                              (Lens.Family2.set
                                 (Data.ProtoLens.Field.field @"vec'typeDefs") frozen'typeDefs x)))
               else
                   do tag <- Data.ProtoLens.Encoding.Bytes.getVarInt
                      case tag of
                        10
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                           Data.ProtoLens.Encoding.Bytes.isolate
                                             (Prelude.fromIntegral len) Data.ProtoLens.parseMessage)
                                       "module_name"
                                loop
                                  (Lens.Family2.set (Data.ProtoLens.Field.field @"moduleName") y x)
                                  mutable'instances mutable'typeDefs
                        18
                          -> do !y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                        (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                            Data.ProtoLens.Encoding.Bytes.isolate
                                              (Prelude.fromIntegral len)
                                              Data.ProtoLens.parseMessage)
                                        "type_defs"
                                v <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                       (Data.ProtoLens.Encoding.Growing.append mutable'typeDefs y)
                                loop x mutable'instances v
                        26
                          -> do !y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                        (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                            Data.ProtoLens.Encoding.Bytes.isolate
                                              (Prelude.fromIntegral len)
                                              Data.ProtoLens.parseMessage)
                                        "instances"
                                v <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                       (Data.ProtoLens.Encoding.Growing.append mutable'instances y)
                                loop x v mutable'typeDefs
                        34
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                           Data.ProtoLens.Encoding.Bytes.isolate
                                             (Prelude.fromIntegral len) Data.ProtoLens.parseMessage)
                                       "source_info"
                                loop
                                  (Lens.Family2.set (Data.ProtoLens.Field.field @"sourceInfo") y x)
                                  mutable'instances mutable'typeDefs
                        wire
                          -> do !y <- Data.ProtoLens.Encoding.Wire.parseTaggedValueFromWire
                                        wire
                                loop
                                  (Lens.Family2.over
                                     Data.ProtoLens.unknownFields (\ !t -> (:) y t) x)
                                  mutable'instances mutable'typeDefs
      in
        (Data.ProtoLens.Encoding.Bytes.<?>)
          (do mutable'instances <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                     Data.ProtoLens.Encoding.Growing.new
              mutable'typeDefs <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                    Data.ProtoLens.Encoding.Growing.new
              loop Data.ProtoLens.defMessage mutable'instances mutable'typeDefs)
          "Module"
  buildMessage
    = \ _x
        -> (Data.Monoid.<>)
             (case
                  Lens.Family2.view
                    (Data.ProtoLens.Field.field @"maybe'moduleName") _x
              of
                Prelude.Nothing -> Data.Monoid.mempty
                (Prelude.Just _v)
                  -> (Data.Monoid.<>)
                       (Data.ProtoLens.Encoding.Bytes.putVarInt 10)
                       ((Prelude..)
                          (\ bs
                             -> (Data.Monoid.<>)
                                  (Data.ProtoLens.Encoding.Bytes.putVarInt
                                     (Prelude.fromIntegral (Data.ByteString.length bs)))
                                  (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                          Data.ProtoLens.encodeMessage _v))
             ((Data.Monoid.<>)
                (Data.ProtoLens.Encoding.Bytes.foldMapBuilder
                   (\ _v
                      -> (Data.Monoid.<>)
                           (Data.ProtoLens.Encoding.Bytes.putVarInt 18)
                           ((Prelude..)
                              (\ bs
                                 -> (Data.Monoid.<>)
                                      (Data.ProtoLens.Encoding.Bytes.putVarInt
                                         (Prelude.fromIntegral (Data.ByteString.length bs)))
                                      (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                              Data.ProtoLens.encodeMessage _v))
                   (Lens.Family2.view
                      (Data.ProtoLens.Field.field @"vec'typeDefs") _x))
                ((Data.Monoid.<>)
                   (Data.ProtoLens.Encoding.Bytes.foldMapBuilder
                      (\ _v
                         -> (Data.Monoid.<>)
                              (Data.ProtoLens.Encoding.Bytes.putVarInt 26)
                              ((Prelude..)
                                 (\ bs
                                    -> (Data.Monoid.<>)
                                         (Data.ProtoLens.Encoding.Bytes.putVarInt
                                            (Prelude.fromIntegral (Data.ByteString.length bs)))
                                         (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                                 Data.ProtoLens.encodeMessage _v))
                      (Lens.Family2.view
                         (Data.ProtoLens.Field.field @"vec'instances") _x))
                   ((Data.Monoid.<>)
                      (case
                           Lens.Family2.view
                             (Data.ProtoLens.Field.field @"maybe'sourceInfo") _x
                       of
                         Prelude.Nothing -> Data.Monoid.mempty
                         (Prelude.Just _v)
                           -> (Data.Monoid.<>)
                                (Data.ProtoLens.Encoding.Bytes.putVarInt 34)
                                ((Prelude..)
                                   (\ bs
                                      -> (Data.Monoid.<>)
                                           (Data.ProtoLens.Encoding.Bytes.putVarInt
                                              (Prelude.fromIntegral (Data.ByteString.length bs)))
                                           (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                                   Data.ProtoLens.encodeMessage _v))
                      (Data.ProtoLens.Encoding.Wire.buildFieldSet
                         (Lens.Family2.view Data.ProtoLens.unknownFields _x)))))
instance Control.DeepSeq.NFData Module where
  rnf
    = \ x__
        -> Control.DeepSeq.deepseq
             (_Module'_unknownFields x__)
             (Control.DeepSeq.deepseq
                (_Module'moduleName x__)
                (Control.DeepSeq.deepseq
                   (_Module'typeDefs x__)
                   (Control.DeepSeq.deepseq
                      (_Module'instances x__)
                      (Control.DeepSeq.deepseq (_Module'sourceInfo x__) ()))))
{- | Fields :
     
         * 'Proto.Repls.LambdabuffersSource_Fields.name' @:: Lens' ModuleName Data.Text.Text@
         * 'Proto.Repls.LambdabuffersSource_Fields.sourceInfo' @:: Lens' ModuleName SourceInfo@
         * 'Proto.Repls.LambdabuffersSource_Fields.maybe'sourceInfo' @:: Lens' ModuleName (Prelude.Maybe SourceInfo)@ -}
data ModuleName
  = ModuleName'_constructor {_ModuleName'name :: !Data.Text.Text,
                             _ModuleName'sourceInfo :: !(Prelude.Maybe SourceInfo),
                             _ModuleName'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving stock (Prelude.Eq, Prelude.Ord)
instance Prelude.Show ModuleName where
  showsPrec _ __x __s
    = Prelude.showChar
        '{'
        (Prelude.showString
           (Data.ProtoLens.showMessageShort __x) (Prelude.showChar '}' __s))
instance Data.ProtoLens.Field.HasField ModuleName "name" Data.Text.Text where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ModuleName'name (\ x__ y__ -> x__ {_ModuleName'name = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField ModuleName "sourceInfo" SourceInfo where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ModuleName'sourceInfo
           (\ x__ y__ -> x__ {_ModuleName'sourceInfo = y__}))
        (Data.ProtoLens.maybeLens Data.ProtoLens.defMessage)
instance Data.ProtoLens.Field.HasField ModuleName "maybe'sourceInfo" (Prelude.Maybe SourceInfo) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ModuleName'sourceInfo
           (\ x__ y__ -> x__ {_ModuleName'sourceInfo = y__}))
        Prelude.id
instance Data.ProtoLens.Message ModuleName where
  messageName _ = Data.Text.pack "lambdabuffers.source.ModuleName"
  packedMessageDescriptor _
    = "\n\
      \\n\
      \ModuleName\DC2\DC2\n\
      \\EOTname\CAN\SOH \SOH(\tR\EOTname\DC2A\n\
      \\vsource_info\CAN\STX \SOH(\v2 .lambdabuffers.source.SourceInfoR\n\
      \sourceInfo"
  packedFileDescriptor _ = packedFileDescriptor
  fieldsByTag
    = let
        name__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "name"
              (Data.ProtoLens.ScalarField Data.ProtoLens.StringField ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Text.Text)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional (Data.ProtoLens.Field.field @"name")) ::
              Data.ProtoLens.FieldDescriptor ModuleName
        sourceInfo__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "source_info"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor SourceInfo)
              (Data.ProtoLens.OptionalField
                 (Data.ProtoLens.Field.field @"maybe'sourceInfo")) ::
              Data.ProtoLens.FieldDescriptor ModuleName
      in
        Data.Map.fromList
          [(Data.ProtoLens.Tag 1, name__field_descriptor),
           (Data.ProtoLens.Tag 2, sourceInfo__field_descriptor)]
  unknownFields
    = Lens.Family2.Unchecked.lens
        _ModuleName'_unknownFields
        (\ x__ y__ -> x__ {_ModuleName'_unknownFields = y__})
  defMessage
    = ModuleName'_constructor
        {_ModuleName'name = Data.ProtoLens.fieldDefault,
         _ModuleName'sourceInfo = Prelude.Nothing,
         _ModuleName'_unknownFields = []}
  parseMessage
    = let
        loop ::
          ModuleName -> Data.ProtoLens.Encoding.Bytes.Parser ModuleName
        loop x
          = do end <- Data.ProtoLens.Encoding.Bytes.atEnd
               if end then
                   do (let missing = []
                       in
                         if Prelude.null missing then
                             Prelude.return ()
                         else
                             Prelude.fail
                               ((Prelude.++)
                                  "Missing required fields: "
                                  (Prelude.show (missing :: [Prelude.String]))))
                      Prelude.return
                        (Lens.Family2.over
                           Data.ProtoLens.unknownFields (\ !t -> Prelude.reverse t) x)
               else
                   do tag <- Data.ProtoLens.Encoding.Bytes.getVarInt
                      case tag of
                        10
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do value <- do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                                       Data.ProtoLens.Encoding.Bytes.getBytes
                                                         (Prelude.fromIntegral len)
                                           Data.ProtoLens.Encoding.Bytes.runEither
                                             (case Data.Text.Encoding.decodeUtf8' value of
                                                (Prelude.Left err)
                                                  -> Prelude.Left (Prelude.show err)
                                                (Prelude.Right r) -> Prelude.Right r))
                                       "name"
                                loop (Lens.Family2.set (Data.ProtoLens.Field.field @"name") y x)
                        18
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                           Data.ProtoLens.Encoding.Bytes.isolate
                                             (Prelude.fromIntegral len) Data.ProtoLens.parseMessage)
                                       "source_info"
                                loop
                                  (Lens.Family2.set (Data.ProtoLens.Field.field @"sourceInfo") y x)
                        wire
                          -> do !y <- Data.ProtoLens.Encoding.Wire.parseTaggedValueFromWire
                                        wire
                                loop
                                  (Lens.Family2.over
                                     Data.ProtoLens.unknownFields (\ !t -> (:) y t) x)
      in
        (Data.ProtoLens.Encoding.Bytes.<?>)
          (do loop Data.ProtoLens.defMessage) "ModuleName"
  buildMessage
    = \ _x
        -> (Data.Monoid.<>)
             (let _v = Lens.Family2.view (Data.ProtoLens.Field.field @"name") _x
              in
                if (Prelude.==) _v Data.ProtoLens.fieldDefault then
                    Data.Monoid.mempty
                else
                    (Data.Monoid.<>)
                      (Data.ProtoLens.Encoding.Bytes.putVarInt 10)
                      ((Prelude..)
                         (\ bs
                            -> (Data.Monoid.<>)
                                 (Data.ProtoLens.Encoding.Bytes.putVarInt
                                    (Prelude.fromIntegral (Data.ByteString.length bs)))
                                 (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                         Data.Text.Encoding.encodeUtf8 _v))
             ((Data.Monoid.<>)
                (case
                     Lens.Family2.view
                       (Data.ProtoLens.Field.field @"maybe'sourceInfo") _x
                 of
                   Prelude.Nothing -> Data.Monoid.mempty
                   (Prelude.Just _v)
                     -> (Data.Monoid.<>)
                          (Data.ProtoLens.Encoding.Bytes.putVarInt 18)
                          ((Prelude..)
                             (\ bs
                                -> (Data.Monoid.<>)
                                     (Data.ProtoLens.Encoding.Bytes.putVarInt
                                        (Prelude.fromIntegral (Data.ByteString.length bs)))
                                     (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                             Data.ProtoLens.encodeMessage _v))
                (Data.ProtoLens.Encoding.Wire.buildFieldSet
                   (Lens.Family2.view Data.ProtoLens.unknownFields _x)))
instance Control.DeepSeq.NFData ModuleName where
  rnf
    = \ x__
        -> Control.DeepSeq.deepseq
             (_ModuleName'_unknownFields x__)
             (Control.DeepSeq.deepseq
                (_ModuleName'name x__)
                (Control.DeepSeq.deepseq (_ModuleName'sourceInfo x__) ()))
{- | Fields :
     
         * 'Proto.Repls.LambdabuffersSource_Fields.fields' @:: Lens' NTuple [Type]@
         * 'Proto.Repls.LambdabuffersSource_Fields.vec'fields' @:: Lens' NTuple (Data.Vector.Vector Type)@
         * 'Proto.Repls.LambdabuffersSource_Fields.sourceInfo' @:: Lens' NTuple SourceInfo@
         * 'Proto.Repls.LambdabuffersSource_Fields.maybe'sourceInfo' @:: Lens' NTuple (Prelude.Maybe SourceInfo)@ -}
data NTuple
  = NTuple'_constructor {_NTuple'fields :: !(Data.Vector.Vector Type),
                         _NTuple'sourceInfo :: !(Prelude.Maybe SourceInfo),
                         _NTuple'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving stock (Prelude.Eq, Prelude.Ord)
instance Prelude.Show NTuple where
  showsPrec _ __x __s
    = Prelude.showChar
        '{'
        (Prelude.showString
           (Data.ProtoLens.showMessageShort __x) (Prelude.showChar '}' __s))
instance Data.ProtoLens.Field.HasField NTuple "fields" [Type] where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _NTuple'fields (\ x__ y__ -> x__ {_NTuple'fields = y__}))
        (Lens.Family2.Unchecked.lens
           Data.Vector.Generic.toList
           (\ _ y__ -> Data.Vector.Generic.fromList y__))
instance Data.ProtoLens.Field.HasField NTuple "vec'fields" (Data.Vector.Vector Type) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _NTuple'fields (\ x__ y__ -> x__ {_NTuple'fields = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField NTuple "sourceInfo" SourceInfo where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _NTuple'sourceInfo (\ x__ y__ -> x__ {_NTuple'sourceInfo = y__}))
        (Data.ProtoLens.maybeLens Data.ProtoLens.defMessage)
instance Data.ProtoLens.Field.HasField NTuple "maybe'sourceInfo" (Prelude.Maybe SourceInfo) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _NTuple'sourceInfo (\ x__ y__ -> x__ {_NTuple'sourceInfo = y__}))
        Prelude.id
instance Data.ProtoLens.Message NTuple where
  messageName _ = Data.Text.pack "lambdabuffers.source.NTuple"
  packedMessageDescriptor _
    = "\n\
      \\ACKNTuple\DC22\n\
      \\ACKfields\CAN\SOH \ETX(\v2\SUB.lambdabuffers.source.TypeR\ACKfields\DC2A\n\
      \\vsource_info\CAN\STX \SOH(\v2 .lambdabuffers.source.SourceInfoR\n\
      \sourceInfo"
  packedFileDescriptor _ = packedFileDescriptor
  fieldsByTag
    = let
        fields__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "fields"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor Type)
              (Data.ProtoLens.RepeatedField
                 Data.ProtoLens.Unpacked (Data.ProtoLens.Field.field @"fields")) ::
              Data.ProtoLens.FieldDescriptor NTuple
        sourceInfo__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "source_info"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor SourceInfo)
              (Data.ProtoLens.OptionalField
                 (Data.ProtoLens.Field.field @"maybe'sourceInfo")) ::
              Data.ProtoLens.FieldDescriptor NTuple
      in
        Data.Map.fromList
          [(Data.ProtoLens.Tag 1, fields__field_descriptor),
           (Data.ProtoLens.Tag 2, sourceInfo__field_descriptor)]
  unknownFields
    = Lens.Family2.Unchecked.lens
        _NTuple'_unknownFields
        (\ x__ y__ -> x__ {_NTuple'_unknownFields = y__})
  defMessage
    = NTuple'_constructor
        {_NTuple'fields = Data.Vector.Generic.empty,
         _NTuple'sourceInfo = Prelude.Nothing, _NTuple'_unknownFields = []}
  parseMessage
    = let
        loop ::
          NTuple
          -> Data.ProtoLens.Encoding.Growing.Growing Data.Vector.Vector Data.ProtoLens.Encoding.Growing.RealWorld Type
             -> Data.ProtoLens.Encoding.Bytes.Parser NTuple
        loop x mutable'fields
          = do end <- Data.ProtoLens.Encoding.Bytes.atEnd
               if end then
                   do frozen'fields <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                         (Data.ProtoLens.Encoding.Growing.unsafeFreeze
                                            mutable'fields)
                      (let missing = []
                       in
                         if Prelude.null missing then
                             Prelude.return ()
                         else
                             Prelude.fail
                               ((Prelude.++)
                                  "Missing required fields: "
                                  (Prelude.show (missing :: [Prelude.String]))))
                      Prelude.return
                        (Lens.Family2.over
                           Data.ProtoLens.unknownFields (\ !t -> Prelude.reverse t)
                           (Lens.Family2.set
                              (Data.ProtoLens.Field.field @"vec'fields") frozen'fields x))
               else
                   do tag <- Data.ProtoLens.Encoding.Bytes.getVarInt
                      case tag of
                        10
                          -> do !y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                        (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                            Data.ProtoLens.Encoding.Bytes.isolate
                                              (Prelude.fromIntegral len)
                                              Data.ProtoLens.parseMessage)
                                        "fields"
                                v <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                       (Data.ProtoLens.Encoding.Growing.append mutable'fields y)
                                loop x v
                        18
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                           Data.ProtoLens.Encoding.Bytes.isolate
                                             (Prelude.fromIntegral len) Data.ProtoLens.parseMessage)
                                       "source_info"
                                loop
                                  (Lens.Family2.set (Data.ProtoLens.Field.field @"sourceInfo") y x)
                                  mutable'fields
                        wire
                          -> do !y <- Data.ProtoLens.Encoding.Wire.parseTaggedValueFromWire
                                        wire
                                loop
                                  (Lens.Family2.over
                                     Data.ProtoLens.unknownFields (\ !t -> (:) y t) x)
                                  mutable'fields
      in
        (Data.ProtoLens.Encoding.Bytes.<?>)
          (do mutable'fields <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                  Data.ProtoLens.Encoding.Growing.new
              loop Data.ProtoLens.defMessage mutable'fields)
          "NTuple"
  buildMessage
    = \ _x
        -> (Data.Monoid.<>)
             (Data.ProtoLens.Encoding.Bytes.foldMapBuilder
                (\ _v
                   -> (Data.Monoid.<>)
                        (Data.ProtoLens.Encoding.Bytes.putVarInt 10)
                        ((Prelude..)
                           (\ bs
                              -> (Data.Monoid.<>)
                                   (Data.ProtoLens.Encoding.Bytes.putVarInt
                                      (Prelude.fromIntegral (Data.ByteString.length bs)))
                                   (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                           Data.ProtoLens.encodeMessage _v))
                (Lens.Family2.view (Data.ProtoLens.Field.field @"vec'fields") _x))
             ((Data.Monoid.<>)
                (case
                     Lens.Family2.view
                       (Data.ProtoLens.Field.field @"maybe'sourceInfo") _x
                 of
                   Prelude.Nothing -> Data.Monoid.mempty
                   (Prelude.Just _v)
                     -> (Data.Monoid.<>)
                          (Data.ProtoLens.Encoding.Bytes.putVarInt 18)
                          ((Prelude..)
                             (\ bs
                                -> (Data.Monoid.<>)
                                     (Data.ProtoLens.Encoding.Bytes.putVarInt
                                        (Prelude.fromIntegral (Data.ByteString.length bs)))
                                     (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                             Data.ProtoLens.encodeMessage _v))
                (Data.ProtoLens.Encoding.Wire.buildFieldSet
                   (Lens.Family2.view Data.ProtoLens.unknownFields _x)))
instance Control.DeepSeq.NFData NTuple where
  rnf
    = \ x__
        -> Control.DeepSeq.deepseq
             (_NTuple'_unknownFields x__)
             (Control.DeepSeq.deepseq
                (_NTuple'fields x__)
                (Control.DeepSeq.deepseq (_NTuple'sourceInfo x__) ()))
{- | Fields :
     
         * 'Proto.Repls.LambdabuffersSource_Fields.sourceInfo' @:: Lens' Opaque SourceInfo@
         * 'Proto.Repls.LambdabuffersSource_Fields.maybe'sourceInfo' @:: Lens' Opaque (Prelude.Maybe SourceInfo)@ -}
data Opaque
  = Opaque'_constructor {_Opaque'sourceInfo :: !(Prelude.Maybe SourceInfo),
                         _Opaque'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving stock (Prelude.Eq, Prelude.Ord)
instance Prelude.Show Opaque where
  showsPrec _ __x __s
    = Prelude.showChar
        '{'
        (Prelude.showString
           (Data.ProtoLens.showMessageShort __x) (Prelude.showChar '}' __s))
instance Data.ProtoLens.Field.HasField Opaque "sourceInfo" SourceInfo where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _Opaque'sourceInfo (\ x__ y__ -> x__ {_Opaque'sourceInfo = y__}))
        (Data.ProtoLens.maybeLens Data.ProtoLens.defMessage)
instance Data.ProtoLens.Field.HasField Opaque "maybe'sourceInfo" (Prelude.Maybe SourceInfo) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _Opaque'sourceInfo (\ x__ y__ -> x__ {_Opaque'sourceInfo = y__}))
        Prelude.id
instance Data.ProtoLens.Message Opaque where
  messageName _ = Data.Text.pack "lambdabuffers.source.Opaque"
  packedMessageDescriptor _
    = "\n\
      \\ACKOpaque\DC2A\n\
      \\vsource_info\CAN\SOH \SOH(\v2 .lambdabuffers.source.SourceInfoR\n\
      \sourceInfo"
  packedFileDescriptor _ = packedFileDescriptor
  fieldsByTag
    = let
        sourceInfo__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "source_info"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor SourceInfo)
              (Data.ProtoLens.OptionalField
                 (Data.ProtoLens.Field.field @"maybe'sourceInfo")) ::
              Data.ProtoLens.FieldDescriptor Opaque
      in
        Data.Map.fromList
          [(Data.ProtoLens.Tag 1, sourceInfo__field_descriptor)]
  unknownFields
    = Lens.Family2.Unchecked.lens
        _Opaque'_unknownFields
        (\ x__ y__ -> x__ {_Opaque'_unknownFields = y__})
  defMessage
    = Opaque'_constructor
        {_Opaque'sourceInfo = Prelude.Nothing, _Opaque'_unknownFields = []}
  parseMessage
    = let
        loop :: Opaque -> Data.ProtoLens.Encoding.Bytes.Parser Opaque
        loop x
          = do end <- Data.ProtoLens.Encoding.Bytes.atEnd
               if end then
                   do (let missing = []
                       in
                         if Prelude.null missing then
                             Prelude.return ()
                         else
                             Prelude.fail
                               ((Prelude.++)
                                  "Missing required fields: "
                                  (Prelude.show (missing :: [Prelude.String]))))
                      Prelude.return
                        (Lens.Family2.over
                           Data.ProtoLens.unknownFields (\ !t -> Prelude.reverse t) x)
               else
                   do tag <- Data.ProtoLens.Encoding.Bytes.getVarInt
                      case tag of
                        10
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                           Data.ProtoLens.Encoding.Bytes.isolate
                                             (Prelude.fromIntegral len) Data.ProtoLens.parseMessage)
                                       "source_info"
                                loop
                                  (Lens.Family2.set (Data.ProtoLens.Field.field @"sourceInfo") y x)
                        wire
                          -> do !y <- Data.ProtoLens.Encoding.Wire.parseTaggedValueFromWire
                                        wire
                                loop
                                  (Lens.Family2.over
                                     Data.ProtoLens.unknownFields (\ !t -> (:) y t) x)
      in
        (Data.ProtoLens.Encoding.Bytes.<?>)
          (do loop Data.ProtoLens.defMessage) "Opaque"
  buildMessage
    = \ _x
        -> (Data.Monoid.<>)
             (case
                  Lens.Family2.view
                    (Data.ProtoLens.Field.field @"maybe'sourceInfo") _x
              of
                Prelude.Nothing -> Data.Monoid.mempty
                (Prelude.Just _v)
                  -> (Data.Monoid.<>)
                       (Data.ProtoLens.Encoding.Bytes.putVarInt 10)
                       ((Prelude..)
                          (\ bs
                             -> (Data.Monoid.<>)
                                  (Data.ProtoLens.Encoding.Bytes.putVarInt
                                     (Prelude.fromIntegral (Data.ByteString.length bs)))
                                  (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                          Data.ProtoLens.encodeMessage _v))
             (Data.ProtoLens.Encoding.Wire.buildFieldSet
                (Lens.Family2.view Data.ProtoLens.unknownFields _x))
instance Control.DeepSeq.NFData Opaque where
  rnf
    = \ x__
        -> Control.DeepSeq.deepseq
             (_Opaque'_unknownFields x__)
             (Control.DeepSeq.deepseq (_Opaque'sourceInfo x__) ())
{- | Fields :
     
         * 'Proto.Repls.LambdabuffersSource_Fields.sourceInfo' @:: Lens' Product SourceInfo@
         * 'Proto.Repls.LambdabuffersSource_Fields.maybe'sourceInfo' @:: Lens' Product (Prelude.Maybe SourceInfo)@
         * 'Proto.Repls.LambdabuffersSource_Fields.maybe'product' @:: Lens' Product (Prelude.Maybe Product'Product)@
         * 'Proto.Repls.LambdabuffersSource_Fields.maybe'empty' @:: Lens' Product (Prelude.Maybe Empty)@
         * 'Proto.Repls.LambdabuffersSource_Fields.empty' @:: Lens' Product Empty@
         * 'Proto.Repls.LambdabuffersSource_Fields.maybe'record' @:: Lens' Product (Prelude.Maybe Record)@
         * 'Proto.Repls.LambdabuffersSource_Fields.record' @:: Lens' Product Record@
         * 'Proto.Repls.LambdabuffersSource_Fields.maybe'ntuple' @:: Lens' Product (Prelude.Maybe NTuple)@
         * 'Proto.Repls.LambdabuffersSource_Fields.ntuple' @:: Lens' Product NTuple@ -}
data Product
  = Product'_constructor {_Product'sourceInfo :: !(Prelude.Maybe SourceInfo),
                          _Product'product :: !(Prelude.Maybe Product'Product),
                          _Product'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving stock (Prelude.Eq, Prelude.Ord)
instance Prelude.Show Product where
  showsPrec _ __x __s
    = Prelude.showChar
        '{'
        (Prelude.showString
           (Data.ProtoLens.showMessageShort __x) (Prelude.showChar '}' __s))
data Product'Product
  = Product'Empty !Empty |
    Product'Record !Record |
    Product'Ntuple !NTuple
  deriving stock (Prelude.Show, Prelude.Eq, Prelude.Ord)
instance Data.ProtoLens.Field.HasField Product "sourceInfo" SourceInfo where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _Product'sourceInfo (\ x__ y__ -> x__ {_Product'sourceInfo = y__}))
        (Data.ProtoLens.maybeLens Data.ProtoLens.defMessage)
instance Data.ProtoLens.Field.HasField Product "maybe'sourceInfo" (Prelude.Maybe SourceInfo) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _Product'sourceInfo (\ x__ y__ -> x__ {_Product'sourceInfo = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField Product "maybe'product" (Prelude.Maybe Product'Product) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _Product'product (\ x__ y__ -> x__ {_Product'product = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField Product "maybe'empty" (Prelude.Maybe Empty) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _Product'product (\ x__ y__ -> x__ {_Product'product = y__}))
        (Lens.Family2.Unchecked.lens
           (\ x__
              -> case x__ of
                   (Prelude.Just (Product'Empty x__val)) -> Prelude.Just x__val
                   _otherwise -> Prelude.Nothing)
           (\ _ y__ -> Prelude.fmap Product'Empty y__))
instance Data.ProtoLens.Field.HasField Product "empty" Empty where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _Product'product (\ x__ y__ -> x__ {_Product'product = y__}))
        ((Prelude..)
           (Lens.Family2.Unchecked.lens
              (\ x__
                 -> case x__ of
                      (Prelude.Just (Product'Empty x__val)) -> Prelude.Just x__val
                      _otherwise -> Prelude.Nothing)
              (\ _ y__ -> Prelude.fmap Product'Empty y__))
           (Data.ProtoLens.maybeLens Data.ProtoLens.defMessage))
instance Data.ProtoLens.Field.HasField Product "maybe'record" (Prelude.Maybe Record) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _Product'product (\ x__ y__ -> x__ {_Product'product = y__}))
        (Lens.Family2.Unchecked.lens
           (\ x__
              -> case x__ of
                   (Prelude.Just (Product'Record x__val)) -> Prelude.Just x__val
                   _otherwise -> Prelude.Nothing)
           (\ _ y__ -> Prelude.fmap Product'Record y__))
instance Data.ProtoLens.Field.HasField Product "record" Record where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _Product'product (\ x__ y__ -> x__ {_Product'product = y__}))
        ((Prelude..)
           (Lens.Family2.Unchecked.lens
              (\ x__
                 -> case x__ of
                      (Prelude.Just (Product'Record x__val)) -> Prelude.Just x__val
                      _otherwise -> Prelude.Nothing)
              (\ _ y__ -> Prelude.fmap Product'Record y__))
           (Data.ProtoLens.maybeLens Data.ProtoLens.defMessage))
instance Data.ProtoLens.Field.HasField Product "maybe'ntuple" (Prelude.Maybe NTuple) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _Product'product (\ x__ y__ -> x__ {_Product'product = y__}))
        (Lens.Family2.Unchecked.lens
           (\ x__
              -> case x__ of
                   (Prelude.Just (Product'Ntuple x__val)) -> Prelude.Just x__val
                   _otherwise -> Prelude.Nothing)
           (\ _ y__ -> Prelude.fmap Product'Ntuple y__))
instance Data.ProtoLens.Field.HasField Product "ntuple" NTuple where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _Product'product (\ x__ y__ -> x__ {_Product'product = y__}))
        ((Prelude..)
           (Lens.Family2.Unchecked.lens
              (\ x__
                 -> case x__ of
                      (Prelude.Just (Product'Ntuple x__val)) -> Prelude.Just x__val
                      _otherwise -> Prelude.Nothing)
              (\ _ y__ -> Prelude.fmap Product'Ntuple y__))
           (Data.ProtoLens.maybeLens Data.ProtoLens.defMessage))
instance Data.ProtoLens.Message Product where
  messageName _ = Data.Text.pack "lambdabuffers.source.Product"
  packedMessageDescriptor _
    = "\n\
      \\aProduct\DC23\n\
      \\ENQempty\CAN\SOH \SOH(\v2\ESC.lambdabuffers.source.EmptyH\NULR\ENQempty\DC26\n\
      \\ACKrecord\CAN\STX \SOH(\v2\FS.lambdabuffers.source.RecordH\NULR\ACKrecord\DC26\n\
      \\ACKntuple\CAN\ETX \SOH(\v2\FS.lambdabuffers.source.NTupleH\NULR\ACKntuple\DC2A\n\
      \\vsource_info\CAN\EOT \SOH(\v2 .lambdabuffers.source.SourceInfoR\n\
      \sourceInfoB\t\n\
      \\aproduct"
  packedFileDescriptor _ = packedFileDescriptor
  fieldsByTag
    = let
        sourceInfo__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "source_info"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor SourceInfo)
              (Data.ProtoLens.OptionalField
                 (Data.ProtoLens.Field.field @"maybe'sourceInfo")) ::
              Data.ProtoLens.FieldDescriptor Product
        empty__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "empty"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor Empty)
              (Data.ProtoLens.OptionalField
                 (Data.ProtoLens.Field.field @"maybe'empty")) ::
              Data.ProtoLens.FieldDescriptor Product
        record__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "record"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor Record)
              (Data.ProtoLens.OptionalField
                 (Data.ProtoLens.Field.field @"maybe'record")) ::
              Data.ProtoLens.FieldDescriptor Product
        ntuple__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "ntuple"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor NTuple)
              (Data.ProtoLens.OptionalField
                 (Data.ProtoLens.Field.field @"maybe'ntuple")) ::
              Data.ProtoLens.FieldDescriptor Product
      in
        Data.Map.fromList
          [(Data.ProtoLens.Tag 4, sourceInfo__field_descriptor),
           (Data.ProtoLens.Tag 1, empty__field_descriptor),
           (Data.ProtoLens.Tag 2, record__field_descriptor),
           (Data.ProtoLens.Tag 3, ntuple__field_descriptor)]
  unknownFields
    = Lens.Family2.Unchecked.lens
        _Product'_unknownFields
        (\ x__ y__ -> x__ {_Product'_unknownFields = y__})
  defMessage
    = Product'_constructor
        {_Product'sourceInfo = Prelude.Nothing,
         _Product'product = Prelude.Nothing, _Product'_unknownFields = []}
  parseMessage
    = let
        loop :: Product -> Data.ProtoLens.Encoding.Bytes.Parser Product
        loop x
          = do end <- Data.ProtoLens.Encoding.Bytes.atEnd
               if end then
                   do (let missing = []
                       in
                         if Prelude.null missing then
                             Prelude.return ()
                         else
                             Prelude.fail
                               ((Prelude.++)
                                  "Missing required fields: "
                                  (Prelude.show (missing :: [Prelude.String]))))
                      Prelude.return
                        (Lens.Family2.over
                           Data.ProtoLens.unknownFields (\ !t -> Prelude.reverse t) x)
               else
                   do tag <- Data.ProtoLens.Encoding.Bytes.getVarInt
                      case tag of
                        34
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                           Data.ProtoLens.Encoding.Bytes.isolate
                                             (Prelude.fromIntegral len) Data.ProtoLens.parseMessage)
                                       "source_info"
                                loop
                                  (Lens.Family2.set (Data.ProtoLens.Field.field @"sourceInfo") y x)
                        10
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                           Data.ProtoLens.Encoding.Bytes.isolate
                                             (Prelude.fromIntegral len) Data.ProtoLens.parseMessage)
                                       "empty"
                                loop (Lens.Family2.set (Data.ProtoLens.Field.field @"empty") y x)
                        18
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                           Data.ProtoLens.Encoding.Bytes.isolate
                                             (Prelude.fromIntegral len) Data.ProtoLens.parseMessage)
                                       "record"
                                loop (Lens.Family2.set (Data.ProtoLens.Field.field @"record") y x)
                        26
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                           Data.ProtoLens.Encoding.Bytes.isolate
                                             (Prelude.fromIntegral len) Data.ProtoLens.parseMessage)
                                       "ntuple"
                                loop (Lens.Family2.set (Data.ProtoLens.Field.field @"ntuple") y x)
                        wire
                          -> do !y <- Data.ProtoLens.Encoding.Wire.parseTaggedValueFromWire
                                        wire
                                loop
                                  (Lens.Family2.over
                                     Data.ProtoLens.unknownFields (\ !t -> (:) y t) x)
      in
        (Data.ProtoLens.Encoding.Bytes.<?>)
          (do loop Data.ProtoLens.defMessage) "Product"
  buildMessage
    = \ _x
        -> (Data.Monoid.<>)
             (case
                  Lens.Family2.view
                    (Data.ProtoLens.Field.field @"maybe'sourceInfo") _x
              of
                Prelude.Nothing -> Data.Monoid.mempty
                (Prelude.Just _v)
                  -> (Data.Monoid.<>)
                       (Data.ProtoLens.Encoding.Bytes.putVarInt 34)
                       ((Prelude..)
                          (\ bs
                             -> (Data.Monoid.<>)
                                  (Data.ProtoLens.Encoding.Bytes.putVarInt
                                     (Prelude.fromIntegral (Data.ByteString.length bs)))
                                  (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                          Data.ProtoLens.encodeMessage _v))
             ((Data.Monoid.<>)
                (case
                     Lens.Family2.view (Data.ProtoLens.Field.field @"maybe'product") _x
                 of
                   Prelude.Nothing -> Data.Monoid.mempty
                   (Prelude.Just (Product'Empty v))
                     -> (Data.Monoid.<>)
                          (Data.ProtoLens.Encoding.Bytes.putVarInt 10)
                          ((Prelude..)
                             (\ bs
                                -> (Data.Monoid.<>)
                                     (Data.ProtoLens.Encoding.Bytes.putVarInt
                                        (Prelude.fromIntegral (Data.ByteString.length bs)))
                                     (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                             Data.ProtoLens.encodeMessage v)
                   (Prelude.Just (Product'Record v))
                     -> (Data.Monoid.<>)
                          (Data.ProtoLens.Encoding.Bytes.putVarInt 18)
                          ((Prelude..)
                             (\ bs
                                -> (Data.Monoid.<>)
                                     (Data.ProtoLens.Encoding.Bytes.putVarInt
                                        (Prelude.fromIntegral (Data.ByteString.length bs)))
                                     (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                             Data.ProtoLens.encodeMessage v)
                   (Prelude.Just (Product'Ntuple v))
                     -> (Data.Monoid.<>)
                          (Data.ProtoLens.Encoding.Bytes.putVarInt 26)
                          ((Prelude..)
                             (\ bs
                                -> (Data.Monoid.<>)
                                     (Data.ProtoLens.Encoding.Bytes.putVarInt
                                        (Prelude.fromIntegral (Data.ByteString.length bs)))
                                     (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                             Data.ProtoLens.encodeMessage v))
                (Data.ProtoLens.Encoding.Wire.buildFieldSet
                   (Lens.Family2.view Data.ProtoLens.unknownFields _x)))
instance Control.DeepSeq.NFData Product where
  rnf
    = \ x__
        -> Control.DeepSeq.deepseq
             (_Product'_unknownFields x__)
             (Control.DeepSeq.deepseq
                (_Product'sourceInfo x__)
                (Control.DeepSeq.deepseq (_Product'product x__) ()))
instance Control.DeepSeq.NFData Product'Product where
  rnf (Product'Empty x__) = Control.DeepSeq.rnf x__
  rnf (Product'Record x__) = Control.DeepSeq.rnf x__
  rnf (Product'Ntuple x__) = Control.DeepSeq.rnf x__
_Product'Empty :: Data.ProtoLens.Prism.Prism' Product'Product Empty
_Product'Empty
  = Data.ProtoLens.Prism.prism'
      Product'Empty
      (\ p__
         -> case p__ of
              (Product'Empty p__val) -> Prelude.Just p__val
              _otherwise -> Prelude.Nothing)
_Product'Record ::
  Data.ProtoLens.Prism.Prism' Product'Product Record
_Product'Record
  = Data.ProtoLens.Prism.prism'
      Product'Record
      (\ p__
         -> case p__ of
              (Product'Record p__val) -> Prelude.Just p__val
              _otherwise -> Prelude.Nothing)
_Product'Ntuple ::
  Data.ProtoLens.Prism.Prism' Product'Product NTuple
_Product'Ntuple
  = Data.ProtoLens.Prism.prism'
      Product'Ntuple
      (\ p__
         -> case p__ of
              (Product'Ntuple p__val) -> Prelude.Just p__val
              _otherwise -> Prelude.Nothing)
{- | Fields :
     
         * 'Proto.Repls.LambdabuffersSource_Fields.fields' @:: Lens' Record (Data.Map.Map Data.Text.Text Type)@
         * 'Proto.Repls.LambdabuffersSource_Fields.sourceInfog' @:: Lens' Record SourceInfo@
         * 'Proto.Repls.LambdabuffersSource_Fields.maybe'sourceInfog' @:: Lens' Record (Prelude.Maybe SourceInfo)@ -}
data Record
  = Record'_constructor {_Record'fields :: !(Data.Map.Map Data.Text.Text Type),
                         _Record'sourceInfog :: !(Prelude.Maybe SourceInfo),
                         _Record'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving stock (Prelude.Eq, Prelude.Ord)
instance Prelude.Show Record where
  showsPrec _ __x __s
    = Prelude.showChar
        '{'
        (Prelude.showString
           (Data.ProtoLens.showMessageShort __x) (Prelude.showChar '}' __s))
instance Data.ProtoLens.Field.HasField Record "fields" (Data.Map.Map Data.Text.Text Type) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _Record'fields (\ x__ y__ -> x__ {_Record'fields = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField Record "sourceInfog" SourceInfo where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _Record'sourceInfog (\ x__ y__ -> x__ {_Record'sourceInfog = y__}))
        (Data.ProtoLens.maybeLens Data.ProtoLens.defMessage)
instance Data.ProtoLens.Field.HasField Record "maybe'sourceInfog" (Prelude.Maybe SourceInfo) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _Record'sourceInfog (\ x__ y__ -> x__ {_Record'sourceInfog = y__}))
        Prelude.id
instance Data.ProtoLens.Message Record where
  messageName _ = Data.Text.pack "lambdabuffers.source.Record"
  packedMessageDescriptor _
    = "\n\
      \\ACKRecord\DC2@\n\
      \\ACKfields\CAN\SOH \ETX(\v2(.lambdabuffers.source.Record.FieldsEntryR\ACKfields\DC2C\n\
      \\fsource_infog\CAN\STX \SOH(\v2 .lambdabuffers.source.SourceInfoR\vsourceInfog\SUBU\n\
      \\vFieldsEntry\DC2\DLE\n\
      \\ETXkey\CAN\SOH \SOH(\tR\ETXkey\DC20\n\
      \\ENQvalue\CAN\STX \SOH(\v2\SUB.lambdabuffers.source.TypeR\ENQvalue:\STX8\SOH"
  packedFileDescriptor _ = packedFileDescriptor
  fieldsByTag
    = let
        fields__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "fields"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor Record'FieldsEntry)
              (Data.ProtoLens.MapField
                 (Data.ProtoLens.Field.field @"key")
                 (Data.ProtoLens.Field.field @"value")
                 (Data.ProtoLens.Field.field @"fields")) ::
              Data.ProtoLens.FieldDescriptor Record
        sourceInfog__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "source_infog"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor SourceInfo)
              (Data.ProtoLens.OptionalField
                 (Data.ProtoLens.Field.field @"maybe'sourceInfog")) ::
              Data.ProtoLens.FieldDescriptor Record
      in
        Data.Map.fromList
          [(Data.ProtoLens.Tag 1, fields__field_descriptor),
           (Data.ProtoLens.Tag 2, sourceInfog__field_descriptor)]
  unknownFields
    = Lens.Family2.Unchecked.lens
        _Record'_unknownFields
        (\ x__ y__ -> x__ {_Record'_unknownFields = y__})
  defMessage
    = Record'_constructor
        {_Record'fields = Data.Map.empty,
         _Record'sourceInfog = Prelude.Nothing, _Record'_unknownFields = []}
  parseMessage
    = let
        loop :: Record -> Data.ProtoLens.Encoding.Bytes.Parser Record
        loop x
          = do end <- Data.ProtoLens.Encoding.Bytes.atEnd
               if end then
                   do (let missing = []
                       in
                         if Prelude.null missing then
                             Prelude.return ()
                         else
                             Prelude.fail
                               ((Prelude.++)
                                  "Missing required fields: "
                                  (Prelude.show (missing :: [Prelude.String]))))
                      Prelude.return
                        (Lens.Family2.over
                           Data.ProtoLens.unknownFields (\ !t -> Prelude.reverse t) x)
               else
                   do tag <- Data.ProtoLens.Encoding.Bytes.getVarInt
                      case tag of
                        10
                          -> do !(entry :: Record'FieldsEntry) <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                                                    (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                                                        Data.ProtoLens.Encoding.Bytes.isolate
                                                                          (Prelude.fromIntegral len)
                                                                          Data.ProtoLens.parseMessage)
                                                                    "fields"
                                (let
                                   key = Lens.Family2.view (Data.ProtoLens.Field.field @"key") entry
                                   value
                                     = Lens.Family2.view (Data.ProtoLens.Field.field @"value") entry
                                 in
                                   loop
                                     (Lens.Family2.over
                                        (Data.ProtoLens.Field.field @"fields")
                                        (\ !t -> Data.Map.insert key value t) x))
                        18
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                           Data.ProtoLens.Encoding.Bytes.isolate
                                             (Prelude.fromIntegral len) Data.ProtoLens.parseMessage)
                                       "source_infog"
                                loop
                                  (Lens.Family2.set (Data.ProtoLens.Field.field @"sourceInfog") y x)
                        wire
                          -> do !y <- Data.ProtoLens.Encoding.Wire.parseTaggedValueFromWire
                                        wire
                                loop
                                  (Lens.Family2.over
                                     Data.ProtoLens.unknownFields (\ !t -> (:) y t) x)
      in
        (Data.ProtoLens.Encoding.Bytes.<?>)
          (do loop Data.ProtoLens.defMessage) "Record"
  buildMessage
    = \ _x
        -> (Data.Monoid.<>)
             (Data.Monoid.mconcat
                (Prelude.map
                   (\ _v
                      -> (Data.Monoid.<>)
                           (Data.ProtoLens.Encoding.Bytes.putVarInt 10)
                           ((Prelude..)
                              (\ bs
                                 -> (Data.Monoid.<>)
                                      (Data.ProtoLens.Encoding.Bytes.putVarInt
                                         (Prelude.fromIntegral (Data.ByteString.length bs)))
                                      (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                              Data.ProtoLens.encodeMessage
                              (Lens.Family2.set
                                 (Data.ProtoLens.Field.field @"key") (Prelude.fst _v)
                                 (Lens.Family2.set
                                    (Data.ProtoLens.Field.field @"value") (Prelude.snd _v)
                                    (Data.ProtoLens.defMessage :: Record'FieldsEntry)))))
                   (Data.Map.toList
                      (Lens.Family2.view (Data.ProtoLens.Field.field @"fields") _x))))
             ((Data.Monoid.<>)
                (case
                     Lens.Family2.view
                       (Data.ProtoLens.Field.field @"maybe'sourceInfog") _x
                 of
                   Prelude.Nothing -> Data.Monoid.mempty
                   (Prelude.Just _v)
                     -> (Data.Monoid.<>)
                          (Data.ProtoLens.Encoding.Bytes.putVarInt 18)
                          ((Prelude..)
                             (\ bs
                                -> (Data.Monoid.<>)
                                     (Data.ProtoLens.Encoding.Bytes.putVarInt
                                        (Prelude.fromIntegral (Data.ByteString.length bs)))
                                     (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                             Data.ProtoLens.encodeMessage _v))
                (Data.ProtoLens.Encoding.Wire.buildFieldSet
                   (Lens.Family2.view Data.ProtoLens.unknownFields _x)))
instance Control.DeepSeq.NFData Record where
  rnf
    = \ x__
        -> Control.DeepSeq.deepseq
             (_Record'_unknownFields x__)
             (Control.DeepSeq.deepseq
                (_Record'fields x__)
                (Control.DeepSeq.deepseq (_Record'sourceInfog x__) ()))
{- | Fields :
     
         * 'Proto.Repls.LambdabuffersSource_Fields.key' @:: Lens' Record'FieldsEntry Data.Text.Text@
         * 'Proto.Repls.LambdabuffersSource_Fields.value' @:: Lens' Record'FieldsEntry Type@
         * 'Proto.Repls.LambdabuffersSource_Fields.maybe'value' @:: Lens' Record'FieldsEntry (Prelude.Maybe Type)@ -}
data Record'FieldsEntry
  = Record'FieldsEntry'_constructor {_Record'FieldsEntry'key :: !Data.Text.Text,
                                     _Record'FieldsEntry'value :: !(Prelude.Maybe Type),
                                     _Record'FieldsEntry'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving stock (Prelude.Eq, Prelude.Ord)
instance Prelude.Show Record'FieldsEntry where
  showsPrec _ __x __s
    = Prelude.showChar
        '{'
        (Prelude.showString
           (Data.ProtoLens.showMessageShort __x) (Prelude.showChar '}' __s))
instance Data.ProtoLens.Field.HasField Record'FieldsEntry "key" Data.Text.Text where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _Record'FieldsEntry'key
           (\ x__ y__ -> x__ {_Record'FieldsEntry'key = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField Record'FieldsEntry "value" Type where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _Record'FieldsEntry'value
           (\ x__ y__ -> x__ {_Record'FieldsEntry'value = y__}))
        (Data.ProtoLens.maybeLens Data.ProtoLens.defMessage)
instance Data.ProtoLens.Field.HasField Record'FieldsEntry "maybe'value" (Prelude.Maybe Type) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _Record'FieldsEntry'value
           (\ x__ y__ -> x__ {_Record'FieldsEntry'value = y__}))
        Prelude.id
instance Data.ProtoLens.Message Record'FieldsEntry where
  messageName _
    = Data.Text.pack "lambdabuffers.source.Record.FieldsEntry"
  packedMessageDescriptor _
    = "\n\
      \\vFieldsEntry\DC2\DLE\n\
      \\ETXkey\CAN\SOH \SOH(\tR\ETXkey\DC20\n\
      \\ENQvalue\CAN\STX \SOH(\v2\SUB.lambdabuffers.source.TypeR\ENQvalue:\STX8\SOH"
  packedFileDescriptor _ = packedFileDescriptor
  fieldsByTag
    = let
        key__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "key"
              (Data.ProtoLens.ScalarField Data.ProtoLens.StringField ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Text.Text)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional (Data.ProtoLens.Field.field @"key")) ::
              Data.ProtoLens.FieldDescriptor Record'FieldsEntry
        value__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "value"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor Type)
              (Data.ProtoLens.OptionalField
                 (Data.ProtoLens.Field.field @"maybe'value")) ::
              Data.ProtoLens.FieldDescriptor Record'FieldsEntry
      in
        Data.Map.fromList
          [(Data.ProtoLens.Tag 1, key__field_descriptor),
           (Data.ProtoLens.Tag 2, value__field_descriptor)]
  unknownFields
    = Lens.Family2.Unchecked.lens
        _Record'FieldsEntry'_unknownFields
        (\ x__ y__ -> x__ {_Record'FieldsEntry'_unknownFields = y__})
  defMessage
    = Record'FieldsEntry'_constructor
        {_Record'FieldsEntry'key = Data.ProtoLens.fieldDefault,
         _Record'FieldsEntry'value = Prelude.Nothing,
         _Record'FieldsEntry'_unknownFields = []}
  parseMessage
    = let
        loop ::
          Record'FieldsEntry
          -> Data.ProtoLens.Encoding.Bytes.Parser Record'FieldsEntry
        loop x
          = do end <- Data.ProtoLens.Encoding.Bytes.atEnd
               if end then
                   do (let missing = []
                       in
                         if Prelude.null missing then
                             Prelude.return ()
                         else
                             Prelude.fail
                               ((Prelude.++)
                                  "Missing required fields: "
                                  (Prelude.show (missing :: [Prelude.String]))))
                      Prelude.return
                        (Lens.Family2.over
                           Data.ProtoLens.unknownFields (\ !t -> Prelude.reverse t) x)
               else
                   do tag <- Data.ProtoLens.Encoding.Bytes.getVarInt
                      case tag of
                        10
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do value <- do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                                       Data.ProtoLens.Encoding.Bytes.getBytes
                                                         (Prelude.fromIntegral len)
                                           Data.ProtoLens.Encoding.Bytes.runEither
                                             (case Data.Text.Encoding.decodeUtf8' value of
                                                (Prelude.Left err)
                                                  -> Prelude.Left (Prelude.show err)
                                                (Prelude.Right r) -> Prelude.Right r))
                                       "key"
                                loop (Lens.Family2.set (Data.ProtoLens.Field.field @"key") y x)
                        18
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                           Data.ProtoLens.Encoding.Bytes.isolate
                                             (Prelude.fromIntegral len) Data.ProtoLens.parseMessage)
                                       "value"
                                loop (Lens.Family2.set (Data.ProtoLens.Field.field @"value") y x)
                        wire
                          -> do !y <- Data.ProtoLens.Encoding.Wire.parseTaggedValueFromWire
                                        wire
                                loop
                                  (Lens.Family2.over
                                     Data.ProtoLens.unknownFields (\ !t -> (:) y t) x)
      in
        (Data.ProtoLens.Encoding.Bytes.<?>)
          (do loop Data.ProtoLens.defMessage) "FieldsEntry"
  buildMessage
    = \ _x
        -> (Data.Monoid.<>)
             (let _v = Lens.Family2.view (Data.ProtoLens.Field.field @"key") _x
              in
                if (Prelude.==) _v Data.ProtoLens.fieldDefault then
                    Data.Monoid.mempty
                else
                    (Data.Monoid.<>)
                      (Data.ProtoLens.Encoding.Bytes.putVarInt 10)
                      ((Prelude..)
                         (\ bs
                            -> (Data.Monoid.<>)
                                 (Data.ProtoLens.Encoding.Bytes.putVarInt
                                    (Prelude.fromIntegral (Data.ByteString.length bs)))
                                 (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                         Data.Text.Encoding.encodeUtf8 _v))
             ((Data.Monoid.<>)
                (case
                     Lens.Family2.view (Data.ProtoLens.Field.field @"maybe'value") _x
                 of
                   Prelude.Nothing -> Data.Monoid.mempty
                   (Prelude.Just _v)
                     -> (Data.Monoid.<>)
                          (Data.ProtoLens.Encoding.Bytes.putVarInt 18)
                          ((Prelude..)
                             (\ bs
                                -> (Data.Monoid.<>)
                                     (Data.ProtoLens.Encoding.Bytes.putVarInt
                                        (Prelude.fromIntegral (Data.ByteString.length bs)))
                                     (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                             Data.ProtoLens.encodeMessage _v))
                (Data.ProtoLens.Encoding.Wire.buildFieldSet
                   (Lens.Family2.view Data.ProtoLens.unknownFields _x)))
instance Control.DeepSeq.NFData Record'FieldsEntry where
  rnf
    = \ x__
        -> Control.DeepSeq.deepseq
             (_Record'FieldsEntry'_unknownFields x__)
             (Control.DeepSeq.deepseq
                (_Record'FieldsEntry'key x__)
                (Control.DeepSeq.deepseq (_Record'FieldsEntry'value x__) ()))
{- | Fields :
     
         * 'Proto.Repls.LambdabuffersSource_Fields.file' @:: Lens' SourceInfo Data.Text.Text@
         * 'Proto.Repls.LambdabuffersSource_Fields.posFrom' @:: Lens' SourceInfo SourcePosition@
         * 'Proto.Repls.LambdabuffersSource_Fields.maybe'posFrom' @:: Lens' SourceInfo (Prelude.Maybe SourcePosition)@
         * 'Proto.Repls.LambdabuffersSource_Fields.posTo' @:: Lens' SourceInfo SourcePosition@
         * 'Proto.Repls.LambdabuffersSource_Fields.maybe'posTo' @:: Lens' SourceInfo (Prelude.Maybe SourcePosition)@ -}
data SourceInfo
  = SourceInfo'_constructor {_SourceInfo'file :: !Data.Text.Text,
                             _SourceInfo'posFrom :: !(Prelude.Maybe SourcePosition),
                             _SourceInfo'posTo :: !(Prelude.Maybe SourcePosition),
                             _SourceInfo'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving stock (Prelude.Eq, Prelude.Ord)
instance Prelude.Show SourceInfo where
  showsPrec _ __x __s
    = Prelude.showChar
        '{'
        (Prelude.showString
           (Data.ProtoLens.showMessageShort __x) (Prelude.showChar '}' __s))
instance Data.ProtoLens.Field.HasField SourceInfo "file" Data.Text.Text where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _SourceInfo'file (\ x__ y__ -> x__ {_SourceInfo'file = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField SourceInfo "posFrom" SourcePosition where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _SourceInfo'posFrom (\ x__ y__ -> x__ {_SourceInfo'posFrom = y__}))
        (Data.ProtoLens.maybeLens Data.ProtoLens.defMessage)
instance Data.ProtoLens.Field.HasField SourceInfo "maybe'posFrom" (Prelude.Maybe SourcePosition) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _SourceInfo'posFrom (\ x__ y__ -> x__ {_SourceInfo'posFrom = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField SourceInfo "posTo" SourcePosition where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _SourceInfo'posTo (\ x__ y__ -> x__ {_SourceInfo'posTo = y__}))
        (Data.ProtoLens.maybeLens Data.ProtoLens.defMessage)
instance Data.ProtoLens.Field.HasField SourceInfo "maybe'posTo" (Prelude.Maybe SourcePosition) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _SourceInfo'posTo (\ x__ y__ -> x__ {_SourceInfo'posTo = y__}))
        Prelude.id
instance Data.ProtoLens.Message SourceInfo where
  messageName _ = Data.Text.pack "lambdabuffers.source.SourceInfo"
  packedMessageDescriptor _
    = "\n\
      \\n\
      \SourceInfo\DC2\DC2\n\
      \\EOTfile\CAN\SOH \SOH(\tR\EOTfile\DC2?\n\
      \\bpos_from\CAN\STX \SOH(\v2$.lambdabuffers.source.SourcePositionR\aposFrom\DC2;\n\
      \\ACKpos_to\CAN\ETX \SOH(\v2$.lambdabuffers.source.SourcePositionR\ENQposTo"
  packedFileDescriptor _ = packedFileDescriptor
  fieldsByTag
    = let
        file__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "file"
              (Data.ProtoLens.ScalarField Data.ProtoLens.StringField ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Text.Text)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional (Data.ProtoLens.Field.field @"file")) ::
              Data.ProtoLens.FieldDescriptor SourceInfo
        posFrom__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "pos_from"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor SourcePosition)
              (Data.ProtoLens.OptionalField
                 (Data.ProtoLens.Field.field @"maybe'posFrom")) ::
              Data.ProtoLens.FieldDescriptor SourceInfo
        posTo__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "pos_to"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor SourcePosition)
              (Data.ProtoLens.OptionalField
                 (Data.ProtoLens.Field.field @"maybe'posTo")) ::
              Data.ProtoLens.FieldDescriptor SourceInfo
      in
        Data.Map.fromList
          [(Data.ProtoLens.Tag 1, file__field_descriptor),
           (Data.ProtoLens.Tag 2, posFrom__field_descriptor),
           (Data.ProtoLens.Tag 3, posTo__field_descriptor)]
  unknownFields
    = Lens.Family2.Unchecked.lens
        _SourceInfo'_unknownFields
        (\ x__ y__ -> x__ {_SourceInfo'_unknownFields = y__})
  defMessage
    = SourceInfo'_constructor
        {_SourceInfo'file = Data.ProtoLens.fieldDefault,
         _SourceInfo'posFrom = Prelude.Nothing,
         _SourceInfo'posTo = Prelude.Nothing,
         _SourceInfo'_unknownFields = []}
  parseMessage
    = let
        loop ::
          SourceInfo -> Data.ProtoLens.Encoding.Bytes.Parser SourceInfo
        loop x
          = do end <- Data.ProtoLens.Encoding.Bytes.atEnd
               if end then
                   do (let missing = []
                       in
                         if Prelude.null missing then
                             Prelude.return ()
                         else
                             Prelude.fail
                               ((Prelude.++)
                                  "Missing required fields: "
                                  (Prelude.show (missing :: [Prelude.String]))))
                      Prelude.return
                        (Lens.Family2.over
                           Data.ProtoLens.unknownFields (\ !t -> Prelude.reverse t) x)
               else
                   do tag <- Data.ProtoLens.Encoding.Bytes.getVarInt
                      case tag of
                        10
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do value <- do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                                       Data.ProtoLens.Encoding.Bytes.getBytes
                                                         (Prelude.fromIntegral len)
                                           Data.ProtoLens.Encoding.Bytes.runEither
                                             (case Data.Text.Encoding.decodeUtf8' value of
                                                (Prelude.Left err)
                                                  -> Prelude.Left (Prelude.show err)
                                                (Prelude.Right r) -> Prelude.Right r))
                                       "file"
                                loop (Lens.Family2.set (Data.ProtoLens.Field.field @"file") y x)
                        18
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                           Data.ProtoLens.Encoding.Bytes.isolate
                                             (Prelude.fromIntegral len) Data.ProtoLens.parseMessage)
                                       "pos_from"
                                loop (Lens.Family2.set (Data.ProtoLens.Field.field @"posFrom") y x)
                        26
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                           Data.ProtoLens.Encoding.Bytes.isolate
                                             (Prelude.fromIntegral len) Data.ProtoLens.parseMessage)
                                       "pos_to"
                                loop (Lens.Family2.set (Data.ProtoLens.Field.field @"posTo") y x)
                        wire
                          -> do !y <- Data.ProtoLens.Encoding.Wire.parseTaggedValueFromWire
                                        wire
                                loop
                                  (Lens.Family2.over
                                     Data.ProtoLens.unknownFields (\ !t -> (:) y t) x)
      in
        (Data.ProtoLens.Encoding.Bytes.<?>)
          (do loop Data.ProtoLens.defMessage) "SourceInfo"
  buildMessage
    = \ _x
        -> (Data.Monoid.<>)
             (let _v = Lens.Family2.view (Data.ProtoLens.Field.field @"file") _x
              in
                if (Prelude.==) _v Data.ProtoLens.fieldDefault then
                    Data.Monoid.mempty
                else
                    (Data.Monoid.<>)
                      (Data.ProtoLens.Encoding.Bytes.putVarInt 10)
                      ((Prelude..)
                         (\ bs
                            -> (Data.Monoid.<>)
                                 (Data.ProtoLens.Encoding.Bytes.putVarInt
                                    (Prelude.fromIntegral (Data.ByteString.length bs)))
                                 (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                         Data.Text.Encoding.encodeUtf8 _v))
             ((Data.Monoid.<>)
                (case
                     Lens.Family2.view (Data.ProtoLens.Field.field @"maybe'posFrom") _x
                 of
                   Prelude.Nothing -> Data.Monoid.mempty
                   (Prelude.Just _v)
                     -> (Data.Monoid.<>)
                          (Data.ProtoLens.Encoding.Bytes.putVarInt 18)
                          ((Prelude..)
                             (\ bs
                                -> (Data.Monoid.<>)
                                     (Data.ProtoLens.Encoding.Bytes.putVarInt
                                        (Prelude.fromIntegral (Data.ByteString.length bs)))
                                     (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                             Data.ProtoLens.encodeMessage _v))
                ((Data.Monoid.<>)
                   (case
                        Lens.Family2.view (Data.ProtoLens.Field.field @"maybe'posTo") _x
                    of
                      Prelude.Nothing -> Data.Monoid.mempty
                      (Prelude.Just _v)
                        -> (Data.Monoid.<>)
                             (Data.ProtoLens.Encoding.Bytes.putVarInt 26)
                             ((Prelude..)
                                (\ bs
                                   -> (Data.Monoid.<>)
                                        (Data.ProtoLens.Encoding.Bytes.putVarInt
                                           (Prelude.fromIntegral (Data.ByteString.length bs)))
                                        (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                                Data.ProtoLens.encodeMessage _v))
                   (Data.ProtoLens.Encoding.Wire.buildFieldSet
                      (Lens.Family2.view Data.ProtoLens.unknownFields _x))))
instance Control.DeepSeq.NFData SourceInfo where
  rnf
    = \ x__
        -> Control.DeepSeq.deepseq
             (_SourceInfo'_unknownFields x__)
             (Control.DeepSeq.deepseq
                (_SourceInfo'file x__)
                (Control.DeepSeq.deepseq
                   (_SourceInfo'posFrom x__)
                   (Control.DeepSeq.deepseq (_SourceInfo'posTo x__) ())))
{- | Fields :
     
         * 'Proto.Repls.LambdabuffersSource_Fields.column' @:: Lens' SourcePosition Data.Int.Int32@
         * 'Proto.Repls.LambdabuffersSource_Fields.row' @:: Lens' SourcePosition Data.Int.Int32@ -}
data SourcePosition
  = SourcePosition'_constructor {_SourcePosition'column :: !Data.Int.Int32,
                                 _SourcePosition'row :: !Data.Int.Int32,
                                 _SourcePosition'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving stock (Prelude.Eq, Prelude.Ord)
instance Prelude.Show SourcePosition where
  showsPrec _ __x __s
    = Prelude.showChar
        '{'
        (Prelude.showString
           (Data.ProtoLens.showMessageShort __x) (Prelude.showChar '}' __s))
instance Data.ProtoLens.Field.HasField SourcePosition "column" Data.Int.Int32 where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _SourcePosition'column
           (\ x__ y__ -> x__ {_SourcePosition'column = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField SourcePosition "row" Data.Int.Int32 where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _SourcePosition'row (\ x__ y__ -> x__ {_SourcePosition'row = y__}))
        Prelude.id
instance Data.ProtoLens.Message SourcePosition where
  messageName _
    = Data.Text.pack "lambdabuffers.source.SourcePosition"
  packedMessageDescriptor _
    = "\n\
      \\SOSourcePosition\DC2\SYN\n\
      \\ACKcolumn\CAN\SOH \SOH(\ENQR\ACKcolumn\DC2\DLE\n\
      \\ETXrow\CAN\STX \SOH(\ENQR\ETXrow"
  packedFileDescriptor _ = packedFileDescriptor
  fieldsByTag
    = let
        column__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "column"
              (Data.ProtoLens.ScalarField Data.ProtoLens.Int32Field ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Int.Int32)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional (Data.ProtoLens.Field.field @"column")) ::
              Data.ProtoLens.FieldDescriptor SourcePosition
        row__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "row"
              (Data.ProtoLens.ScalarField Data.ProtoLens.Int32Field ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Int.Int32)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional (Data.ProtoLens.Field.field @"row")) ::
              Data.ProtoLens.FieldDescriptor SourcePosition
      in
        Data.Map.fromList
          [(Data.ProtoLens.Tag 1, column__field_descriptor),
           (Data.ProtoLens.Tag 2, row__field_descriptor)]
  unknownFields
    = Lens.Family2.Unchecked.lens
        _SourcePosition'_unknownFields
        (\ x__ y__ -> x__ {_SourcePosition'_unknownFields = y__})
  defMessage
    = SourcePosition'_constructor
        {_SourcePosition'column = Data.ProtoLens.fieldDefault,
         _SourcePosition'row = Data.ProtoLens.fieldDefault,
         _SourcePosition'_unknownFields = []}
  parseMessage
    = let
        loop ::
          SourcePosition
          -> Data.ProtoLens.Encoding.Bytes.Parser SourcePosition
        loop x
          = do end <- Data.ProtoLens.Encoding.Bytes.atEnd
               if end then
                   do (let missing = []
                       in
                         if Prelude.null missing then
                             Prelude.return ()
                         else
                             Prelude.fail
                               ((Prelude.++)
                                  "Missing required fields: "
                                  (Prelude.show (missing :: [Prelude.String]))))
                      Prelude.return
                        (Lens.Family2.over
                           Data.ProtoLens.unknownFields (\ !t -> Prelude.reverse t) x)
               else
                   do tag <- Data.ProtoLens.Encoding.Bytes.getVarInt
                      case tag of
                        8 -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (Prelude.fmap
                                          Prelude.fromIntegral
                                          Data.ProtoLens.Encoding.Bytes.getVarInt)
                                       "column"
                                loop (Lens.Family2.set (Data.ProtoLens.Field.field @"column") y x)
                        16
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (Prelude.fmap
                                          Prelude.fromIntegral
                                          Data.ProtoLens.Encoding.Bytes.getVarInt)
                                       "row"
                                loop (Lens.Family2.set (Data.ProtoLens.Field.field @"row") y x)
                        wire
                          -> do !y <- Data.ProtoLens.Encoding.Wire.parseTaggedValueFromWire
                                        wire
                                loop
                                  (Lens.Family2.over
                                     Data.ProtoLens.unknownFields (\ !t -> (:) y t) x)
      in
        (Data.ProtoLens.Encoding.Bytes.<?>)
          (do loop Data.ProtoLens.defMessage) "SourcePosition"
  buildMessage
    = \ _x
        -> (Data.Monoid.<>)
             (let
                _v = Lens.Family2.view (Data.ProtoLens.Field.field @"column") _x
              in
                if (Prelude.==) _v Data.ProtoLens.fieldDefault then
                    Data.Monoid.mempty
                else
                    (Data.Monoid.<>)
                      (Data.ProtoLens.Encoding.Bytes.putVarInt 8)
                      ((Prelude..)
                         Data.ProtoLens.Encoding.Bytes.putVarInt Prelude.fromIntegral _v))
             ((Data.Monoid.<>)
                (let _v = Lens.Family2.view (Data.ProtoLens.Field.field @"row") _x
                 in
                   if (Prelude.==) _v Data.ProtoLens.fieldDefault then
                       Data.Monoid.mempty
                   else
                       (Data.Monoid.<>)
                         (Data.ProtoLens.Encoding.Bytes.putVarInt 16)
                         ((Prelude..)
                            Data.ProtoLens.Encoding.Bytes.putVarInt Prelude.fromIntegral _v))
                (Data.ProtoLens.Encoding.Wire.buildFieldSet
                   (Lens.Family2.view Data.ProtoLens.unknownFields _x)))
instance Control.DeepSeq.NFData SourcePosition where
  rnf
    = \ x__
        -> Control.DeepSeq.deepseq
             (_SourcePosition'_unknownFields x__)
             (Control.DeepSeq.deepseq
                (_SourcePosition'column x__)
                (Control.DeepSeq.deepseq (_SourcePosition'row x__) ()))
{- | Fields :
     
         * 'Proto.Repls.LambdabuffersSource_Fields.constructors' @:: Lens' Sum (Data.Map.Map Data.Text.Text Product)@
         * 'Proto.Repls.LambdabuffersSource_Fields.sourceInfo' @:: Lens' Sum SourceInfo@
         * 'Proto.Repls.LambdabuffersSource_Fields.maybe'sourceInfo' @:: Lens' Sum (Prelude.Maybe SourceInfo)@ -}
data Sum
  = Sum'_constructor {_Sum'constructors :: !(Data.Map.Map Data.Text.Text Product),
                      _Sum'sourceInfo :: !(Prelude.Maybe SourceInfo),
                      _Sum'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving stock (Prelude.Eq, Prelude.Ord)
instance Prelude.Show Sum where
  showsPrec _ __x __s
    = Prelude.showChar
        '{'
        (Prelude.showString
           (Data.ProtoLens.showMessageShort __x) (Prelude.showChar '}' __s))
instance Data.ProtoLens.Field.HasField Sum "constructors" (Data.Map.Map Data.Text.Text Product) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _Sum'constructors (\ x__ y__ -> x__ {_Sum'constructors = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField Sum "sourceInfo" SourceInfo where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _Sum'sourceInfo (\ x__ y__ -> x__ {_Sum'sourceInfo = y__}))
        (Data.ProtoLens.maybeLens Data.ProtoLens.defMessage)
instance Data.ProtoLens.Field.HasField Sum "maybe'sourceInfo" (Prelude.Maybe SourceInfo) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _Sum'sourceInfo (\ x__ y__ -> x__ {_Sum'sourceInfo = y__}))
        Prelude.id
instance Data.ProtoLens.Message Sum where
  messageName _ = Data.Text.pack "lambdabuffers.source.Sum"
  packedMessageDescriptor _
    = "\n\
      \\ETXSum\DC2O\n\
      \\fconstructors\CAN\SOH \ETX(\v2+.lambdabuffers.source.Sum.ConstructorsEntryR\fconstructors\DC2A\n\
      \\vsource_info\CAN\STX \SOH(\v2 .lambdabuffers.source.SourceInfoR\n\
      \sourceInfo\SUB^\n\
      \\DC1ConstructorsEntry\DC2\DLE\n\
      \\ETXkey\CAN\SOH \SOH(\tR\ETXkey\DC23\n\
      \\ENQvalue\CAN\STX \SOH(\v2\GS.lambdabuffers.source.ProductR\ENQvalue:\STX8\SOH"
  packedFileDescriptor _ = packedFileDescriptor
  fieldsByTag
    = let
        constructors__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "constructors"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor Sum'ConstructorsEntry)
              (Data.ProtoLens.MapField
                 (Data.ProtoLens.Field.field @"key")
                 (Data.ProtoLens.Field.field @"value")
                 (Data.ProtoLens.Field.field @"constructors")) ::
              Data.ProtoLens.FieldDescriptor Sum
        sourceInfo__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "source_info"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor SourceInfo)
              (Data.ProtoLens.OptionalField
                 (Data.ProtoLens.Field.field @"maybe'sourceInfo")) ::
              Data.ProtoLens.FieldDescriptor Sum
      in
        Data.Map.fromList
          [(Data.ProtoLens.Tag 1, constructors__field_descriptor),
           (Data.ProtoLens.Tag 2, sourceInfo__field_descriptor)]
  unknownFields
    = Lens.Family2.Unchecked.lens
        _Sum'_unknownFields (\ x__ y__ -> x__ {_Sum'_unknownFields = y__})
  defMessage
    = Sum'_constructor
        {_Sum'constructors = Data.Map.empty,
         _Sum'sourceInfo = Prelude.Nothing, _Sum'_unknownFields = []}
  parseMessage
    = let
        loop :: Sum -> Data.ProtoLens.Encoding.Bytes.Parser Sum
        loop x
          = do end <- Data.ProtoLens.Encoding.Bytes.atEnd
               if end then
                   do (let missing = []
                       in
                         if Prelude.null missing then
                             Prelude.return ()
                         else
                             Prelude.fail
                               ((Prelude.++)
                                  "Missing required fields: "
                                  (Prelude.show (missing :: [Prelude.String]))))
                      Prelude.return
                        (Lens.Family2.over
                           Data.ProtoLens.unknownFields (\ !t -> Prelude.reverse t) x)
               else
                   do tag <- Data.ProtoLens.Encoding.Bytes.getVarInt
                      case tag of
                        10
                          -> do !(entry :: Sum'ConstructorsEntry) <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                                                       (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                                                           Data.ProtoLens.Encoding.Bytes.isolate
                                                                             (Prelude.fromIntegral
                                                                                len)
                                                                             Data.ProtoLens.parseMessage)
                                                                       "constructors"
                                (let
                                   key = Lens.Family2.view (Data.ProtoLens.Field.field @"key") entry
                                   value
                                     = Lens.Family2.view (Data.ProtoLens.Field.field @"value") entry
                                 in
                                   loop
                                     (Lens.Family2.over
                                        (Data.ProtoLens.Field.field @"constructors")
                                        (\ !t -> Data.Map.insert key value t) x))
                        18
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                           Data.ProtoLens.Encoding.Bytes.isolate
                                             (Prelude.fromIntegral len) Data.ProtoLens.parseMessage)
                                       "source_info"
                                loop
                                  (Lens.Family2.set (Data.ProtoLens.Field.field @"sourceInfo") y x)
                        wire
                          -> do !y <- Data.ProtoLens.Encoding.Wire.parseTaggedValueFromWire
                                        wire
                                loop
                                  (Lens.Family2.over
                                     Data.ProtoLens.unknownFields (\ !t -> (:) y t) x)
      in
        (Data.ProtoLens.Encoding.Bytes.<?>)
          (do loop Data.ProtoLens.defMessage) "Sum"
  buildMessage
    = \ _x
        -> (Data.Monoid.<>)
             (Data.Monoid.mconcat
                (Prelude.map
                   (\ _v
                      -> (Data.Monoid.<>)
                           (Data.ProtoLens.Encoding.Bytes.putVarInt 10)
                           ((Prelude..)
                              (\ bs
                                 -> (Data.Monoid.<>)
                                      (Data.ProtoLens.Encoding.Bytes.putVarInt
                                         (Prelude.fromIntegral (Data.ByteString.length bs)))
                                      (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                              Data.ProtoLens.encodeMessage
                              (Lens.Family2.set
                                 (Data.ProtoLens.Field.field @"key") (Prelude.fst _v)
                                 (Lens.Family2.set
                                    (Data.ProtoLens.Field.field @"value") (Prelude.snd _v)
                                    (Data.ProtoLens.defMessage :: Sum'ConstructorsEntry)))))
                   (Data.Map.toList
                      (Lens.Family2.view
                         (Data.ProtoLens.Field.field @"constructors") _x))))
             ((Data.Monoid.<>)
                (case
                     Lens.Family2.view
                       (Data.ProtoLens.Field.field @"maybe'sourceInfo") _x
                 of
                   Prelude.Nothing -> Data.Monoid.mempty
                   (Prelude.Just _v)
                     -> (Data.Monoid.<>)
                          (Data.ProtoLens.Encoding.Bytes.putVarInt 18)
                          ((Prelude..)
                             (\ bs
                                -> (Data.Monoid.<>)
                                     (Data.ProtoLens.Encoding.Bytes.putVarInt
                                        (Prelude.fromIntegral (Data.ByteString.length bs)))
                                     (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                             Data.ProtoLens.encodeMessage _v))
                (Data.ProtoLens.Encoding.Wire.buildFieldSet
                   (Lens.Family2.view Data.ProtoLens.unknownFields _x)))
instance Control.DeepSeq.NFData Sum where
  rnf
    = \ x__
        -> Control.DeepSeq.deepseq
             (_Sum'_unknownFields x__)
             (Control.DeepSeq.deepseq
                (_Sum'constructors x__)
                (Control.DeepSeq.deepseq (_Sum'sourceInfo x__) ()))
{- | Fields :
     
         * 'Proto.Repls.LambdabuffersSource_Fields.key' @:: Lens' Sum'ConstructorsEntry Data.Text.Text@
         * 'Proto.Repls.LambdabuffersSource_Fields.value' @:: Lens' Sum'ConstructorsEntry Product@
         * 'Proto.Repls.LambdabuffersSource_Fields.maybe'value' @:: Lens' Sum'ConstructorsEntry (Prelude.Maybe Product)@ -}
data Sum'ConstructorsEntry
  = Sum'ConstructorsEntry'_constructor {_Sum'ConstructorsEntry'key :: !Data.Text.Text,
                                        _Sum'ConstructorsEntry'value :: !(Prelude.Maybe Product),
                                        _Sum'ConstructorsEntry'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving stock (Prelude.Eq, Prelude.Ord)
instance Prelude.Show Sum'ConstructorsEntry where
  showsPrec _ __x __s
    = Prelude.showChar
        '{'
        (Prelude.showString
           (Data.ProtoLens.showMessageShort __x) (Prelude.showChar '}' __s))
instance Data.ProtoLens.Field.HasField Sum'ConstructorsEntry "key" Data.Text.Text where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _Sum'ConstructorsEntry'key
           (\ x__ y__ -> x__ {_Sum'ConstructorsEntry'key = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField Sum'ConstructorsEntry "value" Product where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _Sum'ConstructorsEntry'value
           (\ x__ y__ -> x__ {_Sum'ConstructorsEntry'value = y__}))
        (Data.ProtoLens.maybeLens Data.ProtoLens.defMessage)
instance Data.ProtoLens.Field.HasField Sum'ConstructorsEntry "maybe'value" (Prelude.Maybe Product) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _Sum'ConstructorsEntry'value
           (\ x__ y__ -> x__ {_Sum'ConstructorsEntry'value = y__}))
        Prelude.id
instance Data.ProtoLens.Message Sum'ConstructorsEntry where
  messageName _
    = Data.Text.pack "lambdabuffers.source.Sum.ConstructorsEntry"
  packedMessageDescriptor _
    = "\n\
      \\DC1ConstructorsEntry\DC2\DLE\n\
      \\ETXkey\CAN\SOH \SOH(\tR\ETXkey\DC23\n\
      \\ENQvalue\CAN\STX \SOH(\v2\GS.lambdabuffers.source.ProductR\ENQvalue:\STX8\SOH"
  packedFileDescriptor _ = packedFileDescriptor
  fieldsByTag
    = let
        key__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "key"
              (Data.ProtoLens.ScalarField Data.ProtoLens.StringField ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Text.Text)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional (Data.ProtoLens.Field.field @"key")) ::
              Data.ProtoLens.FieldDescriptor Sum'ConstructorsEntry
        value__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "value"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor Product)
              (Data.ProtoLens.OptionalField
                 (Data.ProtoLens.Field.field @"maybe'value")) ::
              Data.ProtoLens.FieldDescriptor Sum'ConstructorsEntry
      in
        Data.Map.fromList
          [(Data.ProtoLens.Tag 1, key__field_descriptor),
           (Data.ProtoLens.Tag 2, value__field_descriptor)]
  unknownFields
    = Lens.Family2.Unchecked.lens
        _Sum'ConstructorsEntry'_unknownFields
        (\ x__ y__ -> x__ {_Sum'ConstructorsEntry'_unknownFields = y__})
  defMessage
    = Sum'ConstructorsEntry'_constructor
        {_Sum'ConstructorsEntry'key = Data.ProtoLens.fieldDefault,
         _Sum'ConstructorsEntry'value = Prelude.Nothing,
         _Sum'ConstructorsEntry'_unknownFields = []}
  parseMessage
    = let
        loop ::
          Sum'ConstructorsEntry
          -> Data.ProtoLens.Encoding.Bytes.Parser Sum'ConstructorsEntry
        loop x
          = do end <- Data.ProtoLens.Encoding.Bytes.atEnd
               if end then
                   do (let missing = []
                       in
                         if Prelude.null missing then
                             Prelude.return ()
                         else
                             Prelude.fail
                               ((Prelude.++)
                                  "Missing required fields: "
                                  (Prelude.show (missing :: [Prelude.String]))))
                      Prelude.return
                        (Lens.Family2.over
                           Data.ProtoLens.unknownFields (\ !t -> Prelude.reverse t) x)
               else
                   do tag <- Data.ProtoLens.Encoding.Bytes.getVarInt
                      case tag of
                        10
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do value <- do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                                       Data.ProtoLens.Encoding.Bytes.getBytes
                                                         (Prelude.fromIntegral len)
                                           Data.ProtoLens.Encoding.Bytes.runEither
                                             (case Data.Text.Encoding.decodeUtf8' value of
                                                (Prelude.Left err)
                                                  -> Prelude.Left (Prelude.show err)
                                                (Prelude.Right r) -> Prelude.Right r))
                                       "key"
                                loop (Lens.Family2.set (Data.ProtoLens.Field.field @"key") y x)
                        18
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                           Data.ProtoLens.Encoding.Bytes.isolate
                                             (Prelude.fromIntegral len) Data.ProtoLens.parseMessage)
                                       "value"
                                loop (Lens.Family2.set (Data.ProtoLens.Field.field @"value") y x)
                        wire
                          -> do !y <- Data.ProtoLens.Encoding.Wire.parseTaggedValueFromWire
                                        wire
                                loop
                                  (Lens.Family2.over
                                     Data.ProtoLens.unknownFields (\ !t -> (:) y t) x)
      in
        (Data.ProtoLens.Encoding.Bytes.<?>)
          (do loop Data.ProtoLens.defMessage) "ConstructorsEntry"
  buildMessage
    = \ _x
        -> (Data.Monoid.<>)
             (let _v = Lens.Family2.view (Data.ProtoLens.Field.field @"key") _x
              in
                if (Prelude.==) _v Data.ProtoLens.fieldDefault then
                    Data.Monoid.mempty
                else
                    (Data.Monoid.<>)
                      (Data.ProtoLens.Encoding.Bytes.putVarInt 10)
                      ((Prelude..)
                         (\ bs
                            -> (Data.Monoid.<>)
                                 (Data.ProtoLens.Encoding.Bytes.putVarInt
                                    (Prelude.fromIntegral (Data.ByteString.length bs)))
                                 (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                         Data.Text.Encoding.encodeUtf8 _v))
             ((Data.Monoid.<>)
                (case
                     Lens.Family2.view (Data.ProtoLens.Field.field @"maybe'value") _x
                 of
                   Prelude.Nothing -> Data.Monoid.mempty
                   (Prelude.Just _v)
                     -> (Data.Monoid.<>)
                          (Data.ProtoLens.Encoding.Bytes.putVarInt 18)
                          ((Prelude..)
                             (\ bs
                                -> (Data.Monoid.<>)
                                     (Data.ProtoLens.Encoding.Bytes.putVarInt
                                        (Prelude.fromIntegral (Data.ByteString.length bs)))
                                     (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                             Data.ProtoLens.encodeMessage _v))
                (Data.ProtoLens.Encoding.Wire.buildFieldSet
                   (Lens.Family2.view Data.ProtoLens.unknownFields _x)))
instance Control.DeepSeq.NFData Sum'ConstructorsEntry where
  rnf
    = \ x__
        -> Control.DeepSeq.deepseq
             (_Sum'ConstructorsEntry'_unknownFields x__)
             (Control.DeepSeq.deepseq
                (_Sum'ConstructorsEntry'key x__)
                (Control.DeepSeq.deepseq (_Sum'ConstructorsEntry'value x__) ()))
{- | Fields :
     
         * 'Proto.Repls.LambdabuffersSource_Fields.sourceInfo' @:: Lens' Type SourceInfo@
         * 'Proto.Repls.LambdabuffersSource_Fields.maybe'sourceInfo' @:: Lens' Type (Prelude.Maybe SourceInfo)@
         * 'Proto.Repls.LambdabuffersSource_Fields.maybe'type'' @:: Lens' Type (Prelude.Maybe Type'Type)@
         * 'Proto.Repls.LambdabuffersSource_Fields.maybe'typeVar' @:: Lens' Type (Prelude.Maybe TypeVar)@
         * 'Proto.Repls.LambdabuffersSource_Fields.typeVar' @:: Lens' Type TypeVar@
         * 'Proto.Repls.LambdabuffersSource_Fields.maybe'typeCon' @:: Lens' Type (Prelude.Maybe TypeCon)@
         * 'Proto.Repls.LambdabuffersSource_Fields.typeCon' @:: Lens' Type TypeCon@
         * 'Proto.Repls.LambdabuffersSource_Fields.maybe'typeApp' @:: Lens' Type (Prelude.Maybe TypeApp)@
         * 'Proto.Repls.LambdabuffersSource_Fields.typeApp' @:: Lens' Type TypeApp@
         * 'Proto.Repls.LambdabuffersSource_Fields.maybe'typeRef' @:: Lens' Type (Prelude.Maybe TypeRef)@
         * 'Proto.Repls.LambdabuffersSource_Fields.typeRef' @:: Lens' Type TypeRef@ -}
data Type
  = Type'_constructor {_Type'sourceInfo :: !(Prelude.Maybe SourceInfo),
                       _Type'type' :: !(Prelude.Maybe Type'Type),
                       _Type'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving stock (Prelude.Eq, Prelude.Ord)
instance Prelude.Show Type where
  showsPrec _ __x __s
    = Prelude.showChar
        '{'
        (Prelude.showString
           (Data.ProtoLens.showMessageShort __x) (Prelude.showChar '}' __s))
data Type'Type
  = Type'TypeVar !TypeVar |
    Type'TypeCon !TypeCon |
    Type'TypeApp !TypeApp |
    Type'TypeRef !TypeRef
  deriving stock (Prelude.Show, Prelude.Eq, Prelude.Ord)
instance Data.ProtoLens.Field.HasField Type "sourceInfo" SourceInfo where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _Type'sourceInfo (\ x__ y__ -> x__ {_Type'sourceInfo = y__}))
        (Data.ProtoLens.maybeLens Data.ProtoLens.defMessage)
instance Data.ProtoLens.Field.HasField Type "maybe'sourceInfo" (Prelude.Maybe SourceInfo) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _Type'sourceInfo (\ x__ y__ -> x__ {_Type'sourceInfo = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField Type "maybe'type'" (Prelude.Maybe Type'Type) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _Type'type' (\ x__ y__ -> x__ {_Type'type' = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField Type "maybe'typeVar" (Prelude.Maybe TypeVar) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _Type'type' (\ x__ y__ -> x__ {_Type'type' = y__}))
        (Lens.Family2.Unchecked.lens
           (\ x__
              -> case x__ of
                   (Prelude.Just (Type'TypeVar x__val)) -> Prelude.Just x__val
                   _otherwise -> Prelude.Nothing)
           (\ _ y__ -> Prelude.fmap Type'TypeVar y__))
instance Data.ProtoLens.Field.HasField Type "typeVar" TypeVar where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _Type'type' (\ x__ y__ -> x__ {_Type'type' = y__}))
        ((Prelude..)
           (Lens.Family2.Unchecked.lens
              (\ x__
                 -> case x__ of
                      (Prelude.Just (Type'TypeVar x__val)) -> Prelude.Just x__val
                      _otherwise -> Prelude.Nothing)
              (\ _ y__ -> Prelude.fmap Type'TypeVar y__))
           (Data.ProtoLens.maybeLens Data.ProtoLens.defMessage))
instance Data.ProtoLens.Field.HasField Type "maybe'typeCon" (Prelude.Maybe TypeCon) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _Type'type' (\ x__ y__ -> x__ {_Type'type' = y__}))
        (Lens.Family2.Unchecked.lens
           (\ x__
              -> case x__ of
                   (Prelude.Just (Type'TypeCon x__val)) -> Prelude.Just x__val
                   _otherwise -> Prelude.Nothing)
           (\ _ y__ -> Prelude.fmap Type'TypeCon y__))
instance Data.ProtoLens.Field.HasField Type "typeCon" TypeCon where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _Type'type' (\ x__ y__ -> x__ {_Type'type' = y__}))
        ((Prelude..)
           (Lens.Family2.Unchecked.lens
              (\ x__
                 -> case x__ of
                      (Prelude.Just (Type'TypeCon x__val)) -> Prelude.Just x__val
                      _otherwise -> Prelude.Nothing)
              (\ _ y__ -> Prelude.fmap Type'TypeCon y__))
           (Data.ProtoLens.maybeLens Data.ProtoLens.defMessage))
instance Data.ProtoLens.Field.HasField Type "maybe'typeApp" (Prelude.Maybe TypeApp) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _Type'type' (\ x__ y__ -> x__ {_Type'type' = y__}))
        (Lens.Family2.Unchecked.lens
           (\ x__
              -> case x__ of
                   (Prelude.Just (Type'TypeApp x__val)) -> Prelude.Just x__val
                   _otherwise -> Prelude.Nothing)
           (\ _ y__ -> Prelude.fmap Type'TypeApp y__))
instance Data.ProtoLens.Field.HasField Type "typeApp" TypeApp where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _Type'type' (\ x__ y__ -> x__ {_Type'type' = y__}))
        ((Prelude..)
           (Lens.Family2.Unchecked.lens
              (\ x__
                 -> case x__ of
                      (Prelude.Just (Type'TypeApp x__val)) -> Prelude.Just x__val
                      _otherwise -> Prelude.Nothing)
              (\ _ y__ -> Prelude.fmap Type'TypeApp y__))
           (Data.ProtoLens.maybeLens Data.ProtoLens.defMessage))
instance Data.ProtoLens.Field.HasField Type "maybe'typeRef" (Prelude.Maybe TypeRef) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _Type'type' (\ x__ y__ -> x__ {_Type'type' = y__}))
        (Lens.Family2.Unchecked.lens
           (\ x__
              -> case x__ of
                   (Prelude.Just (Type'TypeRef x__val)) -> Prelude.Just x__val
                   _otherwise -> Prelude.Nothing)
           (\ _ y__ -> Prelude.fmap Type'TypeRef y__))
instance Data.ProtoLens.Field.HasField Type "typeRef" TypeRef where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _Type'type' (\ x__ y__ -> x__ {_Type'type' = y__}))
        ((Prelude..)
           (Lens.Family2.Unchecked.lens
              (\ x__
                 -> case x__ of
                      (Prelude.Just (Type'TypeRef x__val)) -> Prelude.Just x__val
                      _otherwise -> Prelude.Nothing)
              (\ _ y__ -> Prelude.fmap Type'TypeRef y__))
           (Data.ProtoLens.maybeLens Data.ProtoLens.defMessage))
instance Data.ProtoLens.Message Type where
  messageName _ = Data.Text.pack "lambdabuffers.source.Type"
  packedMessageDescriptor _
    = "\n\
      \\EOTType\DC2:\n\
      \\btype_var\CAN\SOH \SOH(\v2\GS.lambdabuffers.source.TypeVarH\NULR\atypeVar\DC2:\n\
      \\btype_con\CAN\STX \SOH(\v2\GS.lambdabuffers.source.TypeConH\NULR\atypeCon\DC2:\n\
      \\btype_app\CAN\ETX \SOH(\v2\GS.lambdabuffers.source.TypeAppH\NULR\atypeApp\DC2:\n\
      \\btype_ref\CAN\EOT \SOH(\v2\GS.lambdabuffers.source.TypeRefH\NULR\atypeRef\DC2A\n\
      \\vsource_info\CAN\ENQ \SOH(\v2 .lambdabuffers.source.SourceInfoR\n\
      \sourceInfoB\ACK\n\
      \\EOTtype"
  packedFileDescriptor _ = packedFileDescriptor
  fieldsByTag
    = let
        sourceInfo__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "source_info"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor SourceInfo)
              (Data.ProtoLens.OptionalField
                 (Data.ProtoLens.Field.field @"maybe'sourceInfo")) ::
              Data.ProtoLens.FieldDescriptor Type
        typeVar__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "type_var"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor TypeVar)
              (Data.ProtoLens.OptionalField
                 (Data.ProtoLens.Field.field @"maybe'typeVar")) ::
              Data.ProtoLens.FieldDescriptor Type
        typeCon__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "type_con"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor TypeCon)
              (Data.ProtoLens.OptionalField
                 (Data.ProtoLens.Field.field @"maybe'typeCon")) ::
              Data.ProtoLens.FieldDescriptor Type
        typeApp__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "type_app"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor TypeApp)
              (Data.ProtoLens.OptionalField
                 (Data.ProtoLens.Field.field @"maybe'typeApp")) ::
              Data.ProtoLens.FieldDescriptor Type
        typeRef__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "type_ref"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor TypeRef)
              (Data.ProtoLens.OptionalField
                 (Data.ProtoLens.Field.field @"maybe'typeRef")) ::
              Data.ProtoLens.FieldDescriptor Type
      in
        Data.Map.fromList
          [(Data.ProtoLens.Tag 5, sourceInfo__field_descriptor),
           (Data.ProtoLens.Tag 1, typeVar__field_descriptor),
           (Data.ProtoLens.Tag 2, typeCon__field_descriptor),
           (Data.ProtoLens.Tag 3, typeApp__field_descriptor),
           (Data.ProtoLens.Tag 4, typeRef__field_descriptor)]
  unknownFields
    = Lens.Family2.Unchecked.lens
        _Type'_unknownFields
        (\ x__ y__ -> x__ {_Type'_unknownFields = y__})
  defMessage
    = Type'_constructor
        {_Type'sourceInfo = Prelude.Nothing, _Type'type' = Prelude.Nothing,
         _Type'_unknownFields = []}
  parseMessage
    = let
        loop :: Type -> Data.ProtoLens.Encoding.Bytes.Parser Type
        loop x
          = do end <- Data.ProtoLens.Encoding.Bytes.atEnd
               if end then
                   do (let missing = []
                       in
                         if Prelude.null missing then
                             Prelude.return ()
                         else
                             Prelude.fail
                               ((Prelude.++)
                                  "Missing required fields: "
                                  (Prelude.show (missing :: [Prelude.String]))))
                      Prelude.return
                        (Lens.Family2.over
                           Data.ProtoLens.unknownFields (\ !t -> Prelude.reverse t) x)
               else
                   do tag <- Data.ProtoLens.Encoding.Bytes.getVarInt
                      case tag of
                        42
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                           Data.ProtoLens.Encoding.Bytes.isolate
                                             (Prelude.fromIntegral len) Data.ProtoLens.parseMessage)
                                       "source_info"
                                loop
                                  (Lens.Family2.set (Data.ProtoLens.Field.field @"sourceInfo") y x)
                        10
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                           Data.ProtoLens.Encoding.Bytes.isolate
                                             (Prelude.fromIntegral len) Data.ProtoLens.parseMessage)
                                       "type_var"
                                loop (Lens.Family2.set (Data.ProtoLens.Field.field @"typeVar") y x)
                        18
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                           Data.ProtoLens.Encoding.Bytes.isolate
                                             (Prelude.fromIntegral len) Data.ProtoLens.parseMessage)
                                       "type_con"
                                loop (Lens.Family2.set (Data.ProtoLens.Field.field @"typeCon") y x)
                        26
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                           Data.ProtoLens.Encoding.Bytes.isolate
                                             (Prelude.fromIntegral len) Data.ProtoLens.parseMessage)
                                       "type_app"
                                loop (Lens.Family2.set (Data.ProtoLens.Field.field @"typeApp") y x)
                        34
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                           Data.ProtoLens.Encoding.Bytes.isolate
                                             (Prelude.fromIntegral len) Data.ProtoLens.parseMessage)
                                       "type_ref"
                                loop (Lens.Family2.set (Data.ProtoLens.Field.field @"typeRef") y x)
                        wire
                          -> do !y <- Data.ProtoLens.Encoding.Wire.parseTaggedValueFromWire
                                        wire
                                loop
                                  (Lens.Family2.over
                                     Data.ProtoLens.unknownFields (\ !t -> (:) y t) x)
      in
        (Data.ProtoLens.Encoding.Bytes.<?>)
          (do loop Data.ProtoLens.defMessage) "Type"
  buildMessage
    = \ _x
        -> (Data.Monoid.<>)
             (case
                  Lens.Family2.view
                    (Data.ProtoLens.Field.field @"maybe'sourceInfo") _x
              of
                Prelude.Nothing -> Data.Monoid.mempty
                (Prelude.Just _v)
                  -> (Data.Monoid.<>)
                       (Data.ProtoLens.Encoding.Bytes.putVarInt 42)
                       ((Prelude..)
                          (\ bs
                             -> (Data.Monoid.<>)
                                  (Data.ProtoLens.Encoding.Bytes.putVarInt
                                     (Prelude.fromIntegral (Data.ByteString.length bs)))
                                  (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                          Data.ProtoLens.encodeMessage _v))
             ((Data.Monoid.<>)
                (case
                     Lens.Family2.view (Data.ProtoLens.Field.field @"maybe'type'") _x
                 of
                   Prelude.Nothing -> Data.Monoid.mempty
                   (Prelude.Just (Type'TypeVar v))
                     -> (Data.Monoid.<>)
                          (Data.ProtoLens.Encoding.Bytes.putVarInt 10)
                          ((Prelude..)
                             (\ bs
                                -> (Data.Monoid.<>)
                                     (Data.ProtoLens.Encoding.Bytes.putVarInt
                                        (Prelude.fromIntegral (Data.ByteString.length bs)))
                                     (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                             Data.ProtoLens.encodeMessage v)
                   (Prelude.Just (Type'TypeCon v))
                     -> (Data.Monoid.<>)
                          (Data.ProtoLens.Encoding.Bytes.putVarInt 18)
                          ((Prelude..)
                             (\ bs
                                -> (Data.Monoid.<>)
                                     (Data.ProtoLens.Encoding.Bytes.putVarInt
                                        (Prelude.fromIntegral (Data.ByteString.length bs)))
                                     (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                             Data.ProtoLens.encodeMessage v)
                   (Prelude.Just (Type'TypeApp v))
                     -> (Data.Monoid.<>)
                          (Data.ProtoLens.Encoding.Bytes.putVarInt 26)
                          ((Prelude..)
                             (\ bs
                                -> (Data.Monoid.<>)
                                     (Data.ProtoLens.Encoding.Bytes.putVarInt
                                        (Prelude.fromIntegral (Data.ByteString.length bs)))
                                     (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                             Data.ProtoLens.encodeMessage v)
                   (Prelude.Just (Type'TypeRef v))
                     -> (Data.Monoid.<>)
                          (Data.ProtoLens.Encoding.Bytes.putVarInt 34)
                          ((Prelude..)
                             (\ bs
                                -> (Data.Monoid.<>)
                                     (Data.ProtoLens.Encoding.Bytes.putVarInt
                                        (Prelude.fromIntegral (Data.ByteString.length bs)))
                                     (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                             Data.ProtoLens.encodeMessage v))
                (Data.ProtoLens.Encoding.Wire.buildFieldSet
                   (Lens.Family2.view Data.ProtoLens.unknownFields _x)))
instance Control.DeepSeq.NFData Type where
  rnf
    = \ x__
        -> Control.DeepSeq.deepseq
             (_Type'_unknownFields x__)
             (Control.DeepSeq.deepseq
                (_Type'sourceInfo x__)
                (Control.DeepSeq.deepseq (_Type'type' x__) ()))
instance Control.DeepSeq.NFData Type'Type where
  rnf (Type'TypeVar x__) = Control.DeepSeq.rnf x__
  rnf (Type'TypeCon x__) = Control.DeepSeq.rnf x__
  rnf (Type'TypeApp x__) = Control.DeepSeq.rnf x__
  rnf (Type'TypeRef x__) = Control.DeepSeq.rnf x__
_Type'TypeVar :: Data.ProtoLens.Prism.Prism' Type'Type TypeVar
_Type'TypeVar
  = Data.ProtoLens.Prism.prism'
      Type'TypeVar
      (\ p__
         -> case p__ of
              (Type'TypeVar p__val) -> Prelude.Just p__val
              _otherwise -> Prelude.Nothing)
_Type'TypeCon :: Data.ProtoLens.Prism.Prism' Type'Type TypeCon
_Type'TypeCon
  = Data.ProtoLens.Prism.prism'
      Type'TypeCon
      (\ p__
         -> case p__ of
              (Type'TypeCon p__val) -> Prelude.Just p__val
              _otherwise -> Prelude.Nothing)
_Type'TypeApp :: Data.ProtoLens.Prism.Prism' Type'Type TypeApp
_Type'TypeApp
  = Data.ProtoLens.Prism.prism'
      Type'TypeApp
      (\ p__
         -> case p__ of
              (Type'TypeApp p__val) -> Prelude.Just p__val
              _otherwise -> Prelude.Nothing)
_Type'TypeRef :: Data.ProtoLens.Prism.Prism' Type'Type TypeRef
_Type'TypeRef
  = Data.ProtoLens.Prism.prism'
      Type'TypeRef
      (\ p__
         -> case p__ of
              (Type'TypeRef p__val) -> Prelude.Just p__val
              _otherwise -> Prelude.Nothing)
{- | Fields :
     
         * 'Proto.Repls.LambdabuffersSource_Fields.typeAbs' @:: Lens' TypeApp Type@
         * 'Proto.Repls.LambdabuffersSource_Fields.maybe'typeAbs' @:: Lens' TypeApp (Prelude.Maybe Type)@
         * 'Proto.Repls.LambdabuffersSource_Fields.typeArg' @:: Lens' TypeApp Type@
         * 'Proto.Repls.LambdabuffersSource_Fields.maybe'typeArg' @:: Lens' TypeApp (Prelude.Maybe Type)@
         * 'Proto.Repls.LambdabuffersSource_Fields.sourceInfo' @:: Lens' TypeApp SourceInfo@
         * 'Proto.Repls.LambdabuffersSource_Fields.maybe'sourceInfo' @:: Lens' TypeApp (Prelude.Maybe SourceInfo)@ -}
data TypeApp
  = TypeApp'_constructor {_TypeApp'typeAbs :: !(Prelude.Maybe Type),
                          _TypeApp'typeArg :: !(Prelude.Maybe Type),
                          _TypeApp'sourceInfo :: !(Prelude.Maybe SourceInfo),
                          _TypeApp'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving stock (Prelude.Eq, Prelude.Ord)
instance Prelude.Show TypeApp where
  showsPrec _ __x __s
    = Prelude.showChar
        '{'
        (Prelude.showString
           (Data.ProtoLens.showMessageShort __x) (Prelude.showChar '}' __s))
instance Data.ProtoLens.Field.HasField TypeApp "typeAbs" Type where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _TypeApp'typeAbs (\ x__ y__ -> x__ {_TypeApp'typeAbs = y__}))
        (Data.ProtoLens.maybeLens Data.ProtoLens.defMessage)
instance Data.ProtoLens.Field.HasField TypeApp "maybe'typeAbs" (Prelude.Maybe Type) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _TypeApp'typeAbs (\ x__ y__ -> x__ {_TypeApp'typeAbs = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField TypeApp "typeArg" Type where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _TypeApp'typeArg (\ x__ y__ -> x__ {_TypeApp'typeArg = y__}))
        (Data.ProtoLens.maybeLens Data.ProtoLens.defMessage)
instance Data.ProtoLens.Field.HasField TypeApp "maybe'typeArg" (Prelude.Maybe Type) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _TypeApp'typeArg (\ x__ y__ -> x__ {_TypeApp'typeArg = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField TypeApp "sourceInfo" SourceInfo where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _TypeApp'sourceInfo (\ x__ y__ -> x__ {_TypeApp'sourceInfo = y__}))
        (Data.ProtoLens.maybeLens Data.ProtoLens.defMessage)
instance Data.ProtoLens.Field.HasField TypeApp "maybe'sourceInfo" (Prelude.Maybe SourceInfo) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _TypeApp'sourceInfo (\ x__ y__ -> x__ {_TypeApp'sourceInfo = y__}))
        Prelude.id
instance Data.ProtoLens.Message TypeApp where
  messageName _ = Data.Text.pack "lambdabuffers.source.TypeApp"
  packedMessageDescriptor _
    = "\n\
      \\aTypeApp\DC25\n\
      \\btype_abs\CAN\SOH \SOH(\v2\SUB.lambdabuffers.source.TypeR\atypeAbs\DC25\n\
      \\btype_arg\CAN\STX \SOH(\v2\SUB.lambdabuffers.source.TypeR\atypeArg\DC2A\n\
      \\vsource_info\CAN\ETX \SOH(\v2 .lambdabuffers.source.SourceInfoR\n\
      \sourceInfo"
  packedFileDescriptor _ = packedFileDescriptor
  fieldsByTag
    = let
        typeAbs__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "type_abs"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor Type)
              (Data.ProtoLens.OptionalField
                 (Data.ProtoLens.Field.field @"maybe'typeAbs")) ::
              Data.ProtoLens.FieldDescriptor TypeApp
        typeArg__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "type_arg"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor Type)
              (Data.ProtoLens.OptionalField
                 (Data.ProtoLens.Field.field @"maybe'typeArg")) ::
              Data.ProtoLens.FieldDescriptor TypeApp
        sourceInfo__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "source_info"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor SourceInfo)
              (Data.ProtoLens.OptionalField
                 (Data.ProtoLens.Field.field @"maybe'sourceInfo")) ::
              Data.ProtoLens.FieldDescriptor TypeApp
      in
        Data.Map.fromList
          [(Data.ProtoLens.Tag 1, typeAbs__field_descriptor),
           (Data.ProtoLens.Tag 2, typeArg__field_descriptor),
           (Data.ProtoLens.Tag 3, sourceInfo__field_descriptor)]
  unknownFields
    = Lens.Family2.Unchecked.lens
        _TypeApp'_unknownFields
        (\ x__ y__ -> x__ {_TypeApp'_unknownFields = y__})
  defMessage
    = TypeApp'_constructor
        {_TypeApp'typeAbs = Prelude.Nothing,
         _TypeApp'typeArg = Prelude.Nothing,
         _TypeApp'sourceInfo = Prelude.Nothing,
         _TypeApp'_unknownFields = []}
  parseMessage
    = let
        loop :: TypeApp -> Data.ProtoLens.Encoding.Bytes.Parser TypeApp
        loop x
          = do end <- Data.ProtoLens.Encoding.Bytes.atEnd
               if end then
                   do (let missing = []
                       in
                         if Prelude.null missing then
                             Prelude.return ()
                         else
                             Prelude.fail
                               ((Prelude.++)
                                  "Missing required fields: "
                                  (Prelude.show (missing :: [Prelude.String]))))
                      Prelude.return
                        (Lens.Family2.over
                           Data.ProtoLens.unknownFields (\ !t -> Prelude.reverse t) x)
               else
                   do tag <- Data.ProtoLens.Encoding.Bytes.getVarInt
                      case tag of
                        10
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                           Data.ProtoLens.Encoding.Bytes.isolate
                                             (Prelude.fromIntegral len) Data.ProtoLens.parseMessage)
                                       "type_abs"
                                loop (Lens.Family2.set (Data.ProtoLens.Field.field @"typeAbs") y x)
                        18
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                           Data.ProtoLens.Encoding.Bytes.isolate
                                             (Prelude.fromIntegral len) Data.ProtoLens.parseMessage)
                                       "type_arg"
                                loop (Lens.Family2.set (Data.ProtoLens.Field.field @"typeArg") y x)
                        26
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                           Data.ProtoLens.Encoding.Bytes.isolate
                                             (Prelude.fromIntegral len) Data.ProtoLens.parseMessage)
                                       "source_info"
                                loop
                                  (Lens.Family2.set (Data.ProtoLens.Field.field @"sourceInfo") y x)
                        wire
                          -> do !y <- Data.ProtoLens.Encoding.Wire.parseTaggedValueFromWire
                                        wire
                                loop
                                  (Lens.Family2.over
                                     Data.ProtoLens.unknownFields (\ !t -> (:) y t) x)
      in
        (Data.ProtoLens.Encoding.Bytes.<?>)
          (do loop Data.ProtoLens.defMessage) "TypeApp"
  buildMessage
    = \ _x
        -> (Data.Monoid.<>)
             (case
                  Lens.Family2.view (Data.ProtoLens.Field.field @"maybe'typeAbs") _x
              of
                Prelude.Nothing -> Data.Monoid.mempty
                (Prelude.Just _v)
                  -> (Data.Monoid.<>)
                       (Data.ProtoLens.Encoding.Bytes.putVarInt 10)
                       ((Prelude..)
                          (\ bs
                             -> (Data.Monoid.<>)
                                  (Data.ProtoLens.Encoding.Bytes.putVarInt
                                     (Prelude.fromIntegral (Data.ByteString.length bs)))
                                  (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                          Data.ProtoLens.encodeMessage _v))
             ((Data.Monoid.<>)
                (case
                     Lens.Family2.view (Data.ProtoLens.Field.field @"maybe'typeArg") _x
                 of
                   Prelude.Nothing -> Data.Monoid.mempty
                   (Prelude.Just _v)
                     -> (Data.Monoid.<>)
                          (Data.ProtoLens.Encoding.Bytes.putVarInt 18)
                          ((Prelude..)
                             (\ bs
                                -> (Data.Monoid.<>)
                                     (Data.ProtoLens.Encoding.Bytes.putVarInt
                                        (Prelude.fromIntegral (Data.ByteString.length bs)))
                                     (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                             Data.ProtoLens.encodeMessage _v))
                ((Data.Monoid.<>)
                   (case
                        Lens.Family2.view
                          (Data.ProtoLens.Field.field @"maybe'sourceInfo") _x
                    of
                      Prelude.Nothing -> Data.Monoid.mempty
                      (Prelude.Just _v)
                        -> (Data.Monoid.<>)
                             (Data.ProtoLens.Encoding.Bytes.putVarInt 26)
                             ((Prelude..)
                                (\ bs
                                   -> (Data.Monoid.<>)
                                        (Data.ProtoLens.Encoding.Bytes.putVarInt
                                           (Prelude.fromIntegral (Data.ByteString.length bs)))
                                        (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                                Data.ProtoLens.encodeMessage _v))
                   (Data.ProtoLens.Encoding.Wire.buildFieldSet
                      (Lens.Family2.view Data.ProtoLens.unknownFields _x))))
instance Control.DeepSeq.NFData TypeApp where
  rnf
    = \ x__
        -> Control.DeepSeq.deepseq
             (_TypeApp'_unknownFields x__)
             (Control.DeepSeq.deepseq
                (_TypeApp'typeAbs x__)
                (Control.DeepSeq.deepseq
                   (_TypeApp'typeArg x__)
                   (Control.DeepSeq.deepseq (_TypeApp'sourceInfo x__) ())))
{- | Fields :
     
         * 'Proto.Repls.LambdabuffersSource_Fields.sourceInfo' @:: Lens' TypeBody SourceInfo@
         * 'Proto.Repls.LambdabuffersSource_Fields.maybe'sourceInfo' @:: Lens' TypeBody (Prelude.Maybe SourceInfo)@
         * 'Proto.Repls.LambdabuffersSource_Fields.maybe'body' @:: Lens' TypeBody (Prelude.Maybe TypeBody'Body)@
         * 'Proto.Repls.LambdabuffersSource_Fields.maybe'opaque' @:: Lens' TypeBody (Prelude.Maybe Opaque)@
         * 'Proto.Repls.LambdabuffersSource_Fields.opaque' @:: Lens' TypeBody Opaque@
         * 'Proto.Repls.LambdabuffersSource_Fields.maybe'sum' @:: Lens' TypeBody (Prelude.Maybe Sum)@
         * 'Proto.Repls.LambdabuffersSource_Fields.sum' @:: Lens' TypeBody Sum@ -}
data TypeBody
  = TypeBody'_constructor {_TypeBody'sourceInfo :: !(Prelude.Maybe SourceInfo),
                           _TypeBody'body :: !(Prelude.Maybe TypeBody'Body),
                           _TypeBody'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving stock (Prelude.Eq, Prelude.Ord)
instance Prelude.Show TypeBody where
  showsPrec _ __x __s
    = Prelude.showChar
        '{'
        (Prelude.showString
           (Data.ProtoLens.showMessageShort __x) (Prelude.showChar '}' __s))
data TypeBody'Body
  = TypeBody'Opaque !Opaque | TypeBody'Sum !Sum
  deriving stock (Prelude.Show, Prelude.Eq, Prelude.Ord)
instance Data.ProtoLens.Field.HasField TypeBody "sourceInfo" SourceInfo where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _TypeBody'sourceInfo
           (\ x__ y__ -> x__ {_TypeBody'sourceInfo = y__}))
        (Data.ProtoLens.maybeLens Data.ProtoLens.defMessage)
instance Data.ProtoLens.Field.HasField TypeBody "maybe'sourceInfo" (Prelude.Maybe SourceInfo) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _TypeBody'sourceInfo
           (\ x__ y__ -> x__ {_TypeBody'sourceInfo = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField TypeBody "maybe'body" (Prelude.Maybe TypeBody'Body) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _TypeBody'body (\ x__ y__ -> x__ {_TypeBody'body = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField TypeBody "maybe'opaque" (Prelude.Maybe Opaque) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _TypeBody'body (\ x__ y__ -> x__ {_TypeBody'body = y__}))
        (Lens.Family2.Unchecked.lens
           (\ x__
              -> case x__ of
                   (Prelude.Just (TypeBody'Opaque x__val)) -> Prelude.Just x__val
                   _otherwise -> Prelude.Nothing)
           (\ _ y__ -> Prelude.fmap TypeBody'Opaque y__))
instance Data.ProtoLens.Field.HasField TypeBody "opaque" Opaque where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _TypeBody'body (\ x__ y__ -> x__ {_TypeBody'body = y__}))
        ((Prelude..)
           (Lens.Family2.Unchecked.lens
              (\ x__
                 -> case x__ of
                      (Prelude.Just (TypeBody'Opaque x__val)) -> Prelude.Just x__val
                      _otherwise -> Prelude.Nothing)
              (\ _ y__ -> Prelude.fmap TypeBody'Opaque y__))
           (Data.ProtoLens.maybeLens Data.ProtoLens.defMessage))
instance Data.ProtoLens.Field.HasField TypeBody "maybe'sum" (Prelude.Maybe Sum) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _TypeBody'body (\ x__ y__ -> x__ {_TypeBody'body = y__}))
        (Lens.Family2.Unchecked.lens
           (\ x__
              -> case x__ of
                   (Prelude.Just (TypeBody'Sum x__val)) -> Prelude.Just x__val
                   _otherwise -> Prelude.Nothing)
           (\ _ y__ -> Prelude.fmap TypeBody'Sum y__))
instance Data.ProtoLens.Field.HasField TypeBody "sum" Sum where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _TypeBody'body (\ x__ y__ -> x__ {_TypeBody'body = y__}))
        ((Prelude..)
           (Lens.Family2.Unchecked.lens
              (\ x__
                 -> case x__ of
                      (Prelude.Just (TypeBody'Sum x__val)) -> Prelude.Just x__val
                      _otherwise -> Prelude.Nothing)
              (\ _ y__ -> Prelude.fmap TypeBody'Sum y__))
           (Data.ProtoLens.maybeLens Data.ProtoLens.defMessage))
instance Data.ProtoLens.Message TypeBody where
  messageName _ = Data.Text.pack "lambdabuffers.source.TypeBody"
  packedMessageDescriptor _
    = "\n\
      \\bTypeBody\DC26\n\
      \\ACKopaque\CAN\SOH \SOH(\v2\FS.lambdabuffers.source.OpaqueH\NULR\ACKopaque\DC2-\n\
      \\ETXsum\CAN\STX \SOH(\v2\EM.lambdabuffers.source.SumH\NULR\ETXsum\DC2A\n\
      \\vsource_info\CAN\ETX \SOH(\v2 .lambdabuffers.source.SourceInfoR\n\
      \sourceInfoB\ACK\n\
      \\EOTbody"
  packedFileDescriptor _ = packedFileDescriptor
  fieldsByTag
    = let
        sourceInfo__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "source_info"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor SourceInfo)
              (Data.ProtoLens.OptionalField
                 (Data.ProtoLens.Field.field @"maybe'sourceInfo")) ::
              Data.ProtoLens.FieldDescriptor TypeBody
        opaque__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "opaque"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor Opaque)
              (Data.ProtoLens.OptionalField
                 (Data.ProtoLens.Field.field @"maybe'opaque")) ::
              Data.ProtoLens.FieldDescriptor TypeBody
        sum__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "sum"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor Sum)
              (Data.ProtoLens.OptionalField
                 (Data.ProtoLens.Field.field @"maybe'sum")) ::
              Data.ProtoLens.FieldDescriptor TypeBody
      in
        Data.Map.fromList
          [(Data.ProtoLens.Tag 3, sourceInfo__field_descriptor),
           (Data.ProtoLens.Tag 1, opaque__field_descriptor),
           (Data.ProtoLens.Tag 2, sum__field_descriptor)]
  unknownFields
    = Lens.Family2.Unchecked.lens
        _TypeBody'_unknownFields
        (\ x__ y__ -> x__ {_TypeBody'_unknownFields = y__})
  defMessage
    = TypeBody'_constructor
        {_TypeBody'sourceInfo = Prelude.Nothing,
         _TypeBody'body = Prelude.Nothing, _TypeBody'_unknownFields = []}
  parseMessage
    = let
        loop :: TypeBody -> Data.ProtoLens.Encoding.Bytes.Parser TypeBody
        loop x
          = do end <- Data.ProtoLens.Encoding.Bytes.atEnd
               if end then
                   do (let missing = []
                       in
                         if Prelude.null missing then
                             Prelude.return ()
                         else
                             Prelude.fail
                               ((Prelude.++)
                                  "Missing required fields: "
                                  (Prelude.show (missing :: [Prelude.String]))))
                      Prelude.return
                        (Lens.Family2.over
                           Data.ProtoLens.unknownFields (\ !t -> Prelude.reverse t) x)
               else
                   do tag <- Data.ProtoLens.Encoding.Bytes.getVarInt
                      case tag of
                        26
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                           Data.ProtoLens.Encoding.Bytes.isolate
                                             (Prelude.fromIntegral len) Data.ProtoLens.parseMessage)
                                       "source_info"
                                loop
                                  (Lens.Family2.set (Data.ProtoLens.Field.field @"sourceInfo") y x)
                        10
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                           Data.ProtoLens.Encoding.Bytes.isolate
                                             (Prelude.fromIntegral len) Data.ProtoLens.parseMessage)
                                       "opaque"
                                loop (Lens.Family2.set (Data.ProtoLens.Field.field @"opaque") y x)
                        18
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                           Data.ProtoLens.Encoding.Bytes.isolate
                                             (Prelude.fromIntegral len) Data.ProtoLens.parseMessage)
                                       "sum"
                                loop (Lens.Family2.set (Data.ProtoLens.Field.field @"sum") y x)
                        wire
                          -> do !y <- Data.ProtoLens.Encoding.Wire.parseTaggedValueFromWire
                                        wire
                                loop
                                  (Lens.Family2.over
                                     Data.ProtoLens.unknownFields (\ !t -> (:) y t) x)
      in
        (Data.ProtoLens.Encoding.Bytes.<?>)
          (do loop Data.ProtoLens.defMessage) "TypeBody"
  buildMessage
    = \ _x
        -> (Data.Monoid.<>)
             (case
                  Lens.Family2.view
                    (Data.ProtoLens.Field.field @"maybe'sourceInfo") _x
              of
                Prelude.Nothing -> Data.Monoid.mempty
                (Prelude.Just _v)
                  -> (Data.Monoid.<>)
                       (Data.ProtoLens.Encoding.Bytes.putVarInt 26)
                       ((Prelude..)
                          (\ bs
                             -> (Data.Monoid.<>)
                                  (Data.ProtoLens.Encoding.Bytes.putVarInt
                                     (Prelude.fromIntegral (Data.ByteString.length bs)))
                                  (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                          Data.ProtoLens.encodeMessage _v))
             ((Data.Monoid.<>)
                (case
                     Lens.Family2.view (Data.ProtoLens.Field.field @"maybe'body") _x
                 of
                   Prelude.Nothing -> Data.Monoid.mempty
                   (Prelude.Just (TypeBody'Opaque v))
                     -> (Data.Monoid.<>)
                          (Data.ProtoLens.Encoding.Bytes.putVarInt 10)
                          ((Prelude..)
                             (\ bs
                                -> (Data.Monoid.<>)
                                     (Data.ProtoLens.Encoding.Bytes.putVarInt
                                        (Prelude.fromIntegral (Data.ByteString.length bs)))
                                     (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                             Data.ProtoLens.encodeMessage v)
                   (Prelude.Just (TypeBody'Sum v))
                     -> (Data.Monoid.<>)
                          (Data.ProtoLens.Encoding.Bytes.putVarInt 18)
                          ((Prelude..)
                             (\ bs
                                -> (Data.Monoid.<>)
                                     (Data.ProtoLens.Encoding.Bytes.putVarInt
                                        (Prelude.fromIntegral (Data.ByteString.length bs)))
                                     (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                             Data.ProtoLens.encodeMessage v))
                (Data.ProtoLens.Encoding.Wire.buildFieldSet
                   (Lens.Family2.view Data.ProtoLens.unknownFields _x)))
instance Control.DeepSeq.NFData TypeBody where
  rnf
    = \ x__
        -> Control.DeepSeq.deepseq
             (_TypeBody'_unknownFields x__)
             (Control.DeepSeq.deepseq
                (_TypeBody'sourceInfo x__)
                (Control.DeepSeq.deepseq (_TypeBody'body x__) ()))
instance Control.DeepSeq.NFData TypeBody'Body where
  rnf (TypeBody'Opaque x__) = Control.DeepSeq.rnf x__
  rnf (TypeBody'Sum x__) = Control.DeepSeq.rnf x__
_TypeBody'Opaque ::
  Data.ProtoLens.Prism.Prism' TypeBody'Body Opaque
_TypeBody'Opaque
  = Data.ProtoLens.Prism.prism'
      TypeBody'Opaque
      (\ p__
         -> case p__ of
              (TypeBody'Opaque p__val) -> Prelude.Just p__val
              _otherwise -> Prelude.Nothing)
_TypeBody'Sum :: Data.ProtoLens.Prism.Prism' TypeBody'Body Sum
_TypeBody'Sum
  = Data.ProtoLens.Prism.prism'
      TypeBody'Sum
      (\ p__
         -> case p__ of
              (TypeBody'Sum p__val) -> Prelude.Just p__val
              _otherwise -> Prelude.Nothing)
{- | Fields :
     
         * 'Proto.Repls.LambdabuffersSource_Fields.kind' @:: Lens' TypeCon Kind@
         * 'Proto.Repls.LambdabuffersSource_Fields.maybe'kind' @:: Lens' TypeCon (Prelude.Maybe Kind)@
         * 'Proto.Repls.LambdabuffersSource_Fields.sourceInfo' @:: Lens' TypeCon SourceInfo@
         * 'Proto.Repls.LambdabuffersSource_Fields.maybe'sourceInfo' @:: Lens' TypeCon (Prelude.Maybe SourceInfo)@ -}
data TypeCon
  = TypeCon'_constructor {_TypeCon'kind :: !(Prelude.Maybe Kind),
                          _TypeCon'sourceInfo :: !(Prelude.Maybe SourceInfo),
                          _TypeCon'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving stock (Prelude.Eq, Prelude.Ord)
instance Prelude.Show TypeCon where
  showsPrec _ __x __s
    = Prelude.showChar
        '{'
        (Prelude.showString
           (Data.ProtoLens.showMessageShort __x) (Prelude.showChar '}' __s))
instance Data.ProtoLens.Field.HasField TypeCon "kind" Kind where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _TypeCon'kind (\ x__ y__ -> x__ {_TypeCon'kind = y__}))
        (Data.ProtoLens.maybeLens Data.ProtoLens.defMessage)
instance Data.ProtoLens.Field.HasField TypeCon "maybe'kind" (Prelude.Maybe Kind) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _TypeCon'kind (\ x__ y__ -> x__ {_TypeCon'kind = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField TypeCon "sourceInfo" SourceInfo where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _TypeCon'sourceInfo (\ x__ y__ -> x__ {_TypeCon'sourceInfo = y__}))
        (Data.ProtoLens.maybeLens Data.ProtoLens.defMessage)
instance Data.ProtoLens.Field.HasField TypeCon "maybe'sourceInfo" (Prelude.Maybe SourceInfo) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _TypeCon'sourceInfo (\ x__ y__ -> x__ {_TypeCon'sourceInfo = y__}))
        Prelude.id
instance Data.ProtoLens.Message TypeCon where
  messageName _ = Data.Text.pack "lambdabuffers.source.TypeCon"
  packedMessageDescriptor _
    = "\n\
      \\aTypeCon\DC2.\n\
      \\EOTkind\CAN\SOH \SOH(\v2\SUB.lambdabuffers.source.KindR\EOTkind\DC2A\n\
      \\vsource_info\CAN\STX \SOH(\v2 .lambdabuffers.source.SourceInfoR\n\
      \sourceInfo"
  packedFileDescriptor _ = packedFileDescriptor
  fieldsByTag
    = let
        kind__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "kind"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor Kind)
              (Data.ProtoLens.OptionalField
                 (Data.ProtoLens.Field.field @"maybe'kind")) ::
              Data.ProtoLens.FieldDescriptor TypeCon
        sourceInfo__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "source_info"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor SourceInfo)
              (Data.ProtoLens.OptionalField
                 (Data.ProtoLens.Field.field @"maybe'sourceInfo")) ::
              Data.ProtoLens.FieldDescriptor TypeCon
      in
        Data.Map.fromList
          [(Data.ProtoLens.Tag 1, kind__field_descriptor),
           (Data.ProtoLens.Tag 2, sourceInfo__field_descriptor)]
  unknownFields
    = Lens.Family2.Unchecked.lens
        _TypeCon'_unknownFields
        (\ x__ y__ -> x__ {_TypeCon'_unknownFields = y__})
  defMessage
    = TypeCon'_constructor
        {_TypeCon'kind = Prelude.Nothing,
         _TypeCon'sourceInfo = Prelude.Nothing,
         _TypeCon'_unknownFields = []}
  parseMessage
    = let
        loop :: TypeCon -> Data.ProtoLens.Encoding.Bytes.Parser TypeCon
        loop x
          = do end <- Data.ProtoLens.Encoding.Bytes.atEnd
               if end then
                   do (let missing = []
                       in
                         if Prelude.null missing then
                             Prelude.return ()
                         else
                             Prelude.fail
                               ((Prelude.++)
                                  "Missing required fields: "
                                  (Prelude.show (missing :: [Prelude.String]))))
                      Prelude.return
                        (Lens.Family2.over
                           Data.ProtoLens.unknownFields (\ !t -> Prelude.reverse t) x)
               else
                   do tag <- Data.ProtoLens.Encoding.Bytes.getVarInt
                      case tag of
                        10
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                           Data.ProtoLens.Encoding.Bytes.isolate
                                             (Prelude.fromIntegral len) Data.ProtoLens.parseMessage)
                                       "kind"
                                loop (Lens.Family2.set (Data.ProtoLens.Field.field @"kind") y x)
                        18
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                           Data.ProtoLens.Encoding.Bytes.isolate
                                             (Prelude.fromIntegral len) Data.ProtoLens.parseMessage)
                                       "source_info"
                                loop
                                  (Lens.Family2.set (Data.ProtoLens.Field.field @"sourceInfo") y x)
                        wire
                          -> do !y <- Data.ProtoLens.Encoding.Wire.parseTaggedValueFromWire
                                        wire
                                loop
                                  (Lens.Family2.over
                                     Data.ProtoLens.unknownFields (\ !t -> (:) y t) x)
      in
        (Data.ProtoLens.Encoding.Bytes.<?>)
          (do loop Data.ProtoLens.defMessage) "TypeCon"
  buildMessage
    = \ _x
        -> (Data.Monoid.<>)
             (case
                  Lens.Family2.view (Data.ProtoLens.Field.field @"maybe'kind") _x
              of
                Prelude.Nothing -> Data.Monoid.mempty
                (Prelude.Just _v)
                  -> (Data.Monoid.<>)
                       (Data.ProtoLens.Encoding.Bytes.putVarInt 10)
                       ((Prelude..)
                          (\ bs
                             -> (Data.Monoid.<>)
                                  (Data.ProtoLens.Encoding.Bytes.putVarInt
                                     (Prelude.fromIntegral (Data.ByteString.length bs)))
                                  (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                          Data.ProtoLens.encodeMessage _v))
             ((Data.Monoid.<>)
                (case
                     Lens.Family2.view
                       (Data.ProtoLens.Field.field @"maybe'sourceInfo") _x
                 of
                   Prelude.Nothing -> Data.Monoid.mempty
                   (Prelude.Just _v)
                     -> (Data.Monoid.<>)
                          (Data.ProtoLens.Encoding.Bytes.putVarInt 18)
                          ((Prelude..)
                             (\ bs
                                -> (Data.Monoid.<>)
                                     (Data.ProtoLens.Encoding.Bytes.putVarInt
                                        (Prelude.fromIntegral (Data.ByteString.length bs)))
                                     (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                             Data.ProtoLens.encodeMessage _v))
                (Data.ProtoLens.Encoding.Wire.buildFieldSet
                   (Lens.Family2.view Data.ProtoLens.unknownFields _x)))
instance Control.DeepSeq.NFData TypeCon where
  rnf
    = \ x__
        -> Control.DeepSeq.deepseq
             (_TypeCon'_unknownFields x__)
             (Control.DeepSeq.deepseq
                (_TypeCon'kind x__)
                (Control.DeepSeq.deepseq (_TypeCon'sourceInfo x__) ()))
{- | Fields :
     
         * 'Proto.Repls.LambdabuffersSource_Fields.name' @:: Lens' TypeConName Data.Text.Text@
         * 'Proto.Repls.LambdabuffersSource_Fields.sourceInfo' @:: Lens' TypeConName SourceInfo@
         * 'Proto.Repls.LambdabuffersSource_Fields.maybe'sourceInfo' @:: Lens' TypeConName (Prelude.Maybe SourceInfo)@ -}
data TypeConName
  = TypeConName'_constructor {_TypeConName'name :: !Data.Text.Text,
                              _TypeConName'sourceInfo :: !(Prelude.Maybe SourceInfo),
                              _TypeConName'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving stock (Prelude.Eq, Prelude.Ord)
instance Prelude.Show TypeConName where
  showsPrec _ __x __s
    = Prelude.showChar
        '{'
        (Prelude.showString
           (Data.ProtoLens.showMessageShort __x) (Prelude.showChar '}' __s))
instance Data.ProtoLens.Field.HasField TypeConName "name" Data.Text.Text where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _TypeConName'name (\ x__ y__ -> x__ {_TypeConName'name = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField TypeConName "sourceInfo" SourceInfo where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _TypeConName'sourceInfo
           (\ x__ y__ -> x__ {_TypeConName'sourceInfo = y__}))
        (Data.ProtoLens.maybeLens Data.ProtoLens.defMessage)
instance Data.ProtoLens.Field.HasField TypeConName "maybe'sourceInfo" (Prelude.Maybe SourceInfo) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _TypeConName'sourceInfo
           (\ x__ y__ -> x__ {_TypeConName'sourceInfo = y__}))
        Prelude.id
instance Data.ProtoLens.Message TypeConName where
  messageName _ = Data.Text.pack "lambdabuffers.source.TypeConName"
  packedMessageDescriptor _
    = "\n\
      \\vTypeConName\DC2\DC2\n\
      \\EOTname\CAN\SOH \SOH(\tR\EOTname\DC2A\n\
      \\vsource_info\CAN\STX \SOH(\v2 .lambdabuffers.source.SourceInfoR\n\
      \sourceInfo"
  packedFileDescriptor _ = packedFileDescriptor
  fieldsByTag
    = let
        name__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "name"
              (Data.ProtoLens.ScalarField Data.ProtoLens.StringField ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Text.Text)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional (Data.ProtoLens.Field.field @"name")) ::
              Data.ProtoLens.FieldDescriptor TypeConName
        sourceInfo__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "source_info"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor SourceInfo)
              (Data.ProtoLens.OptionalField
                 (Data.ProtoLens.Field.field @"maybe'sourceInfo")) ::
              Data.ProtoLens.FieldDescriptor TypeConName
      in
        Data.Map.fromList
          [(Data.ProtoLens.Tag 1, name__field_descriptor),
           (Data.ProtoLens.Tag 2, sourceInfo__field_descriptor)]
  unknownFields
    = Lens.Family2.Unchecked.lens
        _TypeConName'_unknownFields
        (\ x__ y__ -> x__ {_TypeConName'_unknownFields = y__})
  defMessage
    = TypeConName'_constructor
        {_TypeConName'name = Data.ProtoLens.fieldDefault,
         _TypeConName'sourceInfo = Prelude.Nothing,
         _TypeConName'_unknownFields = []}
  parseMessage
    = let
        loop ::
          TypeConName -> Data.ProtoLens.Encoding.Bytes.Parser TypeConName
        loop x
          = do end <- Data.ProtoLens.Encoding.Bytes.atEnd
               if end then
                   do (let missing = []
                       in
                         if Prelude.null missing then
                             Prelude.return ()
                         else
                             Prelude.fail
                               ((Prelude.++)
                                  "Missing required fields: "
                                  (Prelude.show (missing :: [Prelude.String]))))
                      Prelude.return
                        (Lens.Family2.over
                           Data.ProtoLens.unknownFields (\ !t -> Prelude.reverse t) x)
               else
                   do tag <- Data.ProtoLens.Encoding.Bytes.getVarInt
                      case tag of
                        10
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do value <- do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                                       Data.ProtoLens.Encoding.Bytes.getBytes
                                                         (Prelude.fromIntegral len)
                                           Data.ProtoLens.Encoding.Bytes.runEither
                                             (case Data.Text.Encoding.decodeUtf8' value of
                                                (Prelude.Left err)
                                                  -> Prelude.Left (Prelude.show err)
                                                (Prelude.Right r) -> Prelude.Right r))
                                       "name"
                                loop (Lens.Family2.set (Data.ProtoLens.Field.field @"name") y x)
                        18
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                           Data.ProtoLens.Encoding.Bytes.isolate
                                             (Prelude.fromIntegral len) Data.ProtoLens.parseMessage)
                                       "source_info"
                                loop
                                  (Lens.Family2.set (Data.ProtoLens.Field.field @"sourceInfo") y x)
                        wire
                          -> do !y <- Data.ProtoLens.Encoding.Wire.parseTaggedValueFromWire
                                        wire
                                loop
                                  (Lens.Family2.over
                                     Data.ProtoLens.unknownFields (\ !t -> (:) y t) x)
      in
        (Data.ProtoLens.Encoding.Bytes.<?>)
          (do loop Data.ProtoLens.defMessage) "TypeConName"
  buildMessage
    = \ _x
        -> (Data.Monoid.<>)
             (let _v = Lens.Family2.view (Data.ProtoLens.Field.field @"name") _x
              in
                if (Prelude.==) _v Data.ProtoLens.fieldDefault then
                    Data.Monoid.mempty
                else
                    (Data.Monoid.<>)
                      (Data.ProtoLens.Encoding.Bytes.putVarInt 10)
                      ((Prelude..)
                         (\ bs
                            -> (Data.Monoid.<>)
                                 (Data.ProtoLens.Encoding.Bytes.putVarInt
                                    (Prelude.fromIntegral (Data.ByteString.length bs)))
                                 (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                         Data.Text.Encoding.encodeUtf8 _v))
             ((Data.Monoid.<>)
                (case
                     Lens.Family2.view
                       (Data.ProtoLens.Field.field @"maybe'sourceInfo") _x
                 of
                   Prelude.Nothing -> Data.Monoid.mempty
                   (Prelude.Just _v)
                     -> (Data.Monoid.<>)
                          (Data.ProtoLens.Encoding.Bytes.putVarInt 18)
                          ((Prelude..)
                             (\ bs
                                -> (Data.Monoid.<>)
                                     (Data.ProtoLens.Encoding.Bytes.putVarInt
                                        (Prelude.fromIntegral (Data.ByteString.length bs)))
                                     (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                             Data.ProtoLens.encodeMessage _v))
                (Data.ProtoLens.Encoding.Wire.buildFieldSet
                   (Lens.Family2.view Data.ProtoLens.unknownFields _x)))
instance Control.DeepSeq.NFData TypeConName where
  rnf
    = \ x__
        -> Control.DeepSeq.deepseq
             (_TypeConName'_unknownFields x__)
             (Control.DeepSeq.deepseq
                (_TypeConName'name x__)
                (Control.DeepSeq.deepseq (_TypeConName'sourceInfo x__) ()))
{- | Fields :
     
         * 'Proto.Repls.LambdabuffersSource_Fields.typeName' @:: Lens' TypeDefinition TypeConName@
         * 'Proto.Repls.LambdabuffersSource_Fields.maybe'typeName' @:: Lens' TypeDefinition (Prelude.Maybe TypeConName)@
         * 'Proto.Repls.LambdabuffersSource_Fields.typeKind' @:: Lens' TypeDefinition Kind@
         * 'Proto.Repls.LambdabuffersSource_Fields.maybe'typeKind' @:: Lens' TypeDefinition (Prelude.Maybe Kind)@
         * 'Proto.Repls.LambdabuffersSource_Fields.typeBody' @:: Lens' TypeDefinition TypeBody@
         * 'Proto.Repls.LambdabuffersSource_Fields.maybe'typeBody' @:: Lens' TypeDefinition (Prelude.Maybe TypeBody)@
         * 'Proto.Repls.LambdabuffersSource_Fields.sourceInfo' @:: Lens' TypeDefinition SourceInfo@
         * 'Proto.Repls.LambdabuffersSource_Fields.maybe'sourceInfo' @:: Lens' TypeDefinition (Prelude.Maybe SourceInfo)@ -}
data TypeDefinition
  = TypeDefinition'_constructor {_TypeDefinition'typeName :: !(Prelude.Maybe TypeConName),
                                 _TypeDefinition'typeKind :: !(Prelude.Maybe Kind),
                                 _TypeDefinition'typeBody :: !(Prelude.Maybe TypeBody),
                                 _TypeDefinition'sourceInfo :: !(Prelude.Maybe SourceInfo),
                                 _TypeDefinition'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving stock (Prelude.Eq, Prelude.Ord)
instance Prelude.Show TypeDefinition where
  showsPrec _ __x __s
    = Prelude.showChar
        '{'
        (Prelude.showString
           (Data.ProtoLens.showMessageShort __x) (Prelude.showChar '}' __s))
instance Data.ProtoLens.Field.HasField TypeDefinition "typeName" TypeConName where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _TypeDefinition'typeName
           (\ x__ y__ -> x__ {_TypeDefinition'typeName = y__}))
        (Data.ProtoLens.maybeLens Data.ProtoLens.defMessage)
instance Data.ProtoLens.Field.HasField TypeDefinition "maybe'typeName" (Prelude.Maybe TypeConName) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _TypeDefinition'typeName
           (\ x__ y__ -> x__ {_TypeDefinition'typeName = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField TypeDefinition "typeKind" Kind where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _TypeDefinition'typeKind
           (\ x__ y__ -> x__ {_TypeDefinition'typeKind = y__}))
        (Data.ProtoLens.maybeLens Data.ProtoLens.defMessage)
instance Data.ProtoLens.Field.HasField TypeDefinition "maybe'typeKind" (Prelude.Maybe Kind) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _TypeDefinition'typeKind
           (\ x__ y__ -> x__ {_TypeDefinition'typeKind = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField TypeDefinition "typeBody" TypeBody where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _TypeDefinition'typeBody
           (\ x__ y__ -> x__ {_TypeDefinition'typeBody = y__}))
        (Data.ProtoLens.maybeLens Data.ProtoLens.defMessage)
instance Data.ProtoLens.Field.HasField TypeDefinition "maybe'typeBody" (Prelude.Maybe TypeBody) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _TypeDefinition'typeBody
           (\ x__ y__ -> x__ {_TypeDefinition'typeBody = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField TypeDefinition "sourceInfo" SourceInfo where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _TypeDefinition'sourceInfo
           (\ x__ y__ -> x__ {_TypeDefinition'sourceInfo = y__}))
        (Data.ProtoLens.maybeLens Data.ProtoLens.defMessage)
instance Data.ProtoLens.Field.HasField TypeDefinition "maybe'sourceInfo" (Prelude.Maybe SourceInfo) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _TypeDefinition'sourceInfo
           (\ x__ y__ -> x__ {_TypeDefinition'sourceInfo = y__}))
        Prelude.id
instance Data.ProtoLens.Message TypeDefinition where
  messageName _
    = Data.Text.pack "lambdabuffers.source.TypeDefinition"
  packedMessageDescriptor _
    = "\n\
      \\SOTypeDefinition\DC2>\n\
      \\ttype_name\CAN\SOH \SOH(\v2!.lambdabuffers.source.TypeConNameR\btypeName\DC27\n\
      \\ttype_kind\CAN\STX \SOH(\v2\SUB.lambdabuffers.source.KindR\btypeKind\DC2;\n\
      \\ttype_body\CAN\ETX \SOH(\v2\RS.lambdabuffers.source.TypeBodyR\btypeBody\DC2A\n\
      \\vsource_info\CAN\EOT \SOH(\v2 .lambdabuffers.source.SourceInfoR\n\
      \sourceInfo"
  packedFileDescriptor _ = packedFileDescriptor
  fieldsByTag
    = let
        typeName__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "type_name"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor TypeConName)
              (Data.ProtoLens.OptionalField
                 (Data.ProtoLens.Field.field @"maybe'typeName")) ::
              Data.ProtoLens.FieldDescriptor TypeDefinition
        typeKind__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "type_kind"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor Kind)
              (Data.ProtoLens.OptionalField
                 (Data.ProtoLens.Field.field @"maybe'typeKind")) ::
              Data.ProtoLens.FieldDescriptor TypeDefinition
        typeBody__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "type_body"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor TypeBody)
              (Data.ProtoLens.OptionalField
                 (Data.ProtoLens.Field.field @"maybe'typeBody")) ::
              Data.ProtoLens.FieldDescriptor TypeDefinition
        sourceInfo__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "source_info"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor SourceInfo)
              (Data.ProtoLens.OptionalField
                 (Data.ProtoLens.Field.field @"maybe'sourceInfo")) ::
              Data.ProtoLens.FieldDescriptor TypeDefinition
      in
        Data.Map.fromList
          [(Data.ProtoLens.Tag 1, typeName__field_descriptor),
           (Data.ProtoLens.Tag 2, typeKind__field_descriptor),
           (Data.ProtoLens.Tag 3, typeBody__field_descriptor),
           (Data.ProtoLens.Tag 4, sourceInfo__field_descriptor)]
  unknownFields
    = Lens.Family2.Unchecked.lens
        _TypeDefinition'_unknownFields
        (\ x__ y__ -> x__ {_TypeDefinition'_unknownFields = y__})
  defMessage
    = TypeDefinition'_constructor
        {_TypeDefinition'typeName = Prelude.Nothing,
         _TypeDefinition'typeKind = Prelude.Nothing,
         _TypeDefinition'typeBody = Prelude.Nothing,
         _TypeDefinition'sourceInfo = Prelude.Nothing,
         _TypeDefinition'_unknownFields = []}
  parseMessage
    = let
        loop ::
          TypeDefinition
          -> Data.ProtoLens.Encoding.Bytes.Parser TypeDefinition
        loop x
          = do end <- Data.ProtoLens.Encoding.Bytes.atEnd
               if end then
                   do (let missing = []
                       in
                         if Prelude.null missing then
                             Prelude.return ()
                         else
                             Prelude.fail
                               ((Prelude.++)
                                  "Missing required fields: "
                                  (Prelude.show (missing :: [Prelude.String]))))
                      Prelude.return
                        (Lens.Family2.over
                           Data.ProtoLens.unknownFields (\ !t -> Prelude.reverse t) x)
               else
                   do tag <- Data.ProtoLens.Encoding.Bytes.getVarInt
                      case tag of
                        10
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                           Data.ProtoLens.Encoding.Bytes.isolate
                                             (Prelude.fromIntegral len) Data.ProtoLens.parseMessage)
                                       "type_name"
                                loop
                                  (Lens.Family2.set (Data.ProtoLens.Field.field @"typeName") y x)
                        18
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                           Data.ProtoLens.Encoding.Bytes.isolate
                                             (Prelude.fromIntegral len) Data.ProtoLens.parseMessage)
                                       "type_kind"
                                loop
                                  (Lens.Family2.set (Data.ProtoLens.Field.field @"typeKind") y x)
                        26
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                           Data.ProtoLens.Encoding.Bytes.isolate
                                             (Prelude.fromIntegral len) Data.ProtoLens.parseMessage)
                                       "type_body"
                                loop
                                  (Lens.Family2.set (Data.ProtoLens.Field.field @"typeBody") y x)
                        34
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                           Data.ProtoLens.Encoding.Bytes.isolate
                                             (Prelude.fromIntegral len) Data.ProtoLens.parseMessage)
                                       "source_info"
                                loop
                                  (Lens.Family2.set (Data.ProtoLens.Field.field @"sourceInfo") y x)
                        wire
                          -> do !y <- Data.ProtoLens.Encoding.Wire.parseTaggedValueFromWire
                                        wire
                                loop
                                  (Lens.Family2.over
                                     Data.ProtoLens.unknownFields (\ !t -> (:) y t) x)
      in
        (Data.ProtoLens.Encoding.Bytes.<?>)
          (do loop Data.ProtoLens.defMessage) "TypeDefinition"
  buildMessage
    = \ _x
        -> (Data.Monoid.<>)
             (case
                  Lens.Family2.view (Data.ProtoLens.Field.field @"maybe'typeName") _x
              of
                Prelude.Nothing -> Data.Monoid.mempty
                (Prelude.Just _v)
                  -> (Data.Monoid.<>)
                       (Data.ProtoLens.Encoding.Bytes.putVarInt 10)
                       ((Prelude..)
                          (\ bs
                             -> (Data.Monoid.<>)
                                  (Data.ProtoLens.Encoding.Bytes.putVarInt
                                     (Prelude.fromIntegral (Data.ByteString.length bs)))
                                  (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                          Data.ProtoLens.encodeMessage _v))
             ((Data.Monoid.<>)
                (case
                     Lens.Family2.view (Data.ProtoLens.Field.field @"maybe'typeKind") _x
                 of
                   Prelude.Nothing -> Data.Monoid.mempty
                   (Prelude.Just _v)
                     -> (Data.Monoid.<>)
                          (Data.ProtoLens.Encoding.Bytes.putVarInt 18)
                          ((Prelude..)
                             (\ bs
                                -> (Data.Monoid.<>)
                                     (Data.ProtoLens.Encoding.Bytes.putVarInt
                                        (Prelude.fromIntegral (Data.ByteString.length bs)))
                                     (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                             Data.ProtoLens.encodeMessage _v))
                ((Data.Monoid.<>)
                   (case
                        Lens.Family2.view (Data.ProtoLens.Field.field @"maybe'typeBody") _x
                    of
                      Prelude.Nothing -> Data.Monoid.mempty
                      (Prelude.Just _v)
                        -> (Data.Monoid.<>)
                             (Data.ProtoLens.Encoding.Bytes.putVarInt 26)
                             ((Prelude..)
                                (\ bs
                                   -> (Data.Monoid.<>)
                                        (Data.ProtoLens.Encoding.Bytes.putVarInt
                                           (Prelude.fromIntegral (Data.ByteString.length bs)))
                                        (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                                Data.ProtoLens.encodeMessage _v))
                   ((Data.Monoid.<>)
                      (case
                           Lens.Family2.view
                             (Data.ProtoLens.Field.field @"maybe'sourceInfo") _x
                       of
                         Prelude.Nothing -> Data.Monoid.mempty
                         (Prelude.Just _v)
                           -> (Data.Monoid.<>)
                                (Data.ProtoLens.Encoding.Bytes.putVarInt 34)
                                ((Prelude..)
                                   (\ bs
                                      -> (Data.Monoid.<>)
                                           (Data.ProtoLens.Encoding.Bytes.putVarInt
                                              (Prelude.fromIntegral (Data.ByteString.length bs)))
                                           (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                                   Data.ProtoLens.encodeMessage _v))
                      (Data.ProtoLens.Encoding.Wire.buildFieldSet
                         (Lens.Family2.view Data.ProtoLens.unknownFields _x)))))
instance Control.DeepSeq.NFData TypeDefinition where
  rnf
    = \ x__
        -> Control.DeepSeq.deepseq
             (_TypeDefinition'_unknownFields x__)
             (Control.DeepSeq.deepseq
                (_TypeDefinition'typeName x__)
                (Control.DeepSeq.deepseq
                   (_TypeDefinition'typeKind x__)
                   (Control.DeepSeq.deepseq
                      (_TypeDefinition'typeBody x__)
                      (Control.DeepSeq.deepseq (_TypeDefinition'sourceInfo x__) ()))))
{- | Fields :
     
         * 'Proto.Repls.LambdabuffersSource_Fields.sourceInfo' @:: Lens' TypeRef SourceInfo@
         * 'Proto.Repls.LambdabuffersSource_Fields.maybe'sourceInfo' @:: Lens' TypeRef (Prelude.Maybe SourceInfo)@
         * 'Proto.Repls.LambdabuffersSource_Fields.maybe'typeRef' @:: Lens' TypeRef (Prelude.Maybe TypeRef'TypeRef)@
         * 'Proto.Repls.LambdabuffersSource_Fields.maybe'localTypeRef' @:: Lens' TypeRef (Prelude.Maybe LocalTypeRef)@
         * 'Proto.Repls.LambdabuffersSource_Fields.localTypeRef' @:: Lens' TypeRef LocalTypeRef@
         * 'Proto.Repls.LambdabuffersSource_Fields.maybe'foreignTypeRef' @:: Lens' TypeRef (Prelude.Maybe ForeignTypeRef)@
         * 'Proto.Repls.LambdabuffersSource_Fields.foreignTypeRef' @:: Lens' TypeRef ForeignTypeRef@ -}
data TypeRef
  = TypeRef'_constructor {_TypeRef'sourceInfo :: !(Prelude.Maybe SourceInfo),
                          _TypeRef'typeRef :: !(Prelude.Maybe TypeRef'TypeRef),
                          _TypeRef'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving stock (Prelude.Eq, Prelude.Ord)
instance Prelude.Show TypeRef where
  showsPrec _ __x __s
    = Prelude.showChar
        '{'
        (Prelude.showString
           (Data.ProtoLens.showMessageShort __x) (Prelude.showChar '}' __s))
data TypeRef'TypeRef
  = TypeRef'LocalTypeRef !LocalTypeRef |
    TypeRef'ForeignTypeRef !ForeignTypeRef
  deriving stock (Prelude.Show, Prelude.Eq, Prelude.Ord)
instance Data.ProtoLens.Field.HasField TypeRef "sourceInfo" SourceInfo where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _TypeRef'sourceInfo (\ x__ y__ -> x__ {_TypeRef'sourceInfo = y__}))
        (Data.ProtoLens.maybeLens Data.ProtoLens.defMessage)
instance Data.ProtoLens.Field.HasField TypeRef "maybe'sourceInfo" (Prelude.Maybe SourceInfo) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _TypeRef'sourceInfo (\ x__ y__ -> x__ {_TypeRef'sourceInfo = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField TypeRef "maybe'typeRef" (Prelude.Maybe TypeRef'TypeRef) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _TypeRef'typeRef (\ x__ y__ -> x__ {_TypeRef'typeRef = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField TypeRef "maybe'localTypeRef" (Prelude.Maybe LocalTypeRef) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _TypeRef'typeRef (\ x__ y__ -> x__ {_TypeRef'typeRef = y__}))
        (Lens.Family2.Unchecked.lens
           (\ x__
              -> case x__ of
                   (Prelude.Just (TypeRef'LocalTypeRef x__val)) -> Prelude.Just x__val
                   _otherwise -> Prelude.Nothing)
           (\ _ y__ -> Prelude.fmap TypeRef'LocalTypeRef y__))
instance Data.ProtoLens.Field.HasField TypeRef "localTypeRef" LocalTypeRef where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _TypeRef'typeRef (\ x__ y__ -> x__ {_TypeRef'typeRef = y__}))
        ((Prelude..)
           (Lens.Family2.Unchecked.lens
              (\ x__
                 -> case x__ of
                      (Prelude.Just (TypeRef'LocalTypeRef x__val)) -> Prelude.Just x__val
                      _otherwise -> Prelude.Nothing)
              (\ _ y__ -> Prelude.fmap TypeRef'LocalTypeRef y__))
           (Data.ProtoLens.maybeLens Data.ProtoLens.defMessage))
instance Data.ProtoLens.Field.HasField TypeRef "maybe'foreignTypeRef" (Prelude.Maybe ForeignTypeRef) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _TypeRef'typeRef (\ x__ y__ -> x__ {_TypeRef'typeRef = y__}))
        (Lens.Family2.Unchecked.lens
           (\ x__
              -> case x__ of
                   (Prelude.Just (TypeRef'ForeignTypeRef x__val))
                     -> Prelude.Just x__val
                   _otherwise -> Prelude.Nothing)
           (\ _ y__ -> Prelude.fmap TypeRef'ForeignTypeRef y__))
instance Data.ProtoLens.Field.HasField TypeRef "foreignTypeRef" ForeignTypeRef where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _TypeRef'typeRef (\ x__ y__ -> x__ {_TypeRef'typeRef = y__}))
        ((Prelude..)
           (Lens.Family2.Unchecked.lens
              (\ x__
                 -> case x__ of
                      (Prelude.Just (TypeRef'ForeignTypeRef x__val))
                        -> Prelude.Just x__val
                      _otherwise -> Prelude.Nothing)
              (\ _ y__ -> Prelude.fmap TypeRef'ForeignTypeRef y__))
           (Data.ProtoLens.maybeLens Data.ProtoLens.defMessage))
instance Data.ProtoLens.Message TypeRef where
  messageName _ = Data.Text.pack "lambdabuffers.source.TypeRef"
  packedMessageDescriptor _
    = "\n\
      \\aTypeRef\DC2J\n\
      \\SOlocal_type_ref\CAN\SOH \SOH(\v2\".lambdabuffers.source.LocalTypeRefH\NULR\flocalTypeRef\DC2P\n\
      \\DLEforeign_type_ref\CAN\STX \SOH(\v2$.lambdabuffers.source.ForeignTypeRefH\NULR\SOforeignTypeRef\DC2A\n\
      \\vsource_info\CAN\ETX \SOH(\v2 .lambdabuffers.source.SourceInfoR\n\
      \sourceInfoB\n\
      \\n\
      \\btype_ref"
  packedFileDescriptor _ = packedFileDescriptor
  fieldsByTag
    = let
        sourceInfo__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "source_info"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor SourceInfo)
              (Data.ProtoLens.OptionalField
                 (Data.ProtoLens.Field.field @"maybe'sourceInfo")) ::
              Data.ProtoLens.FieldDescriptor TypeRef
        localTypeRef__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "local_type_ref"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor LocalTypeRef)
              (Data.ProtoLens.OptionalField
                 (Data.ProtoLens.Field.field @"maybe'localTypeRef")) ::
              Data.ProtoLens.FieldDescriptor TypeRef
        foreignTypeRef__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "foreign_type_ref"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor ForeignTypeRef)
              (Data.ProtoLens.OptionalField
                 (Data.ProtoLens.Field.field @"maybe'foreignTypeRef")) ::
              Data.ProtoLens.FieldDescriptor TypeRef
      in
        Data.Map.fromList
          [(Data.ProtoLens.Tag 3, sourceInfo__field_descriptor),
           (Data.ProtoLens.Tag 1, localTypeRef__field_descriptor),
           (Data.ProtoLens.Tag 2, foreignTypeRef__field_descriptor)]
  unknownFields
    = Lens.Family2.Unchecked.lens
        _TypeRef'_unknownFields
        (\ x__ y__ -> x__ {_TypeRef'_unknownFields = y__})
  defMessage
    = TypeRef'_constructor
        {_TypeRef'sourceInfo = Prelude.Nothing,
         _TypeRef'typeRef = Prelude.Nothing, _TypeRef'_unknownFields = []}
  parseMessage
    = let
        loop :: TypeRef -> Data.ProtoLens.Encoding.Bytes.Parser TypeRef
        loop x
          = do end <- Data.ProtoLens.Encoding.Bytes.atEnd
               if end then
                   do (let missing = []
                       in
                         if Prelude.null missing then
                             Prelude.return ()
                         else
                             Prelude.fail
                               ((Prelude.++)
                                  "Missing required fields: "
                                  (Prelude.show (missing :: [Prelude.String]))))
                      Prelude.return
                        (Lens.Family2.over
                           Data.ProtoLens.unknownFields (\ !t -> Prelude.reverse t) x)
               else
                   do tag <- Data.ProtoLens.Encoding.Bytes.getVarInt
                      case tag of
                        26
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                           Data.ProtoLens.Encoding.Bytes.isolate
                                             (Prelude.fromIntegral len) Data.ProtoLens.parseMessage)
                                       "source_info"
                                loop
                                  (Lens.Family2.set (Data.ProtoLens.Field.field @"sourceInfo") y x)
                        10
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                           Data.ProtoLens.Encoding.Bytes.isolate
                                             (Prelude.fromIntegral len) Data.ProtoLens.parseMessage)
                                       "local_type_ref"
                                loop
                                  (Lens.Family2.set
                                     (Data.ProtoLens.Field.field @"localTypeRef") y x)
                        18
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                           Data.ProtoLens.Encoding.Bytes.isolate
                                             (Prelude.fromIntegral len) Data.ProtoLens.parseMessage)
                                       "foreign_type_ref"
                                loop
                                  (Lens.Family2.set
                                     (Data.ProtoLens.Field.field @"foreignTypeRef") y x)
                        wire
                          -> do !y <- Data.ProtoLens.Encoding.Wire.parseTaggedValueFromWire
                                        wire
                                loop
                                  (Lens.Family2.over
                                     Data.ProtoLens.unknownFields (\ !t -> (:) y t) x)
      in
        (Data.ProtoLens.Encoding.Bytes.<?>)
          (do loop Data.ProtoLens.defMessage) "TypeRef"
  buildMessage
    = \ _x
        -> (Data.Monoid.<>)
             (case
                  Lens.Family2.view
                    (Data.ProtoLens.Field.field @"maybe'sourceInfo") _x
              of
                Prelude.Nothing -> Data.Monoid.mempty
                (Prelude.Just _v)
                  -> (Data.Monoid.<>)
                       (Data.ProtoLens.Encoding.Bytes.putVarInt 26)
                       ((Prelude..)
                          (\ bs
                             -> (Data.Monoid.<>)
                                  (Data.ProtoLens.Encoding.Bytes.putVarInt
                                     (Prelude.fromIntegral (Data.ByteString.length bs)))
                                  (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                          Data.ProtoLens.encodeMessage _v))
             ((Data.Monoid.<>)
                (case
                     Lens.Family2.view (Data.ProtoLens.Field.field @"maybe'typeRef") _x
                 of
                   Prelude.Nothing -> Data.Monoid.mempty
                   (Prelude.Just (TypeRef'LocalTypeRef v))
                     -> (Data.Monoid.<>)
                          (Data.ProtoLens.Encoding.Bytes.putVarInt 10)
                          ((Prelude..)
                             (\ bs
                                -> (Data.Monoid.<>)
                                     (Data.ProtoLens.Encoding.Bytes.putVarInt
                                        (Prelude.fromIntegral (Data.ByteString.length bs)))
                                     (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                             Data.ProtoLens.encodeMessage v)
                   (Prelude.Just (TypeRef'ForeignTypeRef v))
                     -> (Data.Monoid.<>)
                          (Data.ProtoLens.Encoding.Bytes.putVarInt 18)
                          ((Prelude..)
                             (\ bs
                                -> (Data.Monoid.<>)
                                     (Data.ProtoLens.Encoding.Bytes.putVarInt
                                        (Prelude.fromIntegral (Data.ByteString.length bs)))
                                     (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                             Data.ProtoLens.encodeMessage v))
                (Data.ProtoLens.Encoding.Wire.buildFieldSet
                   (Lens.Family2.view Data.ProtoLens.unknownFields _x)))
instance Control.DeepSeq.NFData TypeRef where
  rnf
    = \ x__
        -> Control.DeepSeq.deepseq
             (_TypeRef'_unknownFields x__)
             (Control.DeepSeq.deepseq
                (_TypeRef'sourceInfo x__)
                (Control.DeepSeq.deepseq (_TypeRef'typeRef x__) ()))
instance Control.DeepSeq.NFData TypeRef'TypeRef where
  rnf (TypeRef'LocalTypeRef x__) = Control.DeepSeq.rnf x__
  rnf (TypeRef'ForeignTypeRef x__) = Control.DeepSeq.rnf x__
_TypeRef'LocalTypeRef ::
  Data.ProtoLens.Prism.Prism' TypeRef'TypeRef LocalTypeRef
_TypeRef'LocalTypeRef
  = Data.ProtoLens.Prism.prism'
      TypeRef'LocalTypeRef
      (\ p__
         -> case p__ of
              (TypeRef'LocalTypeRef p__val) -> Prelude.Just p__val
              _otherwise -> Prelude.Nothing)
_TypeRef'ForeignTypeRef ::
  Data.ProtoLens.Prism.Prism' TypeRef'TypeRef ForeignTypeRef
_TypeRef'ForeignTypeRef
  = Data.ProtoLens.Prism.prism'
      TypeRef'ForeignTypeRef
      (\ p__
         -> case p__ of
              (TypeRef'ForeignTypeRef p__val) -> Prelude.Just p__val
              _otherwise -> Prelude.Nothing)
{- | Fields :
     
         * 'Proto.Repls.LambdabuffersSource_Fields.varName' @:: Lens' TypeVar VarName@
         * 'Proto.Repls.LambdabuffersSource_Fields.maybe'varName' @:: Lens' TypeVar (Prelude.Maybe VarName)@
         * 'Proto.Repls.LambdabuffersSource_Fields.sourceInfo' @:: Lens' TypeVar SourceInfo@
         * 'Proto.Repls.LambdabuffersSource_Fields.maybe'sourceInfo' @:: Lens' TypeVar (Prelude.Maybe SourceInfo)@ -}
data TypeVar
  = TypeVar'_constructor {_TypeVar'varName :: !(Prelude.Maybe VarName),
                          _TypeVar'sourceInfo :: !(Prelude.Maybe SourceInfo),
                          _TypeVar'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving stock (Prelude.Eq, Prelude.Ord)
instance Prelude.Show TypeVar where
  showsPrec _ __x __s
    = Prelude.showChar
        '{'
        (Prelude.showString
           (Data.ProtoLens.showMessageShort __x) (Prelude.showChar '}' __s))
instance Data.ProtoLens.Field.HasField TypeVar "varName" VarName where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _TypeVar'varName (\ x__ y__ -> x__ {_TypeVar'varName = y__}))
        (Data.ProtoLens.maybeLens Data.ProtoLens.defMessage)
instance Data.ProtoLens.Field.HasField TypeVar "maybe'varName" (Prelude.Maybe VarName) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _TypeVar'varName (\ x__ y__ -> x__ {_TypeVar'varName = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField TypeVar "sourceInfo" SourceInfo where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _TypeVar'sourceInfo (\ x__ y__ -> x__ {_TypeVar'sourceInfo = y__}))
        (Data.ProtoLens.maybeLens Data.ProtoLens.defMessage)
instance Data.ProtoLens.Field.HasField TypeVar "maybe'sourceInfo" (Prelude.Maybe SourceInfo) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _TypeVar'sourceInfo (\ x__ y__ -> x__ {_TypeVar'sourceInfo = y__}))
        Prelude.id
instance Data.ProtoLens.Message TypeVar where
  messageName _ = Data.Text.pack "lambdabuffers.source.TypeVar"
  packedMessageDescriptor _
    = "\n\
      \\aTypeVar\DC28\n\
      \\bvar_name\CAN\SOH \SOH(\v2\GS.lambdabuffers.source.VarNameR\avarName\DC2A\n\
      \\vsource_info\CAN\STX \SOH(\v2 .lambdabuffers.source.SourceInfoR\n\
      \sourceInfo"
  packedFileDescriptor _ = packedFileDescriptor
  fieldsByTag
    = let
        varName__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "var_name"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor VarName)
              (Data.ProtoLens.OptionalField
                 (Data.ProtoLens.Field.field @"maybe'varName")) ::
              Data.ProtoLens.FieldDescriptor TypeVar
        sourceInfo__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "source_info"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor SourceInfo)
              (Data.ProtoLens.OptionalField
                 (Data.ProtoLens.Field.field @"maybe'sourceInfo")) ::
              Data.ProtoLens.FieldDescriptor TypeVar
      in
        Data.Map.fromList
          [(Data.ProtoLens.Tag 1, varName__field_descriptor),
           (Data.ProtoLens.Tag 2, sourceInfo__field_descriptor)]
  unknownFields
    = Lens.Family2.Unchecked.lens
        _TypeVar'_unknownFields
        (\ x__ y__ -> x__ {_TypeVar'_unknownFields = y__})
  defMessage
    = TypeVar'_constructor
        {_TypeVar'varName = Prelude.Nothing,
         _TypeVar'sourceInfo = Prelude.Nothing,
         _TypeVar'_unknownFields = []}
  parseMessage
    = let
        loop :: TypeVar -> Data.ProtoLens.Encoding.Bytes.Parser TypeVar
        loop x
          = do end <- Data.ProtoLens.Encoding.Bytes.atEnd
               if end then
                   do (let missing = []
                       in
                         if Prelude.null missing then
                             Prelude.return ()
                         else
                             Prelude.fail
                               ((Prelude.++)
                                  "Missing required fields: "
                                  (Prelude.show (missing :: [Prelude.String]))))
                      Prelude.return
                        (Lens.Family2.over
                           Data.ProtoLens.unknownFields (\ !t -> Prelude.reverse t) x)
               else
                   do tag <- Data.ProtoLens.Encoding.Bytes.getVarInt
                      case tag of
                        10
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                           Data.ProtoLens.Encoding.Bytes.isolate
                                             (Prelude.fromIntegral len) Data.ProtoLens.parseMessage)
                                       "var_name"
                                loop (Lens.Family2.set (Data.ProtoLens.Field.field @"varName") y x)
                        18
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                           Data.ProtoLens.Encoding.Bytes.isolate
                                             (Prelude.fromIntegral len) Data.ProtoLens.parseMessage)
                                       "source_info"
                                loop
                                  (Lens.Family2.set (Data.ProtoLens.Field.field @"sourceInfo") y x)
                        wire
                          -> do !y <- Data.ProtoLens.Encoding.Wire.parseTaggedValueFromWire
                                        wire
                                loop
                                  (Lens.Family2.over
                                     Data.ProtoLens.unknownFields (\ !t -> (:) y t) x)
      in
        (Data.ProtoLens.Encoding.Bytes.<?>)
          (do loop Data.ProtoLens.defMessage) "TypeVar"
  buildMessage
    = \ _x
        -> (Data.Monoid.<>)
             (case
                  Lens.Family2.view (Data.ProtoLens.Field.field @"maybe'varName") _x
              of
                Prelude.Nothing -> Data.Monoid.mempty
                (Prelude.Just _v)
                  -> (Data.Monoid.<>)
                       (Data.ProtoLens.Encoding.Bytes.putVarInt 10)
                       ((Prelude..)
                          (\ bs
                             -> (Data.Monoid.<>)
                                  (Data.ProtoLens.Encoding.Bytes.putVarInt
                                     (Prelude.fromIntegral (Data.ByteString.length bs)))
                                  (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                          Data.ProtoLens.encodeMessage _v))
             ((Data.Monoid.<>)
                (case
                     Lens.Family2.view
                       (Data.ProtoLens.Field.field @"maybe'sourceInfo") _x
                 of
                   Prelude.Nothing -> Data.Monoid.mempty
                   (Prelude.Just _v)
                     -> (Data.Monoid.<>)
                          (Data.ProtoLens.Encoding.Bytes.putVarInt 18)
                          ((Prelude..)
                             (\ bs
                                -> (Data.Monoid.<>)
                                     (Data.ProtoLens.Encoding.Bytes.putVarInt
                                        (Prelude.fromIntegral (Data.ByteString.length bs)))
                                     (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                             Data.ProtoLens.encodeMessage _v))
                (Data.ProtoLens.Encoding.Wire.buildFieldSet
                   (Lens.Family2.view Data.ProtoLens.unknownFields _x)))
instance Control.DeepSeq.NFData TypeVar where
  rnf
    = \ x__
        -> Control.DeepSeq.deepseq
             (_TypeVar'_unknownFields x__)
             (Control.DeepSeq.deepseq
                (_TypeVar'varName x__)
                (Control.DeepSeq.deepseq (_TypeVar'sourceInfo x__) ()))
{- | Fields :
     
         * 'Proto.Repls.LambdabuffersSource_Fields.typeclassName' @:: Lens' TypeclassDef TypeclassName@
         * 'Proto.Repls.LambdabuffersSource_Fields.maybe'typeclassName' @:: Lens' TypeclassDef (Prelude.Maybe TypeclassName)@
         * 'Proto.Repls.LambdabuffersSource_Fields.documentation' @:: Lens' TypeclassDef Data.Text.Text@
         * 'Proto.Repls.LambdabuffersSource_Fields.sourceInfo' @:: Lens' TypeclassDef SourceInfo@
         * 'Proto.Repls.LambdabuffersSource_Fields.maybe'sourceInfo' @:: Lens' TypeclassDef (Prelude.Maybe SourceInfo)@ -}
data TypeclassDef
  = TypeclassDef'_constructor {_TypeclassDef'typeclassName :: !(Prelude.Maybe TypeclassName),
                               _TypeclassDef'documentation :: !Data.Text.Text,
                               _TypeclassDef'sourceInfo :: !(Prelude.Maybe SourceInfo),
                               _TypeclassDef'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving stock (Prelude.Eq, Prelude.Ord)
instance Prelude.Show TypeclassDef where
  showsPrec _ __x __s
    = Prelude.showChar
        '{'
        (Prelude.showString
           (Data.ProtoLens.showMessageShort __x) (Prelude.showChar '}' __s))
instance Data.ProtoLens.Field.HasField TypeclassDef "typeclassName" TypeclassName where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _TypeclassDef'typeclassName
           (\ x__ y__ -> x__ {_TypeclassDef'typeclassName = y__}))
        (Data.ProtoLens.maybeLens Data.ProtoLens.defMessage)
instance Data.ProtoLens.Field.HasField TypeclassDef "maybe'typeclassName" (Prelude.Maybe TypeclassName) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _TypeclassDef'typeclassName
           (\ x__ y__ -> x__ {_TypeclassDef'typeclassName = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField TypeclassDef "documentation" Data.Text.Text where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _TypeclassDef'documentation
           (\ x__ y__ -> x__ {_TypeclassDef'documentation = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField TypeclassDef "sourceInfo" SourceInfo where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _TypeclassDef'sourceInfo
           (\ x__ y__ -> x__ {_TypeclassDef'sourceInfo = y__}))
        (Data.ProtoLens.maybeLens Data.ProtoLens.defMessage)
instance Data.ProtoLens.Field.HasField TypeclassDef "maybe'sourceInfo" (Prelude.Maybe SourceInfo) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _TypeclassDef'sourceInfo
           (\ x__ y__ -> x__ {_TypeclassDef'sourceInfo = y__}))
        Prelude.id
instance Data.ProtoLens.Message TypeclassDef where
  messageName _ = Data.Text.pack "lambdabuffers.source.TypeclassDef"
  packedMessageDescriptor _
    = "\n\
      \\fTypeclassDef\DC2J\n\
      \\SOtypeclass_name\CAN\SOH \SOH(\v2#.lambdabuffers.source.TypeclassNameR\rtypeclassName\DC2$\n\
      \\rdocumentation\CAN\STX \SOH(\tR\rdocumentation\DC2A\n\
      \\vsource_info\CAN\ETX \SOH(\v2 .lambdabuffers.source.SourceInfoR\n\
      \sourceInfo"
  packedFileDescriptor _ = packedFileDescriptor
  fieldsByTag
    = let
        typeclassName__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "typeclass_name"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor TypeclassName)
              (Data.ProtoLens.OptionalField
                 (Data.ProtoLens.Field.field @"maybe'typeclassName")) ::
              Data.ProtoLens.FieldDescriptor TypeclassDef
        documentation__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "documentation"
              (Data.ProtoLens.ScalarField Data.ProtoLens.StringField ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Text.Text)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional
                 (Data.ProtoLens.Field.field @"documentation")) ::
              Data.ProtoLens.FieldDescriptor TypeclassDef
        sourceInfo__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "source_info"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor SourceInfo)
              (Data.ProtoLens.OptionalField
                 (Data.ProtoLens.Field.field @"maybe'sourceInfo")) ::
              Data.ProtoLens.FieldDescriptor TypeclassDef
      in
        Data.Map.fromList
          [(Data.ProtoLens.Tag 1, typeclassName__field_descriptor),
           (Data.ProtoLens.Tag 2, documentation__field_descriptor),
           (Data.ProtoLens.Tag 3, sourceInfo__field_descriptor)]
  unknownFields
    = Lens.Family2.Unchecked.lens
        _TypeclassDef'_unknownFields
        (\ x__ y__ -> x__ {_TypeclassDef'_unknownFields = y__})
  defMessage
    = TypeclassDef'_constructor
        {_TypeclassDef'typeclassName = Prelude.Nothing,
         _TypeclassDef'documentation = Data.ProtoLens.fieldDefault,
         _TypeclassDef'sourceInfo = Prelude.Nothing,
         _TypeclassDef'_unknownFields = []}
  parseMessage
    = let
        loop ::
          TypeclassDef -> Data.ProtoLens.Encoding.Bytes.Parser TypeclassDef
        loop x
          = do end <- Data.ProtoLens.Encoding.Bytes.atEnd
               if end then
                   do (let missing = []
                       in
                         if Prelude.null missing then
                             Prelude.return ()
                         else
                             Prelude.fail
                               ((Prelude.++)
                                  "Missing required fields: "
                                  (Prelude.show (missing :: [Prelude.String]))))
                      Prelude.return
                        (Lens.Family2.over
                           Data.ProtoLens.unknownFields (\ !t -> Prelude.reverse t) x)
               else
                   do tag <- Data.ProtoLens.Encoding.Bytes.getVarInt
                      case tag of
                        10
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                           Data.ProtoLens.Encoding.Bytes.isolate
                                             (Prelude.fromIntegral len) Data.ProtoLens.parseMessage)
                                       "typeclass_name"
                                loop
                                  (Lens.Family2.set
                                     (Data.ProtoLens.Field.field @"typeclassName") y x)
                        18
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do value <- do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                                       Data.ProtoLens.Encoding.Bytes.getBytes
                                                         (Prelude.fromIntegral len)
                                           Data.ProtoLens.Encoding.Bytes.runEither
                                             (case Data.Text.Encoding.decodeUtf8' value of
                                                (Prelude.Left err)
                                                  -> Prelude.Left (Prelude.show err)
                                                (Prelude.Right r) -> Prelude.Right r))
                                       "documentation"
                                loop
                                  (Lens.Family2.set
                                     (Data.ProtoLens.Field.field @"documentation") y x)
                        26
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                           Data.ProtoLens.Encoding.Bytes.isolate
                                             (Prelude.fromIntegral len) Data.ProtoLens.parseMessage)
                                       "source_info"
                                loop
                                  (Lens.Family2.set (Data.ProtoLens.Field.field @"sourceInfo") y x)
                        wire
                          -> do !y <- Data.ProtoLens.Encoding.Wire.parseTaggedValueFromWire
                                        wire
                                loop
                                  (Lens.Family2.over
                                     Data.ProtoLens.unknownFields (\ !t -> (:) y t) x)
      in
        (Data.ProtoLens.Encoding.Bytes.<?>)
          (do loop Data.ProtoLens.defMessage) "TypeclassDef"
  buildMessage
    = \ _x
        -> (Data.Monoid.<>)
             (case
                  Lens.Family2.view
                    (Data.ProtoLens.Field.field @"maybe'typeclassName") _x
              of
                Prelude.Nothing -> Data.Monoid.mempty
                (Prelude.Just _v)
                  -> (Data.Monoid.<>)
                       (Data.ProtoLens.Encoding.Bytes.putVarInt 10)
                       ((Prelude..)
                          (\ bs
                             -> (Data.Monoid.<>)
                                  (Data.ProtoLens.Encoding.Bytes.putVarInt
                                     (Prelude.fromIntegral (Data.ByteString.length bs)))
                                  (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                          Data.ProtoLens.encodeMessage _v))
             ((Data.Monoid.<>)
                (let
                   _v
                     = Lens.Family2.view
                         (Data.ProtoLens.Field.field @"documentation") _x
                 in
                   if (Prelude.==) _v Data.ProtoLens.fieldDefault then
                       Data.Monoid.mempty
                   else
                       (Data.Monoid.<>)
                         (Data.ProtoLens.Encoding.Bytes.putVarInt 18)
                         ((Prelude..)
                            (\ bs
                               -> (Data.Monoid.<>)
                                    (Data.ProtoLens.Encoding.Bytes.putVarInt
                                       (Prelude.fromIntegral (Data.ByteString.length bs)))
                                    (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                            Data.Text.Encoding.encodeUtf8 _v))
                ((Data.Monoid.<>)
                   (case
                        Lens.Family2.view
                          (Data.ProtoLens.Field.field @"maybe'sourceInfo") _x
                    of
                      Prelude.Nothing -> Data.Monoid.mempty
                      (Prelude.Just _v)
                        -> (Data.Monoid.<>)
                             (Data.ProtoLens.Encoding.Bytes.putVarInt 26)
                             ((Prelude..)
                                (\ bs
                                   -> (Data.Monoid.<>)
                                        (Data.ProtoLens.Encoding.Bytes.putVarInt
                                           (Prelude.fromIntegral (Data.ByteString.length bs)))
                                        (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                                Data.ProtoLens.encodeMessage _v))
                   (Data.ProtoLens.Encoding.Wire.buildFieldSet
                      (Lens.Family2.view Data.ProtoLens.unknownFields _x))))
instance Control.DeepSeq.NFData TypeclassDef where
  rnf
    = \ x__
        -> Control.DeepSeq.deepseq
             (_TypeclassDef'_unknownFields x__)
             (Control.DeepSeq.deepseq
                (_TypeclassDef'typeclassName x__)
                (Control.DeepSeq.deepseq
                   (_TypeclassDef'documentation x__)
                   (Control.DeepSeq.deepseq (_TypeclassDef'sourceInfo x__) ())))
{- | Fields :
     
         * 'Proto.Repls.LambdabuffersSource_Fields.name' @:: Lens' TypeclassName Data.Text.Text@
         * 'Proto.Repls.LambdabuffersSource_Fields.sourceInfo' @:: Lens' TypeclassName SourceInfo@
         * 'Proto.Repls.LambdabuffersSource_Fields.maybe'sourceInfo' @:: Lens' TypeclassName (Prelude.Maybe SourceInfo)@ -}
data TypeclassName
  = TypeclassName'_constructor {_TypeclassName'name :: !Data.Text.Text,
                                _TypeclassName'sourceInfo :: !(Prelude.Maybe SourceInfo),
                                _TypeclassName'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving stock (Prelude.Eq, Prelude.Ord)
instance Prelude.Show TypeclassName where
  showsPrec _ __x __s
    = Prelude.showChar
        '{'
        (Prelude.showString
           (Data.ProtoLens.showMessageShort __x) (Prelude.showChar '}' __s))
instance Data.ProtoLens.Field.HasField TypeclassName "name" Data.Text.Text where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _TypeclassName'name (\ x__ y__ -> x__ {_TypeclassName'name = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField TypeclassName "sourceInfo" SourceInfo where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _TypeclassName'sourceInfo
           (\ x__ y__ -> x__ {_TypeclassName'sourceInfo = y__}))
        (Data.ProtoLens.maybeLens Data.ProtoLens.defMessage)
instance Data.ProtoLens.Field.HasField TypeclassName "maybe'sourceInfo" (Prelude.Maybe SourceInfo) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _TypeclassName'sourceInfo
           (\ x__ y__ -> x__ {_TypeclassName'sourceInfo = y__}))
        Prelude.id
instance Data.ProtoLens.Message TypeclassName where
  messageName _ = Data.Text.pack "lambdabuffers.source.TypeclassName"
  packedMessageDescriptor _
    = "\n\
      \\rTypeclassName\DC2\DC2\n\
      \\EOTname\CAN\SOH \SOH(\tR\EOTname\DC2A\n\
      \\vsource_info\CAN\STX \SOH(\v2 .lambdabuffers.source.SourceInfoR\n\
      \sourceInfo"
  packedFileDescriptor _ = packedFileDescriptor
  fieldsByTag
    = let
        name__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "name"
              (Data.ProtoLens.ScalarField Data.ProtoLens.StringField ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Text.Text)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional (Data.ProtoLens.Field.field @"name")) ::
              Data.ProtoLens.FieldDescriptor TypeclassName
        sourceInfo__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "source_info"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor SourceInfo)
              (Data.ProtoLens.OptionalField
                 (Data.ProtoLens.Field.field @"maybe'sourceInfo")) ::
              Data.ProtoLens.FieldDescriptor TypeclassName
      in
        Data.Map.fromList
          [(Data.ProtoLens.Tag 1, name__field_descriptor),
           (Data.ProtoLens.Tag 2, sourceInfo__field_descriptor)]
  unknownFields
    = Lens.Family2.Unchecked.lens
        _TypeclassName'_unknownFields
        (\ x__ y__ -> x__ {_TypeclassName'_unknownFields = y__})
  defMessage
    = TypeclassName'_constructor
        {_TypeclassName'name = Data.ProtoLens.fieldDefault,
         _TypeclassName'sourceInfo = Prelude.Nothing,
         _TypeclassName'_unknownFields = []}
  parseMessage
    = let
        loop ::
          TypeclassName -> Data.ProtoLens.Encoding.Bytes.Parser TypeclassName
        loop x
          = do end <- Data.ProtoLens.Encoding.Bytes.atEnd
               if end then
                   do (let missing = []
                       in
                         if Prelude.null missing then
                             Prelude.return ()
                         else
                             Prelude.fail
                               ((Prelude.++)
                                  "Missing required fields: "
                                  (Prelude.show (missing :: [Prelude.String]))))
                      Prelude.return
                        (Lens.Family2.over
                           Data.ProtoLens.unknownFields (\ !t -> Prelude.reverse t) x)
               else
                   do tag <- Data.ProtoLens.Encoding.Bytes.getVarInt
                      case tag of
                        10
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do value <- do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                                       Data.ProtoLens.Encoding.Bytes.getBytes
                                                         (Prelude.fromIntegral len)
                                           Data.ProtoLens.Encoding.Bytes.runEither
                                             (case Data.Text.Encoding.decodeUtf8' value of
                                                (Prelude.Left err)
                                                  -> Prelude.Left (Prelude.show err)
                                                (Prelude.Right r) -> Prelude.Right r))
                                       "name"
                                loop (Lens.Family2.set (Data.ProtoLens.Field.field @"name") y x)
                        18
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                           Data.ProtoLens.Encoding.Bytes.isolate
                                             (Prelude.fromIntegral len) Data.ProtoLens.parseMessage)
                                       "source_info"
                                loop
                                  (Lens.Family2.set (Data.ProtoLens.Field.field @"sourceInfo") y x)
                        wire
                          -> do !y <- Data.ProtoLens.Encoding.Wire.parseTaggedValueFromWire
                                        wire
                                loop
                                  (Lens.Family2.over
                                     Data.ProtoLens.unknownFields (\ !t -> (:) y t) x)
      in
        (Data.ProtoLens.Encoding.Bytes.<?>)
          (do loop Data.ProtoLens.defMessage) "TypeclassName"
  buildMessage
    = \ _x
        -> (Data.Monoid.<>)
             (let _v = Lens.Family2.view (Data.ProtoLens.Field.field @"name") _x
              in
                if (Prelude.==) _v Data.ProtoLens.fieldDefault then
                    Data.Monoid.mempty
                else
                    (Data.Monoid.<>)
                      (Data.ProtoLens.Encoding.Bytes.putVarInt 10)
                      ((Prelude..)
                         (\ bs
                            -> (Data.Monoid.<>)
                                 (Data.ProtoLens.Encoding.Bytes.putVarInt
                                    (Prelude.fromIntegral (Data.ByteString.length bs)))
                                 (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                         Data.Text.Encoding.encodeUtf8 _v))
             ((Data.Monoid.<>)
                (case
                     Lens.Family2.view
                       (Data.ProtoLens.Field.field @"maybe'sourceInfo") _x
                 of
                   Prelude.Nothing -> Data.Monoid.mempty
                   (Prelude.Just _v)
                     -> (Data.Monoid.<>)
                          (Data.ProtoLens.Encoding.Bytes.putVarInt 18)
                          ((Prelude..)
                             (\ bs
                                -> (Data.Monoid.<>)
                                     (Data.ProtoLens.Encoding.Bytes.putVarInt
                                        (Prelude.fromIntegral (Data.ByteString.length bs)))
                                     (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                             Data.ProtoLens.encodeMessage _v))
                (Data.ProtoLens.Encoding.Wire.buildFieldSet
                   (Lens.Family2.view Data.ProtoLens.unknownFields _x)))
instance Control.DeepSeq.NFData TypeclassName where
  rnf
    = \ x__
        -> Control.DeepSeq.deepseq
             (_TypeclassName'_unknownFields x__)
             (Control.DeepSeq.deepseq
                (_TypeclassName'name x__)
                (Control.DeepSeq.deepseq (_TypeclassName'sourceInfo x__) ()))
{- | Fields :
     
         * 'Proto.Repls.LambdabuffersSource_Fields.name' @:: Lens' VarName Data.Text.Text@
         * 'Proto.Repls.LambdabuffersSource_Fields.sourceInfo' @:: Lens' VarName SourceInfo@
         * 'Proto.Repls.LambdabuffersSource_Fields.maybe'sourceInfo' @:: Lens' VarName (Prelude.Maybe SourceInfo)@ -}
data VarName
  = VarName'_constructor {_VarName'name :: !Data.Text.Text,
                          _VarName'sourceInfo :: !(Prelude.Maybe SourceInfo),
                          _VarName'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving stock (Prelude.Eq, Prelude.Ord)
instance Prelude.Show VarName where
  showsPrec _ __x __s
    = Prelude.showChar
        '{'
        (Prelude.showString
           (Data.ProtoLens.showMessageShort __x) (Prelude.showChar '}' __s))
instance Data.ProtoLens.Field.HasField VarName "name" Data.Text.Text where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _VarName'name (\ x__ y__ -> x__ {_VarName'name = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField VarName "sourceInfo" SourceInfo where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _VarName'sourceInfo (\ x__ y__ -> x__ {_VarName'sourceInfo = y__}))
        (Data.ProtoLens.maybeLens Data.ProtoLens.defMessage)
instance Data.ProtoLens.Field.HasField VarName "maybe'sourceInfo" (Prelude.Maybe SourceInfo) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _VarName'sourceInfo (\ x__ y__ -> x__ {_VarName'sourceInfo = y__}))
        Prelude.id
instance Data.ProtoLens.Message VarName where
  messageName _ = Data.Text.pack "lambdabuffers.source.VarName"
  packedMessageDescriptor _
    = "\n\
      \\aVarName\DC2\DC2\n\
      \\EOTname\CAN\SOH \SOH(\tR\EOTname\DC2A\n\
      \\vsource_info\CAN\STX \SOH(\v2 .lambdabuffers.source.SourceInfoR\n\
      \sourceInfo"
  packedFileDescriptor _ = packedFileDescriptor
  fieldsByTag
    = let
        name__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "name"
              (Data.ProtoLens.ScalarField Data.ProtoLens.StringField ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Text.Text)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional (Data.ProtoLens.Field.field @"name")) ::
              Data.ProtoLens.FieldDescriptor VarName
        sourceInfo__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "source_info"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor SourceInfo)
              (Data.ProtoLens.OptionalField
                 (Data.ProtoLens.Field.field @"maybe'sourceInfo")) ::
              Data.ProtoLens.FieldDescriptor VarName
      in
        Data.Map.fromList
          [(Data.ProtoLens.Tag 1, name__field_descriptor),
           (Data.ProtoLens.Tag 2, sourceInfo__field_descriptor)]
  unknownFields
    = Lens.Family2.Unchecked.lens
        _VarName'_unknownFields
        (\ x__ y__ -> x__ {_VarName'_unknownFields = y__})
  defMessage
    = VarName'_constructor
        {_VarName'name = Data.ProtoLens.fieldDefault,
         _VarName'sourceInfo = Prelude.Nothing,
         _VarName'_unknownFields = []}
  parseMessage
    = let
        loop :: VarName -> Data.ProtoLens.Encoding.Bytes.Parser VarName
        loop x
          = do end <- Data.ProtoLens.Encoding.Bytes.atEnd
               if end then
                   do (let missing = []
                       in
                         if Prelude.null missing then
                             Prelude.return ()
                         else
                             Prelude.fail
                               ((Prelude.++)
                                  "Missing required fields: "
                                  (Prelude.show (missing :: [Prelude.String]))))
                      Prelude.return
                        (Lens.Family2.over
                           Data.ProtoLens.unknownFields (\ !t -> Prelude.reverse t) x)
               else
                   do tag <- Data.ProtoLens.Encoding.Bytes.getVarInt
                      case tag of
                        10
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do value <- do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                                       Data.ProtoLens.Encoding.Bytes.getBytes
                                                         (Prelude.fromIntegral len)
                                           Data.ProtoLens.Encoding.Bytes.runEither
                                             (case Data.Text.Encoding.decodeUtf8' value of
                                                (Prelude.Left err)
                                                  -> Prelude.Left (Prelude.show err)
                                                (Prelude.Right r) -> Prelude.Right r))
                                       "name"
                                loop (Lens.Family2.set (Data.ProtoLens.Field.field @"name") y x)
                        18
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                           Data.ProtoLens.Encoding.Bytes.isolate
                                             (Prelude.fromIntegral len) Data.ProtoLens.parseMessage)
                                       "source_info"
                                loop
                                  (Lens.Family2.set (Data.ProtoLens.Field.field @"sourceInfo") y x)
                        wire
                          -> do !y <- Data.ProtoLens.Encoding.Wire.parseTaggedValueFromWire
                                        wire
                                loop
                                  (Lens.Family2.over
                                     Data.ProtoLens.unknownFields (\ !t -> (:) y t) x)
      in
        (Data.ProtoLens.Encoding.Bytes.<?>)
          (do loop Data.ProtoLens.defMessage) "VarName"
  buildMessage
    = \ _x
        -> (Data.Monoid.<>)
             (let _v = Lens.Family2.view (Data.ProtoLens.Field.field @"name") _x
              in
                if (Prelude.==) _v Data.ProtoLens.fieldDefault then
                    Data.Monoid.mempty
                else
                    (Data.Monoid.<>)
                      (Data.ProtoLens.Encoding.Bytes.putVarInt 10)
                      ((Prelude..)
                         (\ bs
                            -> (Data.Monoid.<>)
                                 (Data.ProtoLens.Encoding.Bytes.putVarInt
                                    (Prelude.fromIntegral (Data.ByteString.length bs)))
                                 (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                         Data.Text.Encoding.encodeUtf8 _v))
             ((Data.Monoid.<>)
                (case
                     Lens.Family2.view
                       (Data.ProtoLens.Field.field @"maybe'sourceInfo") _x
                 of
                   Prelude.Nothing -> Data.Monoid.mempty
                   (Prelude.Just _v)
                     -> (Data.Monoid.<>)
                          (Data.ProtoLens.Encoding.Bytes.putVarInt 18)
                          ((Prelude..)
                             (\ bs
                                -> (Data.Monoid.<>)
                                     (Data.ProtoLens.Encoding.Bytes.putVarInt
                                        (Prelude.fromIntegral (Data.ByteString.length bs)))
                                     (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                             Data.ProtoLens.encodeMessage _v))
                (Data.ProtoLens.Encoding.Wire.buildFieldSet
                   (Lens.Family2.view Data.ProtoLens.unknownFields _x)))
instance Control.DeepSeq.NFData VarName where
  rnf
    = \ x__
        -> Control.DeepSeq.deepseq
             (_VarName'_unknownFields x__)
             (Control.DeepSeq.deepseq
                (_VarName'name x__)
                (Control.DeepSeq.deepseq (_VarName'sourceInfo x__) ()))
packedFileDescriptor :: Data.ByteString.ByteString
packedFileDescriptor
  = "\n\
    \ repls/lambdabuffers-source.proto\DC2\DC4lambdabuffers.source\"\158\SOH\n\
    \\n\
    \SourceInfo\DC2\DC2\n\
    \\EOTfile\CAN\SOH \SOH(\tR\EOTfile\DC2?\n\
    \\bpos_from\CAN\STX \SOH(\v2$.lambdabuffers.source.SourcePositionR\aposFrom\DC2;\n\
    \\ACKpos_to\CAN\ETX \SOH(\v2$.lambdabuffers.source.SourcePositionR\ENQposTo\":\n\
    \\SOSourcePosition\DC2\SYN\n\
    \\ACKcolumn\CAN\SOH \SOH(\ENQR\ACKcolumn\DC2\DLE\n\
    \\ETXrow\CAN\STX \SOH(\ENQR\ETXrow\"\193\STX\n\
    \\EOTType\DC2:\n\
    \\btype_var\CAN\SOH \SOH(\v2\GS.lambdabuffers.source.TypeVarH\NULR\atypeVar\DC2:\n\
    \\btype_con\CAN\STX \SOH(\v2\GS.lambdabuffers.source.TypeConH\NULR\atypeCon\DC2:\n\
    \\btype_app\CAN\ETX \SOH(\v2\GS.lambdabuffers.source.TypeAppH\NULR\atypeApp\DC2:\n\
    \\btype_ref\CAN\EOT \SOH(\v2\GS.lambdabuffers.source.TypeRefH\NULR\atypeRef\DC2A\n\
    \\vsource_info\CAN\ENQ \SOH(\v2 .lambdabuffers.source.SourceInfoR\n\
    \sourceInfoB\ACK\n\
    \\EOTtype\"c\n\
    \\EOTKind\DC2\CAN\n\
    \\aVarName\CAN\SOH \ETX(\tR\aVarName\DC2A\n\
    \\vsource_info\CAN\STX \SOH(\v2 .lambdabuffers.source.SourceInfoR\n\
    \sourceInfo\"\134\SOH\n\
    \\aTypeVar\DC28\n\
    \\bvar_name\CAN\SOH \SOH(\v2\GS.lambdabuffers.source.VarNameR\avarName\DC2A\n\
    \\vsource_info\CAN\STX \SOH(\v2 .lambdabuffers.source.SourceInfoR\n\
    \sourceInfo\"|\n\
    \\aTypeCon\DC2.\n\
    \\EOTkind\CAN\SOH \SOH(\v2\SUB.lambdabuffers.source.KindR\EOTkind\DC2A\n\
    \\vsource_info\CAN\STX \SOH(\v2 .lambdabuffers.source.SourceInfoR\n\
    \sourceInfo\"\186\SOH\n\
    \\aTypeApp\DC25\n\
    \\btype_abs\CAN\SOH \SOH(\v2\SUB.lambdabuffers.source.TypeR\atypeAbs\DC25\n\
    \\btype_arg\CAN\STX \SOH(\v2\SUB.lambdabuffers.source.TypeR\atypeArg\DC2A\n\
    \\vsource_info\CAN\ETX \SOH(\v2 .lambdabuffers.source.SourceInfoR\n\
    \sourceInfo\"\246\SOH\n\
    \\aTypeRef\DC2J\n\
    \\SOlocal_type_ref\CAN\SOH \SOH(\v2\".lambdabuffers.source.LocalTypeRefH\NULR\flocalTypeRef\DC2P\n\
    \\DLEforeign_type_ref\CAN\STX \SOH(\v2$.lambdabuffers.source.ForeignTypeRefH\NULR\SOforeignTypeRef\DC2A\n\
    \\vsource_info\CAN\ETX \SOH(\v2 .lambdabuffers.source.SourceInfoR\n\
    \sourceInfoB\n\
    \\n\
    \\btype_ref\"\152\SOH\n\
    \\fLocalTypeRef\DC2E\n\
    \\rtype_con_name\CAN\SOH \SOH(\v2!.lambdabuffers.source.TypeConNameR\vtypeConName\DC2A\n\
    \\vsource_info\CAN\STX \SOH(\v2 .lambdabuffers.source.SourceInfoR\n\
    \sourceInfo\"\221\SOH\n\
    \\SOForeignTypeRef\DC2A\n\
    \\vmodule_name\CAN\SOH \SOH(\v2 .lambdabuffers.source.ModuleNameR\n\
    \moduleName\DC2E\n\
    \\rtype_con_name\CAN\STX \SOH(\v2!.lambdabuffers.source.TypeConNameR\vtypeConName\DC2A\n\
    \\vsource_info\CAN\ETX \SOH(\v2 .lambdabuffers.source.SourceInfoR\n\
    \sourceInfo\"d\n\
    \\vTypeConName\DC2\DC2\n\
    \\EOTname\CAN\SOH \SOH(\tR\EOTname\DC2A\n\
    \\vsource_info\CAN\STX \SOH(\v2 .lambdabuffers.source.SourceInfoR\n\
    \sourceInfo\"c\n\
    \\n\
    \ModuleName\DC2\DC2\n\
    \\EOTname\CAN\SOH \SOH(\tR\EOTname\DC2A\n\
    \\vsource_info\CAN\STX \SOH(\v2 .lambdabuffers.source.SourceInfoR\n\
    \sourceInfo\"`\n\
    \\aVarName\DC2\DC2\n\
    \\EOTname\CAN\SOH \SOH(\tR\EOTname\DC2A\n\
    \\vsource_info\CAN\STX \SOH(\v2 .lambdabuffers.source.SourceInfoR\n\
    \sourceInfo\"c\n\
    \\n\
    \ConstrName\DC2\DC2\n\
    \\EOTname\CAN\SOH \SOH(\tR\EOTname\DC2A\n\
    \\vsource_info\CAN\STX \SOH(\v2 .lambdabuffers.source.SourceInfoR\n\
    \sourceInfo\"b\n\
    \\tFieldName\DC2\DC2\n\
    \\EOTname\CAN\SOH \SOH(\tR\EOTname\DC2A\n\
    \\vsource_info\CAN\STX \SOH(\v2 .lambdabuffers.source.SourceInfoR\n\
    \sourceInfo\"f\n\
    \\rTypeclassName\DC2\DC2\n\
    \\EOTname\CAN\SOH \SOH(\tR\EOTname\DC2A\n\
    \\vsource_info\CAN\STX \SOH(\v2 .lambdabuffers.source.SourceInfoR\n\
    \sourceInfo\"\137\STX\n\
    \\SOTypeDefinition\DC2>\n\
    \\ttype_name\CAN\SOH \SOH(\v2!.lambdabuffers.source.TypeConNameR\btypeName\DC27\n\
    \\ttype_kind\CAN\STX \SOH(\v2\SUB.lambdabuffers.source.KindR\btypeKind\DC2;\n\
    \\ttype_body\CAN\ETX \SOH(\v2\RS.lambdabuffers.source.TypeBodyR\btypeBody\DC2A\n\
    \\vsource_info\CAN\EOT \SOH(\v2 .lambdabuffers.source.SourceInfoR\n\
    \sourceInfo\"\188\SOH\n\
    \\bTypeBody\DC26\n\
    \\ACKopaque\CAN\SOH \SOH(\v2\FS.lambdabuffers.source.OpaqueH\NULR\ACKopaque\DC2-\n\
    \\ETXsum\CAN\STX \SOH(\v2\EM.lambdabuffers.source.SumH\NULR\ETXsum\DC2A\n\
    \\vsource_info\CAN\ETX \SOH(\v2 .lambdabuffers.source.SourceInfoR\n\
    \sourceInfoB\ACK\n\
    \\EOTbody\"K\n\
    \\ACKOpaque\DC2A\n\
    \\vsource_info\CAN\SOH \SOH(\v2 .lambdabuffers.source.SourceInfoR\n\
    \sourceInfo\"\249\SOH\n\
    \\ETXSum\DC2O\n\
    \\fconstructors\CAN\SOH \ETX(\v2+.lambdabuffers.source.Sum.ConstructorsEntryR\fconstructors\DC2A\n\
    \\vsource_info\CAN\STX \SOH(\v2 .lambdabuffers.source.SourceInfoR\n\
    \sourceInfo\SUB^\n\
    \\DC1ConstructorsEntry\DC2\DLE\n\
    \\ETXkey\CAN\SOH \SOH(\tR\ETXkey\DC23\n\
    \\ENQvalue\CAN\STX \SOH(\v2\GS.lambdabuffers.source.ProductR\ENQvalue:\STX8\SOH\"\252\SOH\n\
    \\aProduct\DC23\n\
    \\ENQempty\CAN\SOH \SOH(\v2\ESC.lambdabuffers.source.EmptyH\NULR\ENQempty\DC26\n\
    \\ACKrecord\CAN\STX \SOH(\v2\FS.lambdabuffers.source.RecordH\NULR\ACKrecord\DC26\n\
    \\ACKntuple\CAN\ETX \SOH(\v2\FS.lambdabuffers.source.NTupleH\NULR\ACKntuple\DC2A\n\
    \\vsource_info\CAN\EOT \SOH(\v2 .lambdabuffers.source.SourceInfoR\n\
    \sourceInfoB\t\n\
    \\aproduct\"J\n\
    \\ENQEmpty\DC2A\n\
    \\vsource_info\CAN\SOH \SOH(\v2 .lambdabuffers.source.SourceInfoR\n\
    \sourceInfo\"\230\SOH\n\
    \\ACKRecord\DC2@\n\
    \\ACKfields\CAN\SOH \ETX(\v2(.lambdabuffers.source.Record.FieldsEntryR\ACKfields\DC2C\n\
    \\fsource_infog\CAN\STX \SOH(\v2 .lambdabuffers.source.SourceInfoR\vsourceInfog\SUBU\n\
    \\vFieldsEntry\DC2\DLE\n\
    \\ETXkey\CAN\SOH \SOH(\tR\ETXkey\DC20\n\
    \\ENQvalue\CAN\STX \SOH(\v2\SUB.lambdabuffers.source.TypeR\ENQvalue:\STX8\SOH\"\DEL\n\
    \\ACKNTuple\DC22\n\
    \\ACKfields\CAN\SOH \ETX(\v2\SUB.lambdabuffers.source.TypeR\ACKfields\DC2A\n\
    \\vsource_info\CAN\STX \SOH(\v2 .lambdabuffers.source.SourceInfoR\n\
    \sourceInfo\"\195\SOH\n\
    \\fTypeclassDef\DC2J\n\
    \\SOtypeclass_name\CAN\SOH \SOH(\v2#.lambdabuffers.source.TypeclassNameR\rtypeclassName\DC2$\n\
    \\rdocumentation\CAN\STX \SOH(\tR\rdocumentation\DC2A\n\
    \\vsource_info\CAN\ETX \SOH(\v2 .lambdabuffers.source.SourceInfoR\n\
    \sourceInfo\"\147\STX\n\
    \\SOInstanceClause\DC2J\n\
    \\SOtypeclass_name\CAN\SOH \SOH(\v2#.lambdabuffers.source.TypeclassNameR\rtypeclassName\DC2.\n\
    \\EOThead\CAN\STX \SOH(\v2\SUB.lambdabuffers.source.TypeR\EOThead\DC2B\n\
    \\vconstraints\CAN\ETX \ETX(\v2 .lambdabuffers.source.ConstraintR\vconstraints\DC2A\n\
    \\vsource_info\CAN\EOT \SOH(\v2 .lambdabuffers.source.SourceInfoR\n\
    \sourceInfo\"\213\SOH\n\
    \\n\
    \Constraint\DC2J\n\
    \\SOtypeclass_name\CAN\SOH \SOH(\v2#.lambdabuffers.source.TypeclassNameR\rtypeclassName\DC28\n\
    \\targuments\CAN\STX \ETX(\v2\SUB.lambdabuffers.source.TypeR\targuments\DC2A\n\
    \\vsource_info\CAN\ETX \SOH(\v2 .lambdabuffers.source.SourceInfoR\n\
    \sourceInfo\"\149\STX\n\
    \\ACKModule\DC2A\n\
    \\vmodule_name\CAN\SOH \SOH(\v2 .lambdabuffers.source.ModuleNameR\n\
    \moduleName\DC2A\n\
    \\ttype_defs\CAN\STX \ETX(\v2$.lambdabuffers.source.TypeDefinitionR\btypeDefs\DC2B\n\
    \\tinstances\CAN\ETX \ETX(\v2$.lambdabuffers.source.InstanceClauseR\tinstances\DC2A\n\
    \\vsource_info\CAN\EOT \SOH(\v2 .lambdabuffers.source.SourceInfoR\n\
    \sourceInfoJ\129\&4\n\
    \\a\DC2\ENQ\SOH\NUL\246\SOH\SOH\n\
    \\141\SOH\n\
    \\SOH\f\DC2\ETX\SOH\NUL\DC2\SUB\130\SOH Run with: protoc --plugin=protoc-gen-haskell=`which proto-lens-protoc`  --haskell_out=proto_out repls/lambdabuffers-source.proto\n\
    \\n\
    \\b\n\
    \\SOH\STX\DC2\ETX\ETX\NUL\GS\n\
    \\221\SOH\n\
    \\STX\EOT\NUL\DC2\EOT\DC3\NUL\ETB\SOH2\208\SOH Frontend Source information\n\
    \\n\
    \data SourceInfo = SourceInfo {\n\
    \file :: FilePath,\n\
    \posFrom :: SourcePosition,\n\
    \posTo :: SourcePosition\n\
    \}\n\
    \\n\
    \data SourcePosition = SourcePosition {\n\
    \column :: Integer,\n\
    \row :: Integer\n\
    \}\n\
    \\n\
    \\n\
    \\n\
    \\ETX\EOT\NUL\SOH\DC2\ETX\DC3\b\DC2\n\
    \\v\n\
    \\EOT\EOT\NUL\STX\NUL\DC2\ETX\DC4\STX\DC2\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\NUL\ENQ\DC2\ETX\DC4\STX\b\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\NUL\SOH\DC2\ETX\DC4\t\r\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\NUL\ETX\DC2\ETX\DC4\DLE\DC1\n\
    \\v\n\
    \\EOT\EOT\NUL\STX\SOH\DC2\ETX\NAK\STX\RS\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\SOH\ACK\DC2\ETX\NAK\STX\DLE\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\SOH\SOH\DC2\ETX\NAK\DC1\EM\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\SOH\ETX\DC2\ETX\NAK\FS\GS\n\
    \\v\n\
    \\EOT\EOT\NUL\STX\STX\DC2\ETX\SYN\STX\FS\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\STX\ACK\DC2\ETX\SYN\STX\DLE\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\STX\SOH\DC2\ETX\SYN\DC1\ETB\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\STX\ETX\DC2\ETX\SYN\SUB\ESC\n\
    \\n\
    \\n\
    \\STX\EOT\SOH\DC2\EOT\EM\NUL\FS\SOH\n\
    \\n\
    \\n\
    \\ETX\EOT\SOH\SOH\DC2\ETX\EM\b\SYN\n\
    \\v\n\
    \\EOT\EOT\SOH\STX\NUL\DC2\ETX\SUB\STX\DC3\n\
    \\f\n\
    \\ENQ\EOT\SOH\STX\NUL\ENQ\DC2\ETX\SUB\STX\a\n\
    \\f\n\
    \\ENQ\EOT\SOH\STX\NUL\SOH\DC2\ETX\SUB\b\SO\n\
    \\f\n\
    \\ENQ\EOT\SOH\STX\NUL\ETX\DC2\ETX\SUB\DC1\DC2\n\
    \\v\n\
    \\EOT\EOT\SOH\STX\SOH\DC2\ETX\ESC\STX\DLE\n\
    \\f\n\
    \\ENQ\EOT\SOH\STX\SOH\ENQ\DC2\ETX\ESC\STX\a\n\
    \\f\n\
    \\ENQ\EOT\SOH\STX\SOH\SOH\DC2\ETX\ESC\b\v\n\
    \\f\n\
    \\ENQ\EOT\SOH\STX\SOH\ETX\DC2\ETX\ESC\SO\SI\n\
    \\183\SOH\n\
    \\STX\EOT\STX\DC2\EOT(\NUL0\SOH2\170\SOH Type\n\
    \\n\
    \data Ty = TyVar VarName | TCon TKind | TApp Ty Ty | TyRef TypeRef\n\
    \\n\
    \newtype TKind = TKind [VarName]\n\
    \\n\
    \data TypeRef = Local TyConName | Foreign ModuleName TyConName\n\
    \\n\
    \\n\
    \\n\
    \\n\
    \\ETX\EOT\STX\SOH\DC2\ETX(\b\f\n\
    \\f\n\
    \\EOT\EOT\STX\b\NUL\DC2\EOT)\STX.\ETX\n\
    \\f\n\
    \\ENQ\EOT\STX\b\NUL\SOH\DC2\ETX)\b\f\n\
    \\v\n\
    \\EOT\EOT\STX\STX\NUL\DC2\ETX*\ENQ\SUB\n\
    \\f\n\
    \\ENQ\EOT\STX\STX\NUL\ACK\DC2\ETX*\ENQ\f\n\
    \\f\n\
    \\ENQ\EOT\STX\STX\NUL\SOH\DC2\ETX*\r\NAK\n\
    \\f\n\
    \\ENQ\EOT\STX\STX\NUL\ETX\DC2\ETX*\CAN\EM\n\
    \\v\n\
    \\EOT\EOT\STX\STX\SOH\DC2\ETX+\ENQ\SUB\n\
    \\f\n\
    \\ENQ\EOT\STX\STX\SOH\ACK\DC2\ETX+\ENQ\f\n\
    \\f\n\
    \\ENQ\EOT\STX\STX\SOH\SOH\DC2\ETX+\r\NAK\n\
    \\f\n\
    \\ENQ\EOT\STX\STX\SOH\ETX\DC2\ETX+\CAN\EM\n\
    \\v\n\
    \\EOT\EOT\STX\STX\STX\DC2\ETX,\ENQ\SUB\n\
    \\f\n\
    \\ENQ\EOT\STX\STX\STX\ACK\DC2\ETX,\ENQ\f\n\
    \\f\n\
    \\ENQ\EOT\STX\STX\STX\SOH\DC2\ETX,\r\NAK\n\
    \\f\n\
    \\ENQ\EOT\STX\STX\STX\ETX\DC2\ETX,\CAN\EM\n\
    \\v\n\
    \\EOT\EOT\STX\STX\ETX\DC2\ETX-\ENQ\SUB\n\
    \\f\n\
    \\ENQ\EOT\STX\STX\ETX\ACK\DC2\ETX-\ENQ\f\n\
    \\f\n\
    \\ENQ\EOT\STX\STX\ETX\SOH\DC2\ETX-\r\NAK\n\
    \\f\n\
    \\ENQ\EOT\STX\STX\ETX\ETX\DC2\ETX-\CAN\EM\n\
    \\v\n\
    \\EOT\EOT\STX\STX\EOT\DC2\ETX/\STX\GS\n\
    \\f\n\
    \\ENQ\EOT\STX\STX\EOT\ACK\DC2\ETX/\STX\f\n\
    \\f\n\
    \\ENQ\EOT\STX\STX\EOT\SOH\DC2\ETX/\r\CAN\n\
    \\f\n\
    \\ENQ\EOT\STX\STX\EOT\ETX\DC2\ETX/\ESC\FS\n\
    \\n\
    \\n\
    \\STX\EOT\ETX\DC2\EOT2\NUL5\SOH\n\
    \\n\
    \\n\
    \\ETX\EOT\ETX\SOH\DC2\ETX2\b\f\n\
    \\v\n\
    \\EOT\EOT\ETX\STX\NUL\DC2\ETX3\STX\RS\n\
    \\f\n\
    \\ENQ\EOT\ETX\STX\NUL\EOT\DC2\ETX3\STX\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\ETX\STX\NUL\ENQ\DC2\ETX3\v\DC1\n\
    \\f\n\
    \\ENQ\EOT\ETX\STX\NUL\SOH\DC2\ETX3\DC2\EM\n\
    \\f\n\
    \\ENQ\EOT\ETX\STX\NUL\ETX\DC2\ETX3\FS\GS\n\
    \\v\n\
    \\EOT\EOT\ETX\STX\SOH\DC2\ETX4\STX\GS\n\
    \\f\n\
    \\ENQ\EOT\ETX\STX\SOH\ACK\DC2\ETX4\STX\f\n\
    \\f\n\
    \\ENQ\EOT\ETX\STX\SOH\SOH\DC2\ETX4\r\CAN\n\
    \\f\n\
    \\ENQ\EOT\ETX\STX\SOH\ETX\DC2\ETX4\ESC\FS\n\
    \\n\
    \\n\
    \\STX\EOT\EOT\DC2\EOT7\NUL:\SOH\n\
    \\n\
    \\n\
    \\ETX\EOT\EOT\SOH\DC2\ETX7\b\SI\n\
    \\v\n\
    \\EOT\EOT\EOT\STX\NUL\DC2\ETX8\STX\ETB\n\
    \\f\n\
    \\ENQ\EOT\EOT\STX\NUL\ACK\DC2\ETX8\STX\t\n\
    \\f\n\
    \\ENQ\EOT\EOT\STX\NUL\SOH\DC2\ETX8\n\
    \\DC2\n\
    \\f\n\
    \\ENQ\EOT\EOT\STX\NUL\ETX\DC2\ETX8\NAK\SYN\n\
    \\v\n\
    \\EOT\EOT\EOT\STX\SOH\DC2\ETX9\STX\GS\n\
    \\f\n\
    \\ENQ\EOT\EOT\STX\SOH\ACK\DC2\ETX9\STX\f\n\
    \\f\n\
    \\ENQ\EOT\EOT\STX\SOH\SOH\DC2\ETX9\r\CAN\n\
    \\f\n\
    \\ENQ\EOT\EOT\STX\SOH\ETX\DC2\ETX9\ESC\FS\n\
    \\n\
    \\n\
    \\STX\EOT\ENQ\DC2\EOT<\NUL?\SOH\n\
    \\n\
    \\n\
    \\ETX\EOT\ENQ\SOH\DC2\ETX<\b\SI\n\
    \\v\n\
    \\EOT\EOT\ENQ\STX\NUL\DC2\ETX=\STX\DLE\n\
    \\f\n\
    \\ENQ\EOT\ENQ\STX\NUL\ACK\DC2\ETX=\STX\ACK\n\
    \\f\n\
    \\ENQ\EOT\ENQ\STX\NUL\SOH\DC2\ETX=\a\v\n\
    \\f\n\
    \\ENQ\EOT\ENQ\STX\NUL\ETX\DC2\ETX=\SO\SI\n\
    \\v\n\
    \\EOT\EOT\ENQ\STX\SOH\DC2\ETX>\STX\GS\n\
    \\f\n\
    \\ENQ\EOT\ENQ\STX\SOH\ACK\DC2\ETX>\STX\f\n\
    \\f\n\
    \\ENQ\EOT\ENQ\STX\SOH\SOH\DC2\ETX>\r\CAN\n\
    \\f\n\
    \\ENQ\EOT\ENQ\STX\SOH\ETX\DC2\ETX>\ESC\FS\n\
    \\n\
    \\n\
    \\STX\EOT\ACK\DC2\EOTA\NULE\SOH\n\
    \\n\
    \\n\
    \\ETX\EOT\ACK\SOH\DC2\ETXA\b\SI\n\
    \\v\n\
    \\EOT\EOT\ACK\STX\NUL\DC2\ETXB\STX\DC4\n\
    \\f\n\
    \\ENQ\EOT\ACK\STX\NUL\ACK\DC2\ETXB\STX\ACK\n\
    \\f\n\
    \\ENQ\EOT\ACK\STX\NUL\SOH\DC2\ETXB\a\SI\n\
    \\f\n\
    \\ENQ\EOT\ACK\STX\NUL\ETX\DC2\ETXB\DC2\DC3\n\
    \\v\n\
    \\EOT\EOT\ACK\STX\SOH\DC2\ETXC\STX\DC4\n\
    \\f\n\
    \\ENQ\EOT\ACK\STX\SOH\ACK\DC2\ETXC\STX\ACK\n\
    \\f\n\
    \\ENQ\EOT\ACK\STX\SOH\SOH\DC2\ETXC\a\SI\n\
    \\f\n\
    \\ENQ\EOT\ACK\STX\SOH\ETX\DC2\ETXC\DC2\DC3\n\
    \\v\n\
    \\EOT\EOT\ACK\STX\STX\DC2\ETXD\STX\GS\n\
    \\f\n\
    \\ENQ\EOT\ACK\STX\STX\ACK\DC2\ETXD\STX\f\n\
    \\f\n\
    \\ENQ\EOT\ACK\STX\STX\SOH\DC2\ETXD\r\CAN\n\
    \\f\n\
    \\ENQ\EOT\ACK\STX\STX\ETX\DC2\ETXD\ESC\FS\n\
    \\n\
    \\n\
    \\STX\EOT\a\DC2\EOTG\NULL\SOH\n\
    \\n\
    \\n\
    \\ETX\EOT\a\SOH\DC2\ETXG\b\SI\n\
    \\f\n\
    \\EOT\EOT\a\b\NUL\DC2\EOTG\DC3J\ETX\n\
    \\f\n\
    \\ENQ\EOT\a\b\NUL\SOH\DC2\ETXG\EM!\n\
    \\v\n\
    \\EOT\EOT\a\STX\NUL\DC2\ETXH\EOT$\n\
    \\f\n\
    \\ENQ\EOT\a\STX\NUL\ACK\DC2\ETXH\EOT\DLE\n\
    \\f\n\
    \\ENQ\EOT\a\STX\NUL\SOH\DC2\ETXH\DC1\US\n\
    \\f\n\
    \\ENQ\EOT\a\STX\NUL\ETX\DC2\ETXH\"#\n\
    \\v\n\
    \\EOT\EOT\a\STX\SOH\DC2\ETXI\EOT(\n\
    \\f\n\
    \\ENQ\EOT\a\STX\SOH\ACK\DC2\ETXI\EOT\DC2\n\
    \\f\n\
    \\ENQ\EOT\a\STX\SOH\SOH\DC2\ETXI\DC3#\n\
    \\f\n\
    \\ENQ\EOT\a\STX\SOH\ETX\DC2\ETXI&'\n\
    \\v\n\
    \\EOT\EOT\a\STX\STX\DC2\ETXK\STX\GS\n\
    \\f\n\
    \\ENQ\EOT\a\STX\STX\ACK\DC2\ETXK\STX\f\n\
    \\f\n\
    \\ENQ\EOT\a\STX\STX\SOH\DC2\ETXK\r\CAN\n\
    \\f\n\
    \\ENQ\EOT\a\STX\STX\ETX\DC2\ETXK\ESC\FS\n\
    \\n\
    \\n\
    \\STX\EOT\b\DC2\EOTN\NULQ\SOH\n\
    \\n\
    \\n\
    \\ETX\EOT\b\SOH\DC2\ETXN\b\DC4\n\
    \\v\n\
    \\EOT\EOT\b\STX\NUL\DC2\ETXO\STX \n\
    \\f\n\
    \\ENQ\EOT\b\STX\NUL\ACK\DC2\ETXO\STX\r\n\
    \\f\n\
    \\ENQ\EOT\b\STX\NUL\SOH\DC2\ETXO\SO\ESC\n\
    \\f\n\
    \\ENQ\EOT\b\STX\NUL\ETX\DC2\ETXO\RS\US\n\
    \\v\n\
    \\EOT\EOT\b\STX\SOH\DC2\ETXP\STX\GS\n\
    \\f\n\
    \\ENQ\EOT\b\STX\SOH\ACK\DC2\ETXP\STX\f\n\
    \\f\n\
    \\ENQ\EOT\b\STX\SOH\SOH\DC2\ETXP\r\CAN\n\
    \\f\n\
    \\ENQ\EOT\b\STX\SOH\ETX\DC2\ETXP\ESC\FS\n\
    \\n\
    \\n\
    \\STX\EOT\t\DC2\EOTS\NULW\SOH\n\
    \\n\
    \\n\
    \\ETX\EOT\t\SOH\DC2\ETXS\b\SYN\n\
    \\v\n\
    \\EOT\EOT\t\STX\NUL\DC2\ETXT\STX\GS\n\
    \\f\n\
    \\ENQ\EOT\t\STX\NUL\ACK\DC2\ETXT\STX\f\n\
    \\f\n\
    \\ENQ\EOT\t\STX\NUL\SOH\DC2\ETXT\r\CAN\n\
    \\f\n\
    \\ENQ\EOT\t\STX\NUL\ETX\DC2\ETXT\ESC\FS\n\
    \\v\n\
    \\EOT\EOT\t\STX\SOH\DC2\ETXU\STX \n\
    \\f\n\
    \\ENQ\EOT\t\STX\SOH\ACK\DC2\ETXU\STX\r\n\
    \\f\n\
    \\ENQ\EOT\t\STX\SOH\SOH\DC2\ETXU\SO\ESC\n\
    \\f\n\
    \\ENQ\EOT\t\STX\SOH\ETX\DC2\ETXU\RS\US\n\
    \\v\n\
    \\EOT\EOT\t\STX\STX\DC2\ETXV\STX\GS\n\
    \\f\n\
    \\ENQ\EOT\t\STX\STX\ACK\DC2\ETXV\STX\f\n\
    \\f\n\
    \\ENQ\EOT\t\STX\STX\SOH\DC2\ETXV\r\CAN\n\
    \\f\n\
    \\ENQ\EOT\t\STX\STX\ETX\DC2\ETXV\ESC\FS\n\
    \\159\SOH\n\
    \\STX\EOT\n\
    \\DC2\EOTc\NULf\SOH2\146\SOH Names\n\
    \\n\
    \type ModuleName = Text\n\
    \type TypeConName = Text\n\
    \type ConstrName = Text\n\
    \type VarName = Text\n\
    \type FieldName = Text\n\
    \type TypeclassName = Text\n\
    \\n\
    \\n\
    \\n\
    \\ETX\EOT\n\
    \\SOH\DC2\ETXc\b\DC3\n\
    \\v\n\
    \\EOT\EOT\n\
    \\STX\NUL\DC2\ETXd\STX\DC2\n\
    \\f\n\
    \\ENQ\EOT\n\
    \\STX\NUL\ENQ\DC2\ETXd\STX\b\n\
    \\f\n\
    \\ENQ\EOT\n\
    \\STX\NUL\SOH\DC2\ETXd\t\r\n\
    \\f\n\
    \\ENQ\EOT\n\
    \\STX\NUL\ETX\DC2\ETXd\DLE\DC1\n\
    \\v\n\
    \\EOT\EOT\n\
    \\STX\SOH\DC2\ETXe\STX\GS\n\
    \\f\n\
    \\ENQ\EOT\n\
    \\STX\SOH\ACK\DC2\ETXe\STX\f\n\
    \\f\n\
    \\ENQ\EOT\n\
    \\STX\SOH\SOH\DC2\ETXe\r\CAN\n\
    \\f\n\
    \\ENQ\EOT\n\
    \\STX\SOH\ETX\DC2\ETXe\ESC\FS\n\
    \\n\
    \\n\
    \\STX\EOT\v\DC2\EOTh\NULk\SOH\n\
    \\n\
    \\n\
    \\ETX\EOT\v\SOH\DC2\ETXh\b\DC2\n\
    \\v\n\
    \\EOT\EOT\v\STX\NUL\DC2\ETXi\STX\DC2\n\
    \\f\n\
    \\ENQ\EOT\v\STX\NUL\ENQ\DC2\ETXi\STX\b\n\
    \\f\n\
    \\ENQ\EOT\v\STX\NUL\SOH\DC2\ETXi\t\r\n\
    \\f\n\
    \\ENQ\EOT\v\STX\NUL\ETX\DC2\ETXi\DLE\DC1\n\
    \\v\n\
    \\EOT\EOT\v\STX\SOH\DC2\ETXj\STX\GS\n\
    \\f\n\
    \\ENQ\EOT\v\STX\SOH\ACK\DC2\ETXj\STX\f\n\
    \\f\n\
    \\ENQ\EOT\v\STX\SOH\SOH\DC2\ETXj\r\CAN\n\
    \\f\n\
    \\ENQ\EOT\v\STX\SOH\ETX\DC2\ETXj\ESC\FS\n\
    \\n\
    \\n\
    \\STX\EOT\f\DC2\EOTm\NULp\SOH\n\
    \\n\
    \\n\
    \\ETX\EOT\f\SOH\DC2\ETXm\b\SI\n\
    \\v\n\
    \\EOT\EOT\f\STX\NUL\DC2\ETXn\STX\DC2\n\
    \\f\n\
    \\ENQ\EOT\f\STX\NUL\ENQ\DC2\ETXn\STX\b\n\
    \\f\n\
    \\ENQ\EOT\f\STX\NUL\SOH\DC2\ETXn\t\r\n\
    \\f\n\
    \\ENQ\EOT\f\STX\NUL\ETX\DC2\ETXn\DLE\DC1\n\
    \\v\n\
    \\EOT\EOT\f\STX\SOH\DC2\ETXo\STX\GS\n\
    \\f\n\
    \\ENQ\EOT\f\STX\SOH\ACK\DC2\ETXo\STX\f\n\
    \\f\n\
    \\ENQ\EOT\f\STX\SOH\SOH\DC2\ETXo\r\CAN\n\
    \\f\n\
    \\ENQ\EOT\f\STX\SOH\ETX\DC2\ETXo\ESC\FS\n\
    \\n\
    \\n\
    \\STX\EOT\r\DC2\EOTr\NULu\SOH\n\
    \\n\
    \\n\
    \\ETX\EOT\r\SOH\DC2\ETXr\b\DC2\n\
    \\v\n\
    \\EOT\EOT\r\STX\NUL\DC2\ETXs\STX\DC2\n\
    \\f\n\
    \\ENQ\EOT\r\STX\NUL\ENQ\DC2\ETXs\STX\b\n\
    \\f\n\
    \\ENQ\EOT\r\STX\NUL\SOH\DC2\ETXs\t\r\n\
    \\f\n\
    \\ENQ\EOT\r\STX\NUL\ETX\DC2\ETXs\DLE\DC1\n\
    \\v\n\
    \\EOT\EOT\r\STX\SOH\DC2\ETXt\STX\GS\n\
    \\f\n\
    \\ENQ\EOT\r\STX\SOH\ACK\DC2\ETXt\STX\f\n\
    \\f\n\
    \\ENQ\EOT\r\STX\SOH\SOH\DC2\ETXt\r\CAN\n\
    \\f\n\
    \\ENQ\EOT\r\STX\SOH\ETX\DC2\ETXt\ESC\FS\n\
    \\n\
    \\n\
    \\STX\EOT\SO\DC2\EOTw\NULz\SOH\n\
    \\n\
    \\n\
    \\ETX\EOT\SO\SOH\DC2\ETXw\b\DC1\n\
    \\v\n\
    \\EOT\EOT\SO\STX\NUL\DC2\ETXx\STX\DC2\n\
    \\f\n\
    \\ENQ\EOT\SO\STX\NUL\ENQ\DC2\ETXx\STX\b\n\
    \\f\n\
    \\ENQ\EOT\SO\STX\NUL\SOH\DC2\ETXx\t\r\n\
    \\f\n\
    \\ENQ\EOT\SO\STX\NUL\ETX\DC2\ETXx\DLE\DC1\n\
    \\v\n\
    \\EOT\EOT\SO\STX\SOH\DC2\ETXy\STX\GS\n\
    \\f\n\
    \\ENQ\EOT\SO\STX\SOH\ACK\DC2\ETXy\STX\f\n\
    \\f\n\
    \\ENQ\EOT\SO\STX\SOH\SOH\DC2\ETXy\r\CAN\n\
    \\f\n\
    \\ENQ\EOT\SO\STX\SOH\ETX\DC2\ETXy\ESC\FS\n\
    \\n\
    \\n\
    \\STX\EOT\SI\DC2\EOT|\NUL\DEL\SOH\n\
    \\n\
    \\n\
    \\ETX\EOT\SI\SOH\DC2\ETX|\b\NAK\n\
    \\v\n\
    \\EOT\EOT\SI\STX\NUL\DC2\ETX}\STX\DC2\n\
    \\f\n\
    \\ENQ\EOT\SI\STX\NUL\ENQ\DC2\ETX}\STX\b\n\
    \\f\n\
    \\ENQ\EOT\SI\STX\NUL\SOH\DC2\ETX}\t\r\n\
    \\f\n\
    \\ENQ\EOT\SI\STX\NUL\ETX\DC2\ETX}\DLE\DC1\n\
    \\v\n\
    \\EOT\EOT\SI\STX\SOH\DC2\ETX~\STX\GS\n\
    \\f\n\
    \\ENQ\EOT\SI\STX\SOH\ACK\DC2\ETX~\STX\f\n\
    \\f\n\
    \\ENQ\EOT\SI\STX\SOH\SOH\DC2\ETX~\r\CAN\n\
    \\f\n\
    \\ENQ\EOT\SI\STX\SOH\ETX\DC2\ETX~\ESC\FS\n\
    \x\n\
    \\STX\EOT\DLE\DC2\ACK\138\SOH\NUL\143\SOH\SOH\SUBj Type definition\n\
    \\n\
    \data TyDef = TyDef\n\
    \{ tyDefName :: TyConName,\n\
    \tyDefKind :: TKind,\n\
    \tyDefBody :: TyBody\n\
    \}\n\
    \\n\
    \\n\
    \\v\n\
    \\ETX\EOT\DLE\SOH\DC2\EOT\138\SOH\b\SYN\n\
    \\f\n\
    \\EOT\EOT\DLE\STX\NUL\DC2\EOT\139\SOH\STX\FS\n\
    \\r\n\
    \\ENQ\EOT\DLE\STX\NUL\ACK\DC2\EOT\139\SOH\STX\r\n\
    \\r\n\
    \\ENQ\EOT\DLE\STX\NUL\SOH\DC2\EOT\139\SOH\SO\ETB\n\
    \\r\n\
    \\ENQ\EOT\DLE\STX\NUL\ETX\DC2\EOT\139\SOH\SUB\ESC\n\
    \\f\n\
    \\EOT\EOT\DLE\STX\SOH\DC2\EOT\140\SOH\STX\NAK\n\
    \\r\n\
    \\ENQ\EOT\DLE\STX\SOH\ACK\DC2\EOT\140\SOH\STX\ACK\n\
    \\r\n\
    \\ENQ\EOT\DLE\STX\SOH\SOH\DC2\EOT\140\SOH\a\DLE\n\
    \\r\n\
    \\ENQ\EOT\DLE\STX\SOH\ETX\DC2\EOT\140\SOH\DC3\DC4\n\
    \\f\n\
    \\EOT\EOT\DLE\STX\STX\DC2\EOT\141\SOH\STX\EM\n\
    \\r\n\
    \\ENQ\EOT\DLE\STX\STX\ACK\DC2\EOT\141\SOH\STX\n\
    \\n\
    \\r\n\
    \\ENQ\EOT\DLE\STX\STX\SOH\DC2\EOT\141\SOH\v\DC4\n\
    \\r\n\
    \\ENQ\EOT\DLE\STX\STX\ETX\DC2\EOT\141\SOH\ETB\CAN\n\
    \\f\n\
    \\EOT\EOT\DLE\STX\ETX\DC2\EOT\142\SOH\STX\GS\n\
    \\r\n\
    \\ENQ\EOT\DLE\STX\ETX\ACK\DC2\EOT\142\SOH\STX\f\n\
    \\r\n\
    \\ENQ\EOT\DLE\STX\ETX\SOH\DC2\EOT\142\SOH\r\CAN\n\
    \\r\n\
    \\ENQ\EOT\DLE\STX\ETX\ETX\DC2\EOT\142\SOH\ESC\FS\n\
    \N\n\
    \\STX\EOT\DC1\DC2\ACK\149\SOH\NUL\155\SOH\SOH\SUB@ Type body\n\
    \\n\
    \data TyBody = Opaque | Sum (Map ConstrName Product)\n\
    \\n\
    \\v\n\
    \\ETX\EOT\DC1\SOH\DC2\EOT\149\SOH\b\DLE\n\
    \\SO\n\
    \\EOT\EOT\DC1\b\NUL\DC2\ACK\150\SOH\STX\153\SOH\ETX\n\
    \\r\n\
    \\ENQ\EOT\DC1\b\NUL\SOH\DC2\EOT\150\SOH\b\f\n\
    \\f\n\
    \\EOT\EOT\DC1\STX\NUL\DC2\EOT\151\SOH\EOT\SYN\n\
    \\r\n\
    \\ENQ\EOT\DC1\STX\NUL\ACK\DC2\EOT\151\SOH\EOT\n\
    \\n\
    \\r\n\
    \\ENQ\EOT\DC1\STX\NUL\SOH\DC2\EOT\151\SOH\v\DC1\n\
    \\r\n\
    \\ENQ\EOT\DC1\STX\NUL\ETX\DC2\EOT\151\SOH\DC4\NAK\n\
    \\f\n\
    \\EOT\EOT\DC1\STX\SOH\DC2\EOT\152\SOH\EOT\DLE\n\
    \\r\n\
    \\ENQ\EOT\DC1\STX\SOH\ACK\DC2\EOT\152\SOH\EOT\a\n\
    \\r\n\
    \\ENQ\EOT\DC1\STX\SOH\SOH\DC2\EOT\152\SOH\b\v\n\
    \\r\n\
    \\ENQ\EOT\DC1\STX\SOH\ETX\DC2\EOT\152\SOH\SO\SI\n\
    \\f\n\
    \\EOT\EOT\DC1\STX\STX\DC2\EOT\154\SOH\STX\GS\n\
    \\r\n\
    \\ENQ\EOT\DC1\STX\STX\ACK\DC2\EOT\154\SOH\STX\f\n\
    \\r\n\
    \\ENQ\EOT\DC1\STX\STX\SOH\DC2\EOT\154\SOH\r\CAN\n\
    \\r\n\
    \\ENQ\EOT\DC1\STX\STX\ETX\DC2\EOT\154\SOH\ESC\FS\n\
    \\f\n\
    \\STX\EOT\DC2\DC2\ACK\157\SOH\NUL\159\SOH\SOH\n\
    \\v\n\
    \\ETX\EOT\DC2\SOH\DC2\EOT\157\SOH\b\SO\n\
    \\f\n\
    \\EOT\EOT\DC2\STX\NUL\DC2\EOT\158\SOH\STX\GS\n\
    \\r\n\
    \\ENQ\EOT\DC2\STX\NUL\ACK\DC2\EOT\158\SOH\STX\f\n\
    \\r\n\
    \\ENQ\EOT\DC2\STX\NUL\SOH\DC2\EOT\158\SOH\r\CAN\n\
    \\r\n\
    \\ENQ\EOT\DC2\STX\NUL\ETX\DC2\EOT\158\SOH\ESC\FS\n\
    \\f\n\
    \\STX\EOT\DC3\DC2\ACK\161\SOH\NUL\165\SOH\SOH\n\
    \\v\n\
    \\ETX\EOT\DC3\SOH\DC2\EOT\161\SOH\b\v\n\
    \\132\SOH\n\
    \\EOT\EOT\DC3\STX\NUL\DC2\EOT\163\SOH\STX(\SUB! Should be <ConstrName, Product>\n\
    \\"S NOTE(lameness): Key in map fields cannot be float/double, bytes or message types.\n\
    \\n\
    \\r\n\
    \\ENQ\EOT\DC3\STX\NUL\ACK\DC2\EOT\163\SOH\STX\SYN\n\
    \\r\n\
    \\ENQ\EOT\DC3\STX\NUL\SOH\DC2\EOT\163\SOH\ETB#\n\
    \\r\n\
    \\ENQ\EOT\DC3\STX\NUL\ETX\DC2\EOT\163\SOH&'\n\
    \\f\n\
    \\EOT\EOT\DC3\STX\SOH\DC2\EOT\164\SOH\STX\GS\n\
    \\r\n\
    \\ENQ\EOT\DC3\STX\SOH\ACK\DC2\EOT\164\SOH\STX\f\n\
    \\r\n\
    \\ENQ\EOT\DC3\STX\SOH\SOH\DC2\EOT\164\SOH\r\CAN\n\
    \\r\n\
    \\ENQ\EOT\DC3\STX\SOH\ETX\DC2\EOT\164\SOH\ESC\FS\n\
    \N\n\
    \\STX\EOT\DC4\DC2\ACK\168\SOH\NUL\175\SOH\SOH\SUB@ data Product = Record (Map FieldName Ty) | NTuple [Ty] | Empty\n\
    \\n\
    \\v\n\
    \\ETX\EOT\DC4\SOH\DC2\EOT\168\SOH\b\SI\n\
    \\SO\n\
    \\EOT\EOT\DC4\b\NUL\DC2\ACK\169\SOH\STX\173\SOH\ETX\n\
    \\r\n\
    \\ENQ\EOT\DC4\b\NUL\SOH\DC2\EOT\169\SOH\b\SI\n\
    \\f\n\
    \\EOT\EOT\DC4\STX\NUL\DC2\EOT\170\SOH\EOT\DC4\n\
    \\r\n\
    \\ENQ\EOT\DC4\STX\NUL\ACK\DC2\EOT\170\SOH\EOT\t\n\
    \\r\n\
    \\ENQ\EOT\DC4\STX\NUL\SOH\DC2\EOT\170\SOH\n\
    \\SI\n\
    \\r\n\
    \\ENQ\EOT\DC4\STX\NUL\ETX\DC2\EOT\170\SOH\DC2\DC3\n\
    \\f\n\
    \\EOT\EOT\DC4\STX\SOH\DC2\EOT\171\SOH\EOT\SYN\n\
    \\r\n\
    \\ENQ\EOT\DC4\STX\SOH\ACK\DC2\EOT\171\SOH\EOT\n\
    \\n\
    \\r\n\
    \\ENQ\EOT\DC4\STX\SOH\SOH\DC2\EOT\171\SOH\v\DC1\n\
    \\r\n\
    \\ENQ\EOT\DC4\STX\SOH\ETX\DC2\EOT\171\SOH\DC4\NAK\n\
    \\f\n\
    \\EOT\EOT\DC4\STX\STX\DC2\EOT\172\SOH\EOT\SYN\n\
    \\r\n\
    \\ENQ\EOT\DC4\STX\STX\ACK\DC2\EOT\172\SOH\EOT\n\
    \\n\
    \\r\n\
    \\ENQ\EOT\DC4\STX\STX\SOH\DC2\EOT\172\SOH\v\DC1\n\
    \\r\n\
    \\ENQ\EOT\DC4\STX\STX\ETX\DC2\EOT\172\SOH\DC4\NAK\n\
    \\f\n\
    \\EOT\EOT\DC4\STX\ETX\DC2\EOT\174\SOH\STX\GS\n\
    \\r\n\
    \\ENQ\EOT\DC4\STX\ETX\ACK\DC2\EOT\174\SOH\STX\f\n\
    \\r\n\
    \\ENQ\EOT\DC4\STX\ETX\SOH\DC2\EOT\174\SOH\r\CAN\n\
    \\r\n\
    \\ENQ\EOT\DC4\STX\ETX\ETX\DC2\EOT\174\SOH\ESC\FS\n\
    \\f\n\
    \\STX\EOT\NAK\DC2\ACK\177\SOH\NUL\179\SOH\SOH\n\
    \\v\n\
    \\ETX\EOT\NAK\SOH\DC2\EOT\177\SOH\b\r\n\
    \\f\n\
    \\EOT\EOT\NAK\STX\NUL\DC2\EOT\178\SOH\STX\GS\n\
    \\r\n\
    \\ENQ\EOT\NAK\STX\NUL\ACK\DC2\EOT\178\SOH\STX\f\n\
    \\r\n\
    \\ENQ\EOT\NAK\STX\NUL\SOH\DC2\EOT\178\SOH\r\CAN\n\
    \\r\n\
    \\ENQ\EOT\NAK\STX\NUL\ETX\DC2\EOT\178\SOH\ESC\FS\n\
    \\f\n\
    \\STX\EOT\SYN\DC2\ACK\181\SOH\NUL\185\SOH\SOH\n\
    \\v\n\
    \\ETX\EOT\SYN\SOH\DC2\EOT\181\SOH\b\SO\n\
    \\131\SOH\n\
    \\EOT\EOT\SYN\STX\NUL\DC2\EOT\183\SOH\STX\US\SUB  Should be map<FieldName, Type>\n\
    \\"S NOTE(lameness): Key in map fields cannot be float/double, bytes or message types.\n\
    \\n\
    \\r\n\
    \\ENQ\EOT\SYN\STX\NUL\ACK\DC2\EOT\183\SOH\STX\DC3\n\
    \\r\n\
    \\ENQ\EOT\SYN\STX\NUL\SOH\DC2\EOT\183\SOH\DC4\SUB\n\
    \\r\n\
    \\ENQ\EOT\SYN\STX\NUL\ETX\DC2\EOT\183\SOH\GS\RS\n\
    \\f\n\
    \\EOT\EOT\SYN\STX\SOH\DC2\EOT\184\SOH\STX\RS\n\
    \\r\n\
    \\ENQ\EOT\SYN\STX\SOH\ACK\DC2\EOT\184\SOH\STX\f\n\
    \\r\n\
    \\ENQ\EOT\SYN\STX\SOH\SOH\DC2\EOT\184\SOH\r\EM\n\
    \\r\n\
    \\ENQ\EOT\SYN\STX\SOH\ETX\DC2\EOT\184\SOH\FS\GS\n\
    \\f\n\
    \\STX\EOT\ETB\DC2\ACK\187\SOH\NUL\190\SOH\SOH\n\
    \\v\n\
    \\ETX\EOT\ETB\SOH\DC2\EOT\187\SOH\b\SO\n\
    \\f\n\
    \\EOT\EOT\ETB\STX\NUL\DC2\EOT\188\SOH\STX\ESC\n\
    \\r\n\
    \\ENQ\EOT\ETB\STX\NUL\EOT\DC2\EOT\188\SOH\STX\n\
    \\n\
    \\r\n\
    \\ENQ\EOT\ETB\STX\NUL\ACK\DC2\EOT\188\SOH\v\SI\n\
    \\r\n\
    \\ENQ\EOT\ETB\STX\NUL\SOH\DC2\EOT\188\SOH\DLE\SYN\n\
    \\r\n\
    \\ENQ\EOT\ETB\STX\NUL\ETX\DC2\EOT\188\SOH\EM\SUB\n\
    \\f\n\
    \\EOT\EOT\ETB\STX\SOH\DC2\EOT\189\SOH\STX\GS\n\
    \\r\n\
    \\ENQ\EOT\ETB\STX\SOH\ACK\DC2\EOT\189\SOH\STX\f\n\
    \\r\n\
    \\ENQ\EOT\ETB\STX\SOH\SOH\DC2\EOT\189\SOH\r\CAN\n\
    \\r\n\
    \\ENQ\EOT\ETB\STX\SOH\ETX\DC2\EOT\189\SOH\ESC\FS\n\
    \\192\STX\n\
    \\STX\EOT\CAN\DC2\ACK\211\SOH\NUL\215\SOH\SOH2\177\STX Typeclasses and instances\n\
    \\n\
    \data TypeclassDef = TypeclassDef {\n\
    \tcName :: TypeclassName,\n\
    \tcDocumentation :: Text\n\
    \}\n\
    \\n\
    \data InstanceClause = InstanceClause\n\
    \{ icClassName :: TypeclassName,\n\
    \icHead :: Ty,\n\
    \icBody :: [Constraint]\n\
    \}\n\
    \\n\
    \data Constraint = Constraint\n\
    \{ cClassName :: TypeclassName,\n\
    \cArguments :: [Ty]\n\
    \}\n\
    \\n\
    \\v\n\
    \\ETX\EOT\CAN\SOH\DC2\EOT\211\SOH\b\DC4\n\
    \\f\n\
    \\EOT\EOT\CAN\STX\NUL\DC2\EOT\212\SOH\STX#\n\
    \\r\n\
    \\ENQ\EOT\CAN\STX\NUL\ACK\DC2\EOT\212\SOH\STX\SI\n\
    \\r\n\
    \\ENQ\EOT\CAN\STX\NUL\SOH\DC2\EOT\212\SOH\DLE\RS\n\
    \\r\n\
    \\ENQ\EOT\CAN\STX\NUL\ETX\DC2\EOT\212\SOH!\"\n\
    \\f\n\
    \\EOT\EOT\CAN\STX\SOH\DC2\EOT\213\SOH\STX\ESC\n\
    \\r\n\
    \\ENQ\EOT\CAN\STX\SOH\ENQ\DC2\EOT\213\SOH\STX\b\n\
    \\r\n\
    \\ENQ\EOT\CAN\STX\SOH\SOH\DC2\EOT\213\SOH\t\SYN\n\
    \\r\n\
    \\ENQ\EOT\CAN\STX\SOH\ETX\DC2\EOT\213\SOH\EM\SUB\n\
    \\f\n\
    \\EOT\EOT\CAN\STX\STX\DC2\EOT\214\SOH\STX\GS\n\
    \\r\n\
    \\ENQ\EOT\CAN\STX\STX\ACK\DC2\EOT\214\SOH\STX\f\n\
    \\r\n\
    \\ENQ\EOT\CAN\STX\STX\SOH\DC2\EOT\214\SOH\r\CAN\n\
    \\r\n\
    \\ENQ\EOT\CAN\STX\STX\ETX\DC2\EOT\214\SOH\ESC\FS\n\
    \\f\n\
    \\STX\EOT\EM\DC2\ACK\217\SOH\NUL\222\SOH\SOH\n\
    \\v\n\
    \\ETX\EOT\EM\SOH\DC2\EOT\217\SOH\b\SYN\n\
    \\f\n\
    \\EOT\EOT\EM\STX\NUL\DC2\EOT\218\SOH\STX#\n\
    \\r\n\
    \\ENQ\EOT\EM\STX\NUL\ACK\DC2\EOT\218\SOH\STX\SI\n\
    \\r\n\
    \\ENQ\EOT\EM\STX\NUL\SOH\DC2\EOT\218\SOH\DLE\RS\n\
    \\r\n\
    \\ENQ\EOT\EM\STX\NUL\ETX\DC2\EOT\218\SOH!\"\n\
    \\f\n\
    \\EOT\EOT\EM\STX\SOH\DC2\EOT\219\SOH\STX\DLE\n\
    \\r\n\
    \\ENQ\EOT\EM\STX\SOH\ACK\DC2\EOT\219\SOH\STX\ACK\n\
    \\r\n\
    \\ENQ\EOT\EM\STX\SOH\SOH\DC2\EOT\219\SOH\a\v\n\
    \\r\n\
    \\ENQ\EOT\EM\STX\SOH\ETX\DC2\EOT\219\SOH\SO\SI\n\
    \\f\n\
    \\EOT\EOT\EM\STX\STX\DC2\EOT\220\SOH\STX&\n\
    \\r\n\
    \\ENQ\EOT\EM\STX\STX\EOT\DC2\EOT\220\SOH\STX\n\
    \\n\
    \\r\n\
    \\ENQ\EOT\EM\STX\STX\ACK\DC2\EOT\220\SOH\v\NAK\n\
    \\r\n\
    \\ENQ\EOT\EM\STX\STX\SOH\DC2\EOT\220\SOH\SYN!\n\
    \\r\n\
    \\ENQ\EOT\EM\STX\STX\ETX\DC2\EOT\220\SOH$%\n\
    \\f\n\
    \\EOT\EOT\EM\STX\ETX\DC2\EOT\221\SOH\STX\GS\n\
    \\r\n\
    \\ENQ\EOT\EM\STX\ETX\ACK\DC2\EOT\221\SOH\STX\f\n\
    \\r\n\
    \\ENQ\EOT\EM\STX\ETX\SOH\DC2\EOT\221\SOH\r\CAN\n\
    \\r\n\
    \\ENQ\EOT\EM\STX\ETX\ETX\DC2\EOT\221\SOH\ESC\FS\n\
    \\f\n\
    \\STX\EOT\SUB\DC2\ACK\224\SOH\NUL\229\SOH\SOH\n\
    \\v\n\
    \\ETX\EOT\SUB\SOH\DC2\EOT\224\SOH\b\DC2\n\
    \0\n\
    \\EOT\EOT\SUB\STX\NUL\DC2\EOT\226\SOH\STX#\SUB\" This is naive and probably wrong\n\
    \\n\
    \\r\n\
    \\ENQ\EOT\SUB\STX\NUL\ACK\DC2\EOT\226\SOH\STX\SI\n\
    \\r\n\
    \\ENQ\EOT\SUB\STX\NUL\SOH\DC2\EOT\226\SOH\DLE\RS\n\
    \\r\n\
    \\ENQ\EOT\SUB\STX\NUL\ETX\DC2\EOT\226\SOH!\"\n\
    \\f\n\
    \\EOT\EOT\SUB\STX\SOH\DC2\EOT\227\SOH\STX\RS\n\
    \\r\n\
    \\ENQ\EOT\SUB\STX\SOH\EOT\DC2\EOT\227\SOH\STX\n\
    \\n\
    \\r\n\
    \\ENQ\EOT\SUB\STX\SOH\ACK\DC2\EOT\227\SOH\v\SI\n\
    \\r\n\
    \\ENQ\EOT\SUB\STX\SOH\SOH\DC2\EOT\227\SOH\DLE\EM\n\
    \\r\n\
    \\ENQ\EOT\SUB\STX\SOH\ETX\DC2\EOT\227\SOH\FS\GS\n\
    \\f\n\
    \\EOT\EOT\SUB\STX\STX\DC2\EOT\228\SOH\STX\GS\n\
    \\r\n\
    \\ENQ\EOT\SUB\STX\STX\ACK\DC2\EOT\228\SOH\STX\f\n\
    \\r\n\
    \\ENQ\EOT\SUB\STX\STX\SOH\DC2\EOT\228\SOH\r\CAN\n\
    \\r\n\
    \\ENQ\EOT\SUB\STX\STX\ETX\DC2\EOT\228\SOH\ESC\FS\n\
    \\136\SOH\n\
    \\STX\EOT\ESC\DC2\ACK\241\SOH\NUL\246\SOH\SOH2z Module\n\
    \\n\
    \data Module = Module  { \n\
    \moduleName :: ModuleName,\n\
    \moduleDefs :: [TyDef],\n\
    \moduleInstances :: [InstanceClause]\n\
    \}\n\
    \\n\
    \\n\
    \\v\n\
    \\ETX\EOT\ESC\SOH\DC2\EOT\241\SOH\b\SO\n\
    \\f\n\
    \\EOT\EOT\ESC\STX\NUL\DC2\EOT\242\SOH\STX\GS\n\
    \\r\n\
    \\ENQ\EOT\ESC\STX\NUL\ACK\DC2\EOT\242\SOH\STX\f\n\
    \\r\n\
    \\ENQ\EOT\ESC\STX\NUL\SOH\DC2\EOT\242\SOH\r\CAN\n\
    \\r\n\
    \\ENQ\EOT\ESC\STX\NUL\ETX\DC2\EOT\242\SOH\ESC\FS\n\
    \\f\n\
    \\EOT\EOT\ESC\STX\SOH\DC2\EOT\243\SOH\STX(\n\
    \\r\n\
    \\ENQ\EOT\ESC\STX\SOH\EOT\DC2\EOT\243\SOH\STX\n\
    \\n\
    \\r\n\
    \\ENQ\EOT\ESC\STX\SOH\ACK\DC2\EOT\243\SOH\v\EM\n\
    \\r\n\
    \\ENQ\EOT\ESC\STX\SOH\SOH\DC2\EOT\243\SOH\SUB#\n\
    \\r\n\
    \\ENQ\EOT\ESC\STX\SOH\ETX\DC2\EOT\243\SOH&'\n\
    \\f\n\
    \\EOT\EOT\ESC\STX\STX\DC2\EOT\244\SOH\STX(\n\
    \\r\n\
    \\ENQ\EOT\ESC\STX\STX\EOT\DC2\EOT\244\SOH\STX\n\
    \\n\
    \\r\n\
    \\ENQ\EOT\ESC\STX\STX\ACK\DC2\EOT\244\SOH\v\EM\n\
    \\r\n\
    \\ENQ\EOT\ESC\STX\STX\SOH\DC2\EOT\244\SOH\SUB#\n\
    \\r\n\
    \\ENQ\EOT\ESC\STX\STX\ETX\DC2\EOT\244\SOH&'\n\
    \\f\n\
    \\EOT\EOT\ESC\STX\ETX\DC2\EOT\245\SOH\STX\GS\n\
    \\r\n\
    \\ENQ\EOT\ESC\STX\ETX\ACK\DC2\EOT\245\SOH\STX\f\n\
    \\r\n\
    \\ENQ\EOT\ESC\STX\ETX\SOH\DC2\EOT\245\SOH\r\CAN\n\
    \\r\n\
    \\ENQ\EOT\ESC\STX\ETX\ETX\DC2\EOT\245\SOH\ESC\FSb\ACKproto3"