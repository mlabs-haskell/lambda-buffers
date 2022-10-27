{- This file was auto-generated from lambdabuffers-source-diag.proto by the proto-lens-protoc program. -}
{-# LANGUAGE ScopedTypeVariables, DataKinds, TypeFamilies, UndecidableInstances, GeneralizedNewtypeDeriving, MultiParamTypeClasses, FlexibleContexts, FlexibleInstances, PatternSynonyms, MagicHash, NoImplicitPrelude, DataKinds, BangPatterns, TypeApplications, OverloadedStrings, DerivingStrategies#-}
{-# OPTIONS_GHC -Wno-unused-imports#-}
{-# OPTIONS_GHC -Wno-duplicate-exports#-}
{-# OPTIONS_GHC -Wno-dodgy-exports#-}
module Proto.LambdabuffersSourceDiag (
        Diagnostic(), Diagnostic'Level(..), Diagnostic'Level(),
        Diagnostic'Level'UnrecognizedValue, Msg()
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
import qualified Proto.LambdabuffersSource
{- | Fields :
     
         * 'Proto.LambdabuffersSourceDiag_Fields.level' @:: Lens' Diagnostic Diagnostic'Level@
         * 'Proto.LambdabuffersSourceDiag_Fields.msg' @:: Lens' Diagnostic Msg@
         * 'Proto.LambdabuffersSourceDiag_Fields.maybe'msg' @:: Lens' Diagnostic (Prelude.Maybe Msg)@ -}
data Diagnostic
  = Diagnostic'_constructor {_Diagnostic'level :: !Diagnostic'Level,
                             _Diagnostic'msg :: !(Prelude.Maybe Msg),
                             _Diagnostic'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving stock (Prelude.Eq, Prelude.Ord)
instance Prelude.Show Diagnostic where
  showsPrec _ __x __s
    = Prelude.showChar
        '{'
        (Prelude.showString
           (Data.ProtoLens.showMessageShort __x) (Prelude.showChar '}' __s))
instance Data.ProtoLens.Field.HasField Diagnostic "level" Diagnostic'Level where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _Diagnostic'level (\ x__ y__ -> x__ {_Diagnostic'level = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField Diagnostic "msg" Msg where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _Diagnostic'msg (\ x__ y__ -> x__ {_Diagnostic'msg = y__}))
        (Data.ProtoLens.maybeLens Data.ProtoLens.defMessage)
instance Data.ProtoLens.Field.HasField Diagnostic "maybe'msg" (Prelude.Maybe Msg) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _Diagnostic'msg (\ x__ y__ -> x__ {_Diagnostic'msg = y__}))
        Prelude.id
instance Data.ProtoLens.Message Diagnostic where
  messageName _
    = Data.Text.pack "lambdabuffers.source.diag.Diagnostic"
  packedMessageDescriptor _
    = "\n\
      \\n\
      \Diagnostic\DC2A\n\
      \\ENQlevel\CAN\SOH \SOH(\SO2+.lambdabuffers.source.diag.Diagnostic.LevelR\ENQlevel\DC20\n\
      \\ETXmsg\CAN\STX \SOH(\v2\RS.lambdabuffers.source.diag.MsgR\ETXmsg\"3\n\
      \\ENQLevel\DC2\b\n\
      \\EOTNONE\DLE\NUL\DC2\b\n\
      \\EOTINFO\DLE\SOH\DC2\v\n\
      \\aWARNING\DLE\STX\DC2\t\n\
      \\ENQERROR\DLE\ETX"
  packedFileDescriptor _ = packedFileDescriptor
  fieldsByTag
    = let
        level__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "level"
              (Data.ProtoLens.ScalarField Data.ProtoLens.EnumField ::
                 Data.ProtoLens.FieldTypeDescriptor Diagnostic'Level)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional (Data.ProtoLens.Field.field @"level")) ::
              Data.ProtoLens.FieldDescriptor Diagnostic
        msg__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "msg"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor Msg)
              (Data.ProtoLens.OptionalField
                 (Data.ProtoLens.Field.field @"maybe'msg")) ::
              Data.ProtoLens.FieldDescriptor Diagnostic
      in
        Data.Map.fromList
          [(Data.ProtoLens.Tag 1, level__field_descriptor),
           (Data.ProtoLens.Tag 2, msg__field_descriptor)]
  unknownFields
    = Lens.Family2.Unchecked.lens
        _Diagnostic'_unknownFields
        (\ x__ y__ -> x__ {_Diagnostic'_unknownFields = y__})
  defMessage
    = Diagnostic'_constructor
        {_Diagnostic'level = Data.ProtoLens.fieldDefault,
         _Diagnostic'msg = Prelude.Nothing, _Diagnostic'_unknownFields = []}
  parseMessage
    = let
        loop ::
          Diagnostic -> Data.ProtoLens.Encoding.Bytes.Parser Diagnostic
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
                                          Prelude.toEnum
                                          (Prelude.fmap
                                             Prelude.fromIntegral
                                             Data.ProtoLens.Encoding.Bytes.getVarInt))
                                       "level"
                                loop (Lens.Family2.set (Data.ProtoLens.Field.field @"level") y x)
                        18
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                           Data.ProtoLens.Encoding.Bytes.isolate
                                             (Prelude.fromIntegral len) Data.ProtoLens.parseMessage)
                                       "msg"
                                loop (Lens.Family2.set (Data.ProtoLens.Field.field @"msg") y x)
                        wire
                          -> do !y <- Data.ProtoLens.Encoding.Wire.parseTaggedValueFromWire
                                        wire
                                loop
                                  (Lens.Family2.over
                                     Data.ProtoLens.unknownFields (\ !t -> (:) y t) x)
      in
        (Data.ProtoLens.Encoding.Bytes.<?>)
          (do loop Data.ProtoLens.defMessage) "Diagnostic"
  buildMessage
    = \ _x
        -> (Data.Monoid.<>)
             (let
                _v = Lens.Family2.view (Data.ProtoLens.Field.field @"level") _x
              in
                if (Prelude.==) _v Data.ProtoLens.fieldDefault then
                    Data.Monoid.mempty
                else
                    (Data.Monoid.<>)
                      (Data.ProtoLens.Encoding.Bytes.putVarInt 8)
                      ((Prelude..)
                         ((Prelude..)
                            Data.ProtoLens.Encoding.Bytes.putVarInt Prelude.fromIntegral)
                         Prelude.fromEnum _v))
             ((Data.Monoid.<>)
                (case
                     Lens.Family2.view (Data.ProtoLens.Field.field @"maybe'msg") _x
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
instance Control.DeepSeq.NFData Diagnostic where
  rnf
    = \ x__
        -> Control.DeepSeq.deepseq
             (_Diagnostic'_unknownFields x__)
             (Control.DeepSeq.deepseq
                (_Diagnostic'level x__)
                (Control.DeepSeq.deepseq (_Diagnostic'msg x__) ()))
newtype Diagnostic'Level'UnrecognizedValue
  = Diagnostic'Level'UnrecognizedValue Data.Int.Int32
  deriving stock (Prelude.Eq, Prelude.Ord, Prelude.Show)
data Diagnostic'Level
  = Diagnostic'NONE |
    Diagnostic'INFO |
    Diagnostic'WARNING |
    Diagnostic'ERROR |
    Diagnostic'Level'Unrecognized !Diagnostic'Level'UnrecognizedValue
  deriving stock (Prelude.Show, Prelude.Eq, Prelude.Ord)
instance Data.ProtoLens.MessageEnum Diagnostic'Level where
  maybeToEnum 0 = Prelude.Just Diagnostic'NONE
  maybeToEnum 1 = Prelude.Just Diagnostic'INFO
  maybeToEnum 2 = Prelude.Just Diagnostic'WARNING
  maybeToEnum 3 = Prelude.Just Diagnostic'ERROR
  maybeToEnum k
    = Prelude.Just
        (Diagnostic'Level'Unrecognized
           (Diagnostic'Level'UnrecognizedValue (Prelude.fromIntegral k)))
  showEnum Diagnostic'NONE = "NONE"
  showEnum Diagnostic'INFO = "INFO"
  showEnum Diagnostic'WARNING = "WARNING"
  showEnum Diagnostic'ERROR = "ERROR"
  showEnum
    (Diagnostic'Level'Unrecognized (Diagnostic'Level'UnrecognizedValue k))
    = Prelude.show k
  readEnum k
    | (Prelude.==) k "NONE" = Prelude.Just Diagnostic'NONE
    | (Prelude.==) k "INFO" = Prelude.Just Diagnostic'INFO
    | (Prelude.==) k "WARNING" = Prelude.Just Diagnostic'WARNING
    | (Prelude.==) k "ERROR" = Prelude.Just Diagnostic'ERROR
    | Prelude.otherwise
    = (Prelude.>>=) (Text.Read.readMaybe k) Data.ProtoLens.maybeToEnum
instance Prelude.Bounded Diagnostic'Level where
  minBound = Diagnostic'NONE
  maxBound = Diagnostic'ERROR
instance Prelude.Enum Diagnostic'Level where
  toEnum k__
    = Prelude.maybe
        (Prelude.error
           ((Prelude.++)
              "toEnum: unknown value for enum Level: " (Prelude.show k__)))
        Prelude.id (Data.ProtoLens.maybeToEnum k__)
  fromEnum Diagnostic'NONE = 0
  fromEnum Diagnostic'INFO = 1
  fromEnum Diagnostic'WARNING = 2
  fromEnum Diagnostic'ERROR = 3
  fromEnum
    (Diagnostic'Level'Unrecognized (Diagnostic'Level'UnrecognizedValue k))
    = Prelude.fromIntegral k
  succ Diagnostic'ERROR
    = Prelude.error
        "Diagnostic'Level.succ: bad argument Diagnostic'ERROR. This value would be out of bounds."
  succ Diagnostic'NONE = Diagnostic'INFO
  succ Diagnostic'INFO = Diagnostic'WARNING
  succ Diagnostic'WARNING = Diagnostic'ERROR
  succ (Diagnostic'Level'Unrecognized _)
    = Prelude.error
        "Diagnostic'Level.succ: bad argument: unrecognized value"
  pred Diagnostic'NONE
    = Prelude.error
        "Diagnostic'Level.pred: bad argument Diagnostic'NONE. This value would be out of bounds."
  pred Diagnostic'INFO = Diagnostic'NONE
  pred Diagnostic'WARNING = Diagnostic'INFO
  pred Diagnostic'ERROR = Diagnostic'WARNING
  pred (Diagnostic'Level'Unrecognized _)
    = Prelude.error
        "Diagnostic'Level.pred: bad argument: unrecognized value"
  enumFrom = Data.ProtoLens.Message.Enum.messageEnumFrom
  enumFromTo = Data.ProtoLens.Message.Enum.messageEnumFromTo
  enumFromThen = Data.ProtoLens.Message.Enum.messageEnumFromThen
  enumFromThenTo = Data.ProtoLens.Message.Enum.messageEnumFromThenTo
instance Data.ProtoLens.FieldDefault Diagnostic'Level where
  fieldDefault = Diagnostic'NONE
instance Control.DeepSeq.NFData Diagnostic'Level where
  rnf x__ = Prelude.seq x__ ()
{- | Fields :
     
         * 'Proto.LambdabuffersSourceDiag_Fields.sourceInfo' @:: Lens' Msg Proto.LambdabuffersSource.SourceInfo@
         * 'Proto.LambdabuffersSourceDiag_Fields.maybe'sourceInfo' @:: Lens' Msg (Prelude.Maybe Proto.LambdabuffersSource.SourceInfo)@
         * 'Proto.LambdabuffersSourceDiag_Fields.issue' @:: Lens' Msg Data.Text.Text@
         * 'Proto.LambdabuffersSourceDiag_Fields.recommendation' @:: Lens' Msg Data.Text.Text@ -}
data Msg
  = Msg'_constructor {_Msg'sourceInfo :: !(Prelude.Maybe Proto.LambdabuffersSource.SourceInfo),
                      _Msg'issue :: !Data.Text.Text,
                      _Msg'recommendation :: !Data.Text.Text,
                      _Msg'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving stock (Prelude.Eq, Prelude.Ord)
instance Prelude.Show Msg where
  showsPrec _ __x __s
    = Prelude.showChar
        '{'
        (Prelude.showString
           (Data.ProtoLens.showMessageShort __x) (Prelude.showChar '}' __s))
instance Data.ProtoLens.Field.HasField Msg "sourceInfo" Proto.LambdabuffersSource.SourceInfo where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _Msg'sourceInfo (\ x__ y__ -> x__ {_Msg'sourceInfo = y__}))
        (Data.ProtoLens.maybeLens Data.ProtoLens.defMessage)
instance Data.ProtoLens.Field.HasField Msg "maybe'sourceInfo" (Prelude.Maybe Proto.LambdabuffersSource.SourceInfo) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _Msg'sourceInfo (\ x__ y__ -> x__ {_Msg'sourceInfo = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField Msg "issue" Data.Text.Text where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _Msg'issue (\ x__ y__ -> x__ {_Msg'issue = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField Msg "recommendation" Data.Text.Text where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _Msg'recommendation (\ x__ y__ -> x__ {_Msg'recommendation = y__}))
        Prelude.id
instance Data.ProtoLens.Message Msg where
  messageName _ = Data.Text.pack "lambdabuffers.source.diag.Msg"
  packedMessageDescriptor _
    = "\n\
      \\ETXMsg\DC2A\n\
      \\vsource_info\CAN\SOH \SOH(\v2 .lambdabuffers.source.SourceInfoR\n\
      \sourceInfo\DC2\DC4\n\
      \\ENQissue\CAN\STX \SOH(\tR\ENQissue\DC2&\n\
      \\SOrecommendation\CAN\ETX \SOH(\tR\SOrecommendation"
  packedFileDescriptor _ = packedFileDescriptor
  fieldsByTag
    = let
        sourceInfo__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "source_info"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor Proto.LambdabuffersSource.SourceInfo)
              (Data.ProtoLens.OptionalField
                 (Data.ProtoLens.Field.field @"maybe'sourceInfo")) ::
              Data.ProtoLens.FieldDescriptor Msg
        issue__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "issue"
              (Data.ProtoLens.ScalarField Data.ProtoLens.StringField ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Text.Text)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional (Data.ProtoLens.Field.field @"issue")) ::
              Data.ProtoLens.FieldDescriptor Msg
        recommendation__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "recommendation"
              (Data.ProtoLens.ScalarField Data.ProtoLens.StringField ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Text.Text)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional
                 (Data.ProtoLens.Field.field @"recommendation")) ::
              Data.ProtoLens.FieldDescriptor Msg
      in
        Data.Map.fromList
          [(Data.ProtoLens.Tag 1, sourceInfo__field_descriptor),
           (Data.ProtoLens.Tag 2, issue__field_descriptor),
           (Data.ProtoLens.Tag 3, recommendation__field_descriptor)]
  unknownFields
    = Lens.Family2.Unchecked.lens
        _Msg'_unknownFields (\ x__ y__ -> x__ {_Msg'_unknownFields = y__})
  defMessage
    = Msg'_constructor
        {_Msg'sourceInfo = Prelude.Nothing,
         _Msg'issue = Data.ProtoLens.fieldDefault,
         _Msg'recommendation = Data.ProtoLens.fieldDefault,
         _Msg'_unknownFields = []}
  parseMessage
    = let
        loop :: Msg -> Data.ProtoLens.Encoding.Bytes.Parser Msg
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
                                       "issue"
                                loop (Lens.Family2.set (Data.ProtoLens.Field.field @"issue") y x)
                        26
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do value <- do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                                       Data.ProtoLens.Encoding.Bytes.getBytes
                                                         (Prelude.fromIntegral len)
                                           Data.ProtoLens.Encoding.Bytes.runEither
                                             (case Data.Text.Encoding.decodeUtf8' value of
                                                (Prelude.Left err)
                                                  -> Prelude.Left (Prelude.show err)
                                                (Prelude.Right r) -> Prelude.Right r))
                                       "recommendation"
                                loop
                                  (Lens.Family2.set
                                     (Data.ProtoLens.Field.field @"recommendation") y x)
                        wire
                          -> do !y <- Data.ProtoLens.Encoding.Wire.parseTaggedValueFromWire
                                        wire
                                loop
                                  (Lens.Family2.over
                                     Data.ProtoLens.unknownFields (\ !t -> (:) y t) x)
      in
        (Data.ProtoLens.Encoding.Bytes.<?>)
          (do loop Data.ProtoLens.defMessage) "Msg"
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
             ((Data.Monoid.<>)
                (let
                   _v = Lens.Family2.view (Data.ProtoLens.Field.field @"issue") _x
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
                   (let
                      _v
                        = Lens.Family2.view
                            (Data.ProtoLens.Field.field @"recommendation") _x
                    in
                      if (Prelude.==) _v Data.ProtoLens.fieldDefault then
                          Data.Monoid.mempty
                      else
                          (Data.Monoid.<>)
                            (Data.ProtoLens.Encoding.Bytes.putVarInt 26)
                            ((Prelude..)
                               (\ bs
                                  -> (Data.Monoid.<>)
                                       (Data.ProtoLens.Encoding.Bytes.putVarInt
                                          (Prelude.fromIntegral (Data.ByteString.length bs)))
                                       (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                               Data.Text.Encoding.encodeUtf8 _v))
                   (Data.ProtoLens.Encoding.Wire.buildFieldSet
                      (Lens.Family2.view Data.ProtoLens.unknownFields _x))))
instance Control.DeepSeq.NFData Msg where
  rnf
    = \ x__
        -> Control.DeepSeq.deepseq
             (_Msg'_unknownFields x__)
             (Control.DeepSeq.deepseq
                (_Msg'sourceInfo x__)
                (Control.DeepSeq.deepseq
                   (_Msg'issue x__)
                   (Control.DeepSeq.deepseq (_Msg'recommendation x__) ())))
packedFileDescriptor :: Data.ByteString.ByteString
packedFileDescriptor
  = "\n\
    \\USlambdabuffers-source-diag.proto\DC2\EMlambdabuffers.source.diag\SUB\SUBlambdabuffers-source.proto\"\182\SOH\n\
    \\n\
    \Diagnostic\DC2A\n\
    \\ENQlevel\CAN\SOH \SOH(\SO2+.lambdabuffers.source.diag.Diagnostic.LevelR\ENQlevel\DC20\n\
    \\ETXmsg\CAN\STX \SOH(\v2\RS.lambdabuffers.source.diag.MsgR\ETXmsg\"3\n\
    \\ENQLevel\DC2\b\n\
    \\EOTNONE\DLE\NUL\DC2\b\n\
    \\EOTINFO\DLE\SOH\DC2\v\n\
    \\aWARNING\DLE\STX\DC2\t\n\
    \\ENQERROR\DLE\ETX\"\134\SOH\n\
    \\ETXMsg\DC2A\n\
    \\vsource_info\CAN\SOH \SOH(\v2 .lambdabuffers.source.SourceInfoR\n\
    \sourceInfo\DC2\DC4\n\
    \\ENQissue\CAN\STX \SOH(\tR\ENQissue\DC2&\n\
    \\SOrecommendation\CAN\ETX \SOH(\tR\SOrecommendationJ\223\ENQ\n\
    \\ACK\DC2\EOT\SOH\NUL\SYN\SOH\n\
    \\164\SOH\n\
    \\SOH\f\DC2\ETX\SOH\NUL\DC2\SUB\153\SOH Run with: protoc --plugin=protoc-gen-haskell=`which proto-lens-protoc` --proto_path=repls --haskell_out=proto_out repls/lambdabuffers-source-diag.proto\n\
    \\n\
    \\b\n\
    \\SOH\STX\DC2\ETX\ETX\NUL\"\n\
    \\t\n\
    \\STX\ETX\NUL\DC2\ETX\ENQ\NUL$\n\
    \\n\
    \\n\
    \\STX\EOT\NUL\DC2\EOT\a\NUL\DLE\SOH\n\
    \\n\
    \\n\
    \\ETX\EOT\NUL\SOH\DC2\ETX\a\b\DC2\n\
    \\f\n\
    \\EOT\EOT\NUL\EOT\NUL\DC2\EOT\b\STX\r\ETX\n\
    \\f\n\
    \\ENQ\EOT\NUL\EOT\NUL\SOH\DC2\ETX\b\a\f\n\
    \\r\n\
    \\ACK\EOT\NUL\EOT\NUL\STX\NUL\DC2\ETX\t\EOT\r\n\
    \\SO\n\
    \\a\EOT\NUL\EOT\NUL\STX\NUL\SOH\DC2\ETX\t\EOT\b\n\
    \\SO\n\
    \\a\EOT\NUL\EOT\NUL\STX\NUL\STX\DC2\ETX\t\v\f\n\
    \\r\n\
    \\ACK\EOT\NUL\EOT\NUL\STX\SOH\DC2\ETX\n\
    \\EOT\r\n\
    \\SO\n\
    \\a\EOT\NUL\EOT\NUL\STX\SOH\SOH\DC2\ETX\n\
    \\EOT\b\n\
    \\SO\n\
    \\a\EOT\NUL\EOT\NUL\STX\SOH\STX\DC2\ETX\n\
    \\v\f\n\
    \\r\n\
    \\ACK\EOT\NUL\EOT\NUL\STX\STX\DC2\ETX\v\EOT\DLE\n\
    \\SO\n\
    \\a\EOT\NUL\EOT\NUL\STX\STX\SOH\DC2\ETX\v\EOT\v\n\
    \\SO\n\
    \\a\EOT\NUL\EOT\NUL\STX\STX\STX\DC2\ETX\v\SO\SI\n\
    \\r\n\
    \\ACK\EOT\NUL\EOT\NUL\STX\ETX\DC2\ETX\f\EOT\SO\n\
    \\SO\n\
    \\a\EOT\NUL\EOT\NUL\STX\ETX\SOH\DC2\ETX\f\EOT\t\n\
    \\SO\n\
    \\a\EOT\NUL\EOT\NUL\STX\ETX\STX\DC2\ETX\f\f\r\n\
    \\v\n\
    \\EOT\EOT\NUL\STX\NUL\DC2\ETX\SO\STX\DC2\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\NUL\ACK\DC2\ETX\SO\STX\a\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\NUL\SOH\DC2\ETX\SO\b\r\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\NUL\ETX\DC2\ETX\SO\DLE\DC1\n\
    \\v\n\
    \\EOT\EOT\NUL\STX\SOH\DC2\ETX\SI\STX\SO\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\SOH\ACK\DC2\ETX\SI\STX\ENQ\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\SOH\SOH\DC2\ETX\SI\ACK\t\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\SOH\ETX\DC2\ETX\SI\f\r\n\
    \\n\
    \\n\
    \\STX\EOT\SOH\DC2\EOT\DC2\NUL\SYN\SOH\n\
    \\n\
    \\n\
    \\ETX\EOT\SOH\SOH\DC2\ETX\DC2\b\v\n\
    \\v\n\
    \\EOT\EOT\SOH\STX\NUL\DC2\ETX\DC3\STX\GS\n\
    \\f\n\
    \\ENQ\EOT\SOH\STX\NUL\ACK\DC2\ETX\DC3\STX\f\n\
    \\f\n\
    \\ENQ\EOT\SOH\STX\NUL\SOH\DC2\ETX\DC3\r\CAN\n\
    \\f\n\
    \\ENQ\EOT\SOH\STX\NUL\ETX\DC2\ETX\DC3\ESC\FS\n\
    \\v\n\
    \\EOT\EOT\SOH\STX\SOH\DC2\ETX\DC4\STX\DC3\n\
    \\f\n\
    \\ENQ\EOT\SOH\STX\SOH\ENQ\DC2\ETX\DC4\STX\b\n\
    \\f\n\
    \\ENQ\EOT\SOH\STX\SOH\SOH\DC2\ETX\DC4\t\SO\n\
    \\f\n\
    \\ENQ\EOT\SOH\STX\SOH\ETX\DC2\ETX\DC4\DC1\DC2\n\
    \\v\n\
    \\EOT\EOT\SOH\STX\STX\DC2\ETX\NAK\STX\FS\n\
    \\f\n\
    \\ENQ\EOT\SOH\STX\STX\ENQ\DC2\ETX\NAK\STX\b\n\
    \\f\n\
    \\ENQ\EOT\SOH\STX\STX\SOH\DC2\ETX\NAK\t\ETB\n\
    \\f\n\
    \\ENQ\EOT\SOH\STX\STX\ETX\DC2\ETX\NAK\SUB\ESCb\ACKproto3"