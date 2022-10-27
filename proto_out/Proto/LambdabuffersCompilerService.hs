{- This file was auto-generated from lambdabuffers-compiler-service.proto by the proto-lens-protoc program. -}
{-# LANGUAGE ScopedTypeVariables, DataKinds, TypeFamilies, UndecidableInstances, GeneralizedNewtypeDeriving, MultiParamTypeClasses, FlexibleContexts, FlexibleInstances, PatternSynonyms, MagicHash, NoImplicitPrelude, DataKinds, BangPatterns, TypeApplications, OverloadedStrings, DerivingStrategies#-}
{-# OPTIONS_GHC -Wno-unused-imports#-}
{-# OPTIONS_GHC -Wno-duplicate-exports#-}
{-# OPTIONS_GHC -Wno-dodgy-exports#-}
module Proto.LambdabuffersCompilerService (
        LambdaBuffersCompiler(..), CompileRequest(), CompileResponse(),
        Compiled()
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
import qualified Proto.LambdabuffersSourceDiag
{- | Fields :
     
         * 'Proto.LambdabuffersCompilerService_Fields.source' @:: Lens' CompileRequest Proto.LambdabuffersSource.Source@
         * 'Proto.LambdabuffersCompilerService_Fields.maybe'source' @:: Lens' CompileRequest (Prelude.Maybe Proto.LambdabuffersSource.Source)@ -}
data CompileRequest
  = CompileRequest'_constructor {_CompileRequest'source :: !(Prelude.Maybe Proto.LambdabuffersSource.Source),
                                 _CompileRequest'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving stock (Prelude.Eq, Prelude.Ord)
instance Prelude.Show CompileRequest where
  showsPrec _ __x __s
    = Prelude.showChar
        '{'
        (Prelude.showString
           (Data.ProtoLens.showMessageShort __x) (Prelude.showChar '}' __s))
instance Data.ProtoLens.Field.HasField CompileRequest "source" Proto.LambdabuffersSource.Source where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _CompileRequest'source
           (\ x__ y__ -> x__ {_CompileRequest'source = y__}))
        (Data.ProtoLens.maybeLens Data.ProtoLens.defMessage)
instance Data.ProtoLens.Field.HasField CompileRequest "maybe'source" (Prelude.Maybe Proto.LambdabuffersSource.Source) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _CompileRequest'source
           (\ x__ y__ -> x__ {_CompileRequest'source = y__}))
        Prelude.id
instance Data.ProtoLens.Message CompileRequest where
  messageName _
    = Data.Text.pack "lambdabuffers.compiler.CompileRequest"
  packedMessageDescriptor _
    = "\n\
      \\SOCompileRequest\DC24\n\
      \\ACKsource\CAN\SOH \SOH(\v2\FS.lambdabuffers.source.SourceR\ACKsource"
  packedFileDescriptor _ = packedFileDescriptor
  fieldsByTag
    = let
        source__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "source"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor Proto.LambdabuffersSource.Source)
              (Data.ProtoLens.OptionalField
                 (Data.ProtoLens.Field.field @"maybe'source")) ::
              Data.ProtoLens.FieldDescriptor CompileRequest
      in
        Data.Map.fromList
          [(Data.ProtoLens.Tag 1, source__field_descriptor)]
  unknownFields
    = Lens.Family2.Unchecked.lens
        _CompileRequest'_unknownFields
        (\ x__ y__ -> x__ {_CompileRequest'_unknownFields = y__})
  defMessage
    = CompileRequest'_constructor
        {_CompileRequest'source = Prelude.Nothing,
         _CompileRequest'_unknownFields = []}
  parseMessage
    = let
        loop ::
          CompileRequest
          -> Data.ProtoLens.Encoding.Bytes.Parser CompileRequest
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
                                       "source"
                                loop (Lens.Family2.set (Data.ProtoLens.Field.field @"source") y x)
                        wire
                          -> do !y <- Data.ProtoLens.Encoding.Wire.parseTaggedValueFromWire
                                        wire
                                loop
                                  (Lens.Family2.over
                                     Data.ProtoLens.unknownFields (\ !t -> (:) y t) x)
      in
        (Data.ProtoLens.Encoding.Bytes.<?>)
          (do loop Data.ProtoLens.defMessage) "CompileRequest"
  buildMessage
    = \ _x
        -> (Data.Monoid.<>)
             (case
                  Lens.Family2.view (Data.ProtoLens.Field.field @"maybe'source") _x
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
instance Control.DeepSeq.NFData CompileRequest where
  rnf
    = \ x__
        -> Control.DeepSeq.deepseq
             (_CompileRequest'_unknownFields x__)
             (Control.DeepSeq.deepseq (_CompileRequest'source x__) ())
{- | Fields :
     
         * 'Proto.LambdabuffersCompilerService_Fields.diagnostics' @:: Lens' CompileResponse Proto.LambdabuffersSourceDiag.Diagnostic@
         * 'Proto.LambdabuffersCompilerService_Fields.maybe'diagnostics' @:: Lens' CompileResponse (Prelude.Maybe Proto.LambdabuffersSourceDiag.Diagnostic)@
         * 'Proto.LambdabuffersCompilerService_Fields.compiled' @:: Lens' CompileResponse Compiled@
         * 'Proto.LambdabuffersCompilerService_Fields.maybe'compiled' @:: Lens' CompileResponse (Prelude.Maybe Compiled)@ -}
data CompileResponse
  = CompileResponse'_constructor {_CompileResponse'diagnostics :: !(Prelude.Maybe Proto.LambdabuffersSourceDiag.Diagnostic),
                                  _CompileResponse'compiled :: !(Prelude.Maybe Compiled),
                                  _CompileResponse'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving stock (Prelude.Eq, Prelude.Ord)
instance Prelude.Show CompileResponse where
  showsPrec _ __x __s
    = Prelude.showChar
        '{'
        (Prelude.showString
           (Data.ProtoLens.showMessageShort __x) (Prelude.showChar '}' __s))
instance Data.ProtoLens.Field.HasField CompileResponse "diagnostics" Proto.LambdabuffersSourceDiag.Diagnostic where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _CompileResponse'diagnostics
           (\ x__ y__ -> x__ {_CompileResponse'diagnostics = y__}))
        (Data.ProtoLens.maybeLens Data.ProtoLens.defMessage)
instance Data.ProtoLens.Field.HasField CompileResponse "maybe'diagnostics" (Prelude.Maybe Proto.LambdabuffersSourceDiag.Diagnostic) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _CompileResponse'diagnostics
           (\ x__ y__ -> x__ {_CompileResponse'diagnostics = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField CompileResponse "compiled" Compiled where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _CompileResponse'compiled
           (\ x__ y__ -> x__ {_CompileResponse'compiled = y__}))
        (Data.ProtoLens.maybeLens Data.ProtoLens.defMessage)
instance Data.ProtoLens.Field.HasField CompileResponse "maybe'compiled" (Prelude.Maybe Compiled) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _CompileResponse'compiled
           (\ x__ y__ -> x__ {_CompileResponse'compiled = y__}))
        Prelude.id
instance Data.ProtoLens.Message CompileResponse where
  messageName _
    = Data.Text.pack "lambdabuffers.compiler.CompileResponse"
  packedMessageDescriptor _
    = "\n\
      \\SICompileResponse\DC2G\n\
      \\vdiagnostics\CAN\SOH \SOH(\v2%.lambdabuffers.source.diag.DiagnosticR\vdiagnostics\DC2<\n\
      \\bcompiled\CAN\STX \SOH(\v2 .lambdabuffers.compiler.CompiledR\bcompiled"
  packedFileDescriptor _ = packedFileDescriptor
  fieldsByTag
    = let
        diagnostics__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "diagnostics"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor Proto.LambdabuffersSourceDiag.Diagnostic)
              (Data.ProtoLens.OptionalField
                 (Data.ProtoLens.Field.field @"maybe'diagnostics")) ::
              Data.ProtoLens.FieldDescriptor CompileResponse
        compiled__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "compiled"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor Compiled)
              (Data.ProtoLens.OptionalField
                 (Data.ProtoLens.Field.field @"maybe'compiled")) ::
              Data.ProtoLens.FieldDescriptor CompileResponse
      in
        Data.Map.fromList
          [(Data.ProtoLens.Tag 1, diagnostics__field_descriptor),
           (Data.ProtoLens.Tag 2, compiled__field_descriptor)]
  unknownFields
    = Lens.Family2.Unchecked.lens
        _CompileResponse'_unknownFields
        (\ x__ y__ -> x__ {_CompileResponse'_unknownFields = y__})
  defMessage
    = CompileResponse'_constructor
        {_CompileResponse'diagnostics = Prelude.Nothing,
         _CompileResponse'compiled = Prelude.Nothing,
         _CompileResponse'_unknownFields = []}
  parseMessage
    = let
        loop ::
          CompileResponse
          -> Data.ProtoLens.Encoding.Bytes.Parser CompileResponse
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
                                       "diagnostics"
                                loop
                                  (Lens.Family2.set (Data.ProtoLens.Field.field @"diagnostics") y x)
                        18
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                           Data.ProtoLens.Encoding.Bytes.isolate
                                             (Prelude.fromIntegral len) Data.ProtoLens.parseMessage)
                                       "compiled"
                                loop
                                  (Lens.Family2.set (Data.ProtoLens.Field.field @"compiled") y x)
                        wire
                          -> do !y <- Data.ProtoLens.Encoding.Wire.parseTaggedValueFromWire
                                        wire
                                loop
                                  (Lens.Family2.over
                                     Data.ProtoLens.unknownFields (\ !t -> (:) y t) x)
      in
        (Data.ProtoLens.Encoding.Bytes.<?>)
          (do loop Data.ProtoLens.defMessage) "CompileResponse"
  buildMessage
    = \ _x
        -> (Data.Monoid.<>)
             (case
                  Lens.Family2.view
                    (Data.ProtoLens.Field.field @"maybe'diagnostics") _x
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
                     Lens.Family2.view (Data.ProtoLens.Field.field @"maybe'compiled") _x
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
instance Control.DeepSeq.NFData CompileResponse where
  rnf
    = \ x__
        -> Control.DeepSeq.deepseq
             (_CompileResponse'_unknownFields x__)
             (Control.DeepSeq.deepseq
                (_CompileResponse'diagnostics x__)
                (Control.DeepSeq.deepseq (_CompileResponse'compiled x__) ()))
{- | Fields :
      -}
data Compiled
  = Compiled'_constructor {_Compiled'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving stock (Prelude.Eq, Prelude.Ord)
instance Prelude.Show Compiled where
  showsPrec _ __x __s
    = Prelude.showChar
        '{'
        (Prelude.showString
           (Data.ProtoLens.showMessageShort __x) (Prelude.showChar '}' __s))
instance Data.ProtoLens.Message Compiled where
  messageName _ = Data.Text.pack "lambdabuffers.compiler.Compiled"
  packedMessageDescriptor _
    = "\n\
      \\bCompiled"
  packedFileDescriptor _ = packedFileDescriptor
  fieldsByTag = let in Data.Map.fromList []
  unknownFields
    = Lens.Family2.Unchecked.lens
        _Compiled'_unknownFields
        (\ x__ y__ -> x__ {_Compiled'_unknownFields = y__})
  defMessage = Compiled'_constructor {_Compiled'_unknownFields = []}
  parseMessage
    = let
        loop :: Compiled -> Data.ProtoLens.Encoding.Bytes.Parser Compiled
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
                      case tag of {
                        wire
                          -> do !y <- Data.ProtoLens.Encoding.Wire.parseTaggedValueFromWire
                                        wire
                                loop
                                  (Lens.Family2.over
                                     Data.ProtoLens.unknownFields (\ !t -> (:) y t) x) }
      in
        (Data.ProtoLens.Encoding.Bytes.<?>)
          (do loop Data.ProtoLens.defMessage) "Compiled"
  buildMessage
    = \ _x
        -> Data.ProtoLens.Encoding.Wire.buildFieldSet
             (Lens.Family2.view Data.ProtoLens.unknownFields _x)
instance Control.DeepSeq.NFData Compiled where
  rnf
    = \ x__
        -> Control.DeepSeq.deepseq (_Compiled'_unknownFields x__) ()
data LambdaBuffersCompiler = LambdaBuffersCompiler {}
instance Data.ProtoLens.Service.Types.Service LambdaBuffersCompiler where
  type ServiceName LambdaBuffersCompiler = "LambdaBuffersCompiler"
  type ServicePackage LambdaBuffersCompiler = "lambdabuffers.compiler"
  type ServiceMethods LambdaBuffersCompiler = '["compile"]
  packedServiceDescriptor _
    = "\n\
      \\NAKLambdaBuffersCompiler\DC2\\\n\
      \\aCompile\DC2&.lambdabuffers.compiler.CompileRequest\SUB'.lambdabuffers.compiler.CompileResponse\"\NUL"
instance Data.ProtoLens.Service.Types.HasMethodImpl LambdaBuffersCompiler "compile" where
  type MethodName LambdaBuffersCompiler "compile" = "Compile"
  type MethodInput LambdaBuffersCompiler "compile" = CompileRequest
  type MethodOutput LambdaBuffersCompiler "compile" = CompileResponse
  type MethodStreamingType LambdaBuffersCompiler "compile" = 'Data.ProtoLens.Service.Types.NonStreaming
packedFileDescriptor :: Data.ByteString.ByteString
packedFileDescriptor
  = "\n\
    \$lambdabuffers-compiler-service.proto\DC2\SYNlambdabuffers.compiler\SUB\SUBlambdabuffers-source.proto\SUB\USlambdabuffers-source-diag.proto\"F\n\
    \\SOCompileRequest\DC24\n\
    \\ACKsource\CAN\SOH \SOH(\v2\FS.lambdabuffers.source.SourceR\ACKsource\"\152\SOH\n\
    \\SICompileResponse\DC2G\n\
    \\vdiagnostics\CAN\SOH \SOH(\v2%.lambdabuffers.source.diag.DiagnosticR\vdiagnostics\DC2<\n\
    \\bcompiled\CAN\STX \SOH(\v2 .lambdabuffers.compiler.CompiledR\bcompiled\"\n\
    \\n\
    \\bCompiled2u\n\
    \\NAKLambdaBuffersCompiler\DC2\\\n\
    \\aCompile\DC2&.lambdabuffers.compiler.CompileRequest\SUB'.lambdabuffers.compiler.CompileResponse\"\NULJ\222\ENQ\n\
    \\ACK\DC2\EOT\SOH\NUL\SUB\SOH\n\
    \\169\SOH\n\
    \\SOH\f\DC2\ETX\SOH\NUL\DC2\SUB\158\SOH Run with: protoc --plugin=protoc-gen-haskell=`which proto-lens-protoc` --proto_path=repls --haskell_out=proto_out repls/lambdabuffers-compiler-service.proto\n\
    \\n\
    \\b\n\
    \\SOH\STX\DC2\ETX\ETX\NUL\US\n\
    \\t\n\
    \\STX\ETX\NUL\DC2\ETX\ENQ\NUL$\n\
    \\t\n\
    \\STX\ETX\SOH\DC2\ETX\ACK\NUL)\n\
    \\n\
    \\n\
    \\STX\ACK\NUL\DC2\EOT\t\NUL\v\SOH\n\
    \\n\
    \\n\
    \\ETX\ACK\NUL\SOH\DC2\ETX\t\b\GS\n\
    \\v\n\
    \\EOT\ACK\NUL\STX\NUL\DC2\ETX\n\
    \\STX:\n\
    \\f\n\
    \\ENQ\ACK\NUL\STX\NUL\SOH\DC2\ETX\n\
    \\ACK\r\n\
    \\f\n\
    \\ENQ\ACK\NUL\STX\NUL\STX\DC2\ETX\n\
    \\SO\FS\n\
    \\f\n\
    \\ENQ\ACK\NUL\STX\NUL\ETX\DC2\ETX\n\
    \'6\n\
    \\n\
    \\n\
    \\STX\EOT\NUL\DC2\EOT\r\NUL\DC1\SOH\n\
    \\n\
    \\n\
    \\ETX\EOT\NUL\SOH\DC2\ETX\r\b\SYN\n\
    \\131\SOH\n\
    \\EOT\EOT\NUL\STX\NUL\DC2\ETX\SI\STX)\SUBT Compilation takes the whole compilation closure! Every module used should be here!\n\
    \\"  Some other compilation options\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\NUL\ACK\DC2\ETX\SI\STX\GS\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\NUL\SOH\DC2\ETX\SI\RS$\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\NUL\ETX\DC2\ETX\SI'(\n\
    \\n\
    \\n\
    \\STX\EOT\SOH\DC2\EOT\DC3\NUL\SYN\SOH\n\
    \\n\
    \\n\
    \\ETX\EOT\SOH\SOH\DC2\ETX\DC3\b\ETB\n\
    \\v\n\
    \\EOT\EOT\SOH\STX\NUL\DC2\ETX\DC4\STX7\n\
    \\f\n\
    \\ENQ\EOT\SOH\STX\NUL\ACK\DC2\ETX\DC4\STX&\n\
    \\f\n\
    \\ENQ\EOT\SOH\STX\NUL\SOH\DC2\ETX\DC4'2\n\
    \\f\n\
    \\ENQ\EOT\SOH\STX\NUL\ETX\DC2\ETX\DC456\n\
    \\v\n\
    \\EOT\EOT\SOH\STX\SOH\DC2\ETX\NAK\STX\CAN\n\
    \\f\n\
    \\ENQ\EOT\SOH\STX\SOH\ACK\DC2\ETX\NAK\STX\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\SOH\STX\SOH\SOH\DC2\ETX\NAK\v\DC3\n\
    \\f\n\
    \\ENQ\EOT\SOH\STX\SOH\ETX\DC2\ETX\NAK\SYN\ETB\n\
    \_\n\
    \\STX\EOT\STX\DC2\EOT\CAN\NUL\SUB\SOH\"S Almost identical to Source, but with semantically valid and with backwards compat\n\
    \\n\
    \\n\
    \\n\
    \\ETX\EOT\STX\SOH\DC2\ETX\CAN\b\DLEb\ACKproto3"