module LambdaBuffers.Compiler.ProtoCompat.FromProto (
  runFromProto,
  toProto,
) where

import Control.Lens ((&), (.~), (^.))
import Control.Monad.Except (Except, MonadError (throwError), runExcept)
import Control.Monad.Reader (MonadReader (ask, local), ReaderT (runReaderT))
import Data.Foldable (foldlM, toList)
import Data.Generics.Labels ()
import Data.Generics.Product (HasField)
import Data.Kind (Type)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.ProtoLens (Message (messageName), MessageEnum (showEnum), defMessage)
import Data.Proxy (Proxy (Proxy))
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import GHC.Generics (Generic)
import LambdaBuffers.Compiler.NamingCheck (checkClassName, checkConstrName, checkFieldName, checkModuleNamePart, checkTyName, checkVarName)
import LambdaBuffers.Compiler.ProtoCompat.Types qualified as PC
import Proto.Compiler (NamingError)
import Proto.Compiler qualified as P
import Proto.Compiler_Fields qualified as P

data FromProtoErr
  = FPNamingError P.NamingError
  | FPInternalError P.InternalError
  | FPProtoParseError P.ProtoParseError
  deriving stock (Show, Eq, Ord, Generic)

data FromProtoContext
  = CtxCompilerInput
  | CtxModule P.ModuleName
  | CtxTyDef P.ModuleName P.TyDef
  | CtxClassDef P.ModuleName P.ClassDef
  deriving stock (Show, Eq, Ord, Generic)

type FromProto a = ReaderT FromProtoContext (Except [FromProtoErr]) a

-- | Parse a Proto API CompilerInput into the internal CompilerInput representation or report errors (in Proto format).
runFromProto :: P.CompilerInput -> Either P.CompilerError PC.CompilerInput
runFromProto compInp = do
  let exM = runReaderT (fromProto compInp) CtxCompilerInput
      errsOrRes = runExcept exM
  case errsOrRes of
    Left errs ->
      let nerrs = [err | FPNamingError err <- errs]
          pperrs = [err | FPProtoParseError err <- errs]
          ierrs = [err | FPInternalError err <- errs]
       in Left $
            defMessage
              & P.namingErrors .~ nerrs
              & P.protoParseErrors .~ pperrs
              & P.internalErrors .~ ierrs
    Right compIn' -> return compIn'

class IsMessage (proto :: Type) (good :: Type) where
  fromProto :: proto -> FromProto good

  toProto :: good -> proto

throwNamingError :: Either NamingError b -> FromProto b
throwNamingError = either (\err -> throwError [FPNamingError err]) return

throwOneOfError :: Text -> Text -> FromProto b
throwOneOfError protoMsgName protoFieldName =
  throwError
    [ FPProtoParseError $
        defMessage
          & P.oneOfNotSetError . P.messageName .~ protoMsgName
          & P.oneOfNotSetError . P.fieldName .~ protoFieldName
    ]

throwInternalError :: Text -> FromProto b
throwInternalError msg = throwError [FPInternalError $ defMessage & P.msg .~ msg]

parseAndIndex :: forall {t :: Type -> Type} {proto} {a} {k}. (Foldable t, IsMessage proto a, Ord k) => (a -> k) -> t proto -> FromProto (Map k a, Map k [proto])
parseAndIndex key =
  foldlM
    ( \(indexed, multiples) px -> do
        x <- fromProto px
        let k = key x
        if Map.member k indexed
          then return (indexed, Map.insertWith (++) k [px] multiples)
          else return (Map.insert k x indexed, multiples)
    )
    (mempty, mempty)

-- WARN(bladyjoker): This function is used to 'strip' the SourceInfo from types that end up as Map keys.
--   This can cause confusion and errors and we should rather parametrize types with `info` and
--   maintain `Map (TyName ()) (TyDef SourceInfo)`
stripSourceInfo :: HasField "sourceInfo" s t a PC.SourceInfo => s -> t
stripSourceInfo x = x & #sourceInfo .~ PC.defSourceInfo

{-
    SourceInfo
-}

instance IsMessage P.SourcePosition PC.SourcePosition where
  fromProto sp = do
    let col = fromIntegral $ sp ^. P.column
        row = fromIntegral $ sp ^. P.row
    pure $ PC.SourcePosition col row

  toProto sp =
    defMessage
      & P.column .~ fromIntegral (sp ^. #column)
      & P.row .~ fromIntegral (sp ^. #row)

instance IsMessage P.SourceInfo PC.SourceInfo where
  fromProto si = do
    let file = si ^. P.file
    pFrom <- fromProto $ si ^. P.posFrom
    pTo <- fromProto $ si ^. P.posTo
    pure $ PC.SourceInfo file pFrom pTo

  toProto si =
    defMessage
      & P.file .~ (si ^. #file)
      & P.posFrom .~ toProto (si ^. #posFrom)
      & P.posTo .~ toProto (si ^. #posTo)

{-
    Names
-}

instance IsMessage Text Text where
  fromProto = pure
  toProto = id

instance IsMessage P.ModuleNamePart PC.ModuleNamePart where
  fromProto mnp = do
    si <- fromProto $ mnp ^. P.sourceInfo
    throwNamingError $ checkModuleNamePart mnp
    pure $ PC.ModuleNamePart (mnp ^. P.name) si

  toProto (PC.ModuleNamePart nm si) =
    defMessage
      & P.name .~ nm
      & P.sourceInfo .~ toProto si

instance IsMessage P.ModuleName PC.ModuleName where
  fromProto mn = do
    si <- fromProto $ mn ^. P.sourceInfo
    parts <- traverse fromProto $ mn ^. P.parts
    pure $ PC.ModuleName parts si

  toProto (PC.ModuleName parts si) =
    defMessage
      & P.parts .~ (toProto <$> parts)
      & P.sourceInfo .~ toProto si

instance IsMessage P.FieldName PC.FieldName where
  fromProto v = do
    throwNamingError $ checkFieldName v
    PC.FieldName <$> fromProto (v ^. P.name) <*> fromProto (v ^. P.sourceInfo)

  toProto (PC.FieldName n si) =
    defMessage
      & P.name .~ toProto n
      & P.sourceInfo .~ toProto si

instance IsMessage P.ConstrName PC.ConstrName where
  fromProto v = do
    throwNamingError $ checkConstrName v
    PC.ConstrName <$> fromProto (v ^. P.name) <*> fromProto (v ^. P.sourceInfo)

  toProto (PC.ConstrName n si) =
    defMessage
      & P.name .~ toProto n
      & P.sourceInfo .~ toProto si

instance IsMessage P.TyName PC.TyName where
  fromProto v = do
    throwNamingError $ checkTyName v
    PC.TyName <$> fromProto (v ^. P.name) <*> fromProto (v ^. P.sourceInfo)

  toProto (PC.TyName n si) =
    defMessage
      & P.name .~ toProto n
      & P.sourceInfo .~ toProto si

instance IsMessage P.ClassName PC.ClassName where
  fromProto v = do
    throwNamingError $ checkClassName v
    PC.ClassName <$> fromProto (v ^. P.name) <*> fromProto (v ^. P.sourceInfo)

  toProto (PC.ClassName n si) =
    defMessage
      & P.name .~ toProto n
      & P.sourceInfo .~ toProto si

instance IsMessage P.VarName PC.VarName where
  fromProto v = do
    throwNamingError $ checkVarName v
    PC.VarName <$> fromProto (v ^. P.name) <*> fromProto (v ^. P.sourceInfo)

  toProto (PC.VarName n si) =
    defMessage
      & P.name .~ toProto n
      & P.sourceInfo .~ toProto si

{-
    Ty & Components
-}

instance IsMessage P.TyVar PC.TyVar where
  fromProto tv = do
    vn <- fromProto $ tv ^. P.varName
    pure $ PC.TyVar vn

  toProto (PC.TyVar vn) =
    defMessage
      & P.varName .~ toProto vn

instance IsMessage P.TyApp PC.TyApp where
  fromProto ta = do
    tf <- fromProto $ ta ^. P.tyFunc
    si <- fromProto $ ta ^. P.sourceInfo
    targs <- traverse fromProto $ ta ^. P.tyArgs
    pure $ PC.TyApp tf targs si

  toProto (PC.TyApp tf args si) =
    defMessage
      & P.tyFunc .~ toProto tf
      & P.tyArgs .~ toList (toProto <$> args)
      & P.sourceInfo .~ toProto si

instance IsMessage P.Ty PC.Ty where
  fromProto ti = case ti ^. P.maybe'ty of
    Nothing -> throwOneOfError (messageName (Proxy @P.Ty)) "ty"
    Just x -> case x of
      P.Ty'TyVar tv -> PC.TyVarI <$> fromProto tv
      P.Ty'TyApp ta -> PC.TyAppI <$> fromProto ta
      P.Ty'TyRef tr -> PC.TyRefI <$> fromProto tr

  toProto = \case
    PC.TyVarI tv -> defMessage & P.tyVar .~ toProto tv
    PC.TyRefI tr -> defMessage & P.tyRef .~ toProto tr
    PC.TyAppI ta -> defMessage & P.tyApp .~ toProto ta

instance IsMessage P.TyRef'Local PC.LocalRef where
  fromProto lr = do
    si <- fromProto $ lr ^. P.sourceInfo
    nm <- fromProto $ lr ^. P.tyName
    pure $ PC.LocalRef nm si

  toProto (PC.LocalRef nm si) =
    defMessage
      & P.tyName .~ toProto nm
      & P.sourceInfo .~ toProto si

instance IsMessage P.TyRef'Foreign PC.ForeignRef where
  fromProto fr = do
    si <- fromProto $ fr ^. P.sourceInfo
    mn <- fromProto $ fr ^. P.moduleName
    tn <- fromProto $ fr ^. P.tyName
    pure $ PC.ForeignRef tn mn si

  toProto (PC.ForeignRef tn mn si) =
    defMessage
      & P.tyName .~ toProto tn
      & P.moduleName .~ toProto mn
      & P.sourceInfo .~ toProto si

instance IsMessage P.TyRef PC.TyRef where
  fromProto tr = case tr ^. P.maybe'tyRef of
    Nothing -> throwOneOfError (messageName (Proxy @P.TyRef)) "ty_ref"
    Just x -> case x of
      P.TyRef'LocalTyRef lr -> PC.LocalI <$> fromProto lr
      P.TyRef'ForeignTyRef f -> PC.ForeignI <$> fromProto f

  toProto = \case
    PC.LocalI lr -> defMessage & P.localTyRef .~ toProto lr
    PC.ForeignI fr -> defMessage & P.foreignTyRef .~ toProto fr

{-
    TyDef & Components
-}

instance IsMessage P.TyDef PC.TyDef where
  fromProto td = do
    ctx <- ask
    ctxModuleName <- case ctx of
      CtxModule mn -> return mn
      _ -> throwInternalError "Expected to be in Module Context"
    local (const $ CtxTyDef ctxModuleName td) $ do
      tnm <- fromProto $ td ^. P.tyName
      tyabs <- fromProto $ td ^. P.tyAbs
      si <- fromProto $ td ^. P.sourceInfo
      pure $ PC.TyDef tnm tyabs si

  toProto (PC.TyDef tnm tyabs si) =
    defMessage
      & P.tyName .~ toProto tnm
      & P.tyAbs .~ toProto tyabs
      & P.sourceInfo .~ toProto si

instance IsMessage P.TyAbs PC.TyAbs where
  fromProto ta = do
    (tyargs, mulTyArgs) <- parseAndIndex (\a -> stripSourceInfo $ a ^. #argName) (ta ^. P.tyArgs)
    tybody <- fromProto $ ta ^. P.tyBody
    si <- fromProto $ ta ^. P.sourceInfo
    ctx <- ask
    (ctxMn, ctxTyd) <- case ctx of
      CtxTyDef mn tyd -> return (mn, tyd)
      _ -> throwInternalError "Expected to be in TyDef Context"
    let mulArgsErrs =
          [ FPProtoParseError $
            defMessage
              & P.multipleTyargError . P.moduleName .~ ctxMn
              & P.multipleTyargError . P.tyDef .~ ctxTyd
              & P.multipleTyargError . P.tyArgs .~ args
          | (_an, args) <- Map.toList mulTyArgs
          ]
    if null mulArgsErrs
      then pure $ PC.TyAbs tyargs tybody si
      else throwError mulArgsErrs

  toProto (PC.TyAbs tyargs tyabs si) =
    defMessage
      & P.tyArgs .~ (toProto <$> toList tyargs)
      & P.tyBody .~ toProto tyabs
      & P.sourceInfo .~ toProto si

instance IsMessage P.Kind'KindRef PC.KindRefType where
  fromProto kr =
    ( \case
        P.Kind'KIND_REF_TYPE -> pure PC.KType
        P.Kind'KIND_REF_UNSPECIFIED -> pure PC.KUnspecified
        P.Kind'KIND_REF_CONSTRAINT -> pure PC.KConstraint
        P.Kind'KindRef'Unrecognized v ->
          throwError
            [ FPProtoParseError $
                defMessage
                  & P.unknownEnumError . P.enumName .~ Text.pack (showEnum kr)
                  & P.unknownEnumError . P.gotTag .~ (Text.pack . show $ v)
            ]
    )
      kr

  toProto = \case
    PC.KType -> P.Kind'KIND_REF_TYPE
    PC.KUnspecified -> P.Kind'KIND_REF_UNSPECIFIED
    PC.KConstraint -> P.Kind'KIND_REF_CONSTRAINT

instance IsMessage P.Kind PC.Kind where
  fromProto k = do
    kt <- case k ^. P.maybe'kind of
      Nothing -> throwOneOfError (messageName (Proxy @P.Kind)) "kind"
      Just k' -> case k' of
        P.Kind'KindRef r -> PC.KindRef <$> fromProto r
        P.Kind'KindArrow' arr -> PC.KindArrow <$> fromProto (arr ^. P.left) <*> fromProto (arr ^. P.right)
    pure $ PC.Kind kt

  toProto (PC.Kind (PC.KindArrow l r)) = do
    defMessage
      & P.kindArrow . P.left .~ toProto l
      & P.kindArrow . P.right .~ toProto r
  toProto (PC.Kind (PC.KindRef r)) = do
    defMessage
      & P.kindRef .~ toProto r

instance IsMessage P.TyArg PC.TyArg where
  fromProto ta = do
    argnm <- fromProto $ ta ^. P.argName
    si <- fromProto $ ta ^. P.sourceInfo
    kind <- fromProto $ ta ^. P.argKind
    pure $ PC.TyArg argnm kind si

  toProto (PC.TyArg argnm argkind si) =
    defMessage
      & P.argName .~ toProto argnm
      & P.argKind .~ toProto argkind
      & P.sourceInfo .~ toProto si

instance IsMessage P.TyBody PC.TyBody where
  fromProto tb = case tb ^. P.maybe'tyBody of
    Nothing -> throwOneOfError (messageName (Proxy @P.TyBody)) "ty_body"
    Just x -> case x of
      P.TyBody'Opaque opq -> PC.OpaqueI <$> fromProto (opq ^. P.sourceInfo)
      P.TyBody'Sum sumI -> PC.SumI <$> fromProto sumI

  toProto = \case
    PC.OpaqueI si ->
      let opaque = defMessage & P.sourceInfo .~ toProto si
       in defMessage & P.opaque .~ opaque
    PC.SumI sb -> defMessage & P.sum .~ toProto sb

instance IsMessage P.Sum PC.Sum where
  fromProto s = do
    (ctors, mulCtors) <- parseAndIndex (\c -> stripSourceInfo $ c ^. #constrName) (s ^. P.constructors)
    si <- fromProto $ s ^. P.sourceInfo
    ctx <- ask
    (ctxMn, ctxTyd) <- case ctx of
      CtxTyDef mn tyd -> return (mn, tyd)
      _ -> throwInternalError "Expected to be in TyDef Context"
    let mulCtorsErrs =
          [ FPProtoParseError $
            defMessage
              & P.multipleConstructorError . P.moduleName .~ ctxMn
              & P.multipleConstructorError . P.tyDef .~ ctxTyd
              & P.multipleConstructorError . P.constructors .~ cs
          | (_cn, cs) <- Map.toList mulCtors
          ]
    if null mulCtorsErrs
      then pure $ PC.Sum ctors si
      else throwError mulCtorsErrs

  toProto (PC.Sum ctors si) =
    defMessage
      & P.constructors .~ toList (toProto <$> ctors)
      & P.sourceInfo .~ toProto si

instance IsMessage P.Sum'Constructor PC.Constructor where
  fromProto c = do
    cnm <- fromProto $ c ^. P.constrName
    prod <- fromProto $ c ^. P.product
    pure $ PC.Constructor cnm prod

  toProto (PC.Constructor cnm prod) =
    defMessage
      & P.constrName .~ toProto cnm
      & P.product .~ toProto prod

instance IsMessage P.Product'Record PC.Record where
  fromProto r = do
    (fields, mulFields) <- parseAndIndex (\f -> stripSourceInfo $ f ^. #fieldName) (r ^. P.fields)
    si <- fromProto $ r ^. P.sourceInfo
    ctx <- ask
    (ctxMn, ctxTyd) <- case ctx of
      CtxTyDef mn tyd -> return (mn, tyd)
      _ -> throwInternalError "Expected to be in TyDef Context"
    let mulFieldsErrs =
          [ FPProtoParseError $
            defMessage
              & P.multipleFieldError . P.moduleName .~ ctxMn
              & P.multipleFieldError . P.tyDef .~ ctxTyd
              & P.multipleFieldError . P.fields .~ fs
          | (_fn, fs) <- Map.toList mulFields
          ]
    if null mulFieldsErrs
      then pure $ PC.Record fields si
      else throwError mulFieldsErrs

  toProto (PC.Record fs si) =
    defMessage
      & P.fields .~ (toProto <$> toList fs)
      & P.sourceInfo .~ toProto si

instance IsMessage P.Product'NTuple PC.Tuple where
  fromProto r = do
    fs <- traverse fromProto $ r ^. P.fields
    si <- fromProto $ r ^. P.sourceInfo
    pure $ PC.Tuple fs si

  toProto (PC.Tuple fs si) =
    defMessage
      & P.fields .~ (toProto <$> fs)
      & P.sourceInfo .~ toProto si

instance IsMessage P.Product PC.Product where
  fromProto p = case p ^. P.maybe'product of
    Nothing -> throwOneOfError (messageName (Proxy @P.Product)) "product"
    Just x -> case x of
      --- wrong, fix
      P.Product'Record' r -> do
        recrd <- fromProto r
        pure $ PC.RecordI recrd
      P.Product'Ntuple t -> do
        tup <- fromProto t
        pure $ PC.TupleI tup

  toProto = \case
    PC.RecordI r -> defMessage & P.record .~ toProto r
    PC.TupleI t -> defMessage & P.ntuple .~ toProto t

instance IsMessage P.Product'Record'Field PC.Field where
  fromProto f = do
    fnm <- fromProto $ f ^. P.fieldName
    fty <- fromProto $ f ^. P.fieldTy
    pure $ PC.Field fnm fty

  toProto (PC.Field fnm fty) =
    defMessage
      & P.fieldName .~ toProto fnm
      & P.fieldTy .~ toProto fty

{-
    Classes, instances, constraints
-}

instance IsMessage P.TyClassRef'Local PC.LocalClassRef where
  fromProto lr = do
    si <- fromProto $ lr ^. P.sourceInfo
    nm <- fromProto $ lr ^. P.className
    pure $ PC.LocalClassRef nm si

  toProto (PC.LocalClassRef nm si) =
    defMessage
      & P.className .~ toProto nm
      & P.sourceInfo .~ toProto si

instance IsMessage P.TyClassRef'Foreign PC.ForeignClassRef where
  fromProto fr = do
    si <- fromProto $ fr ^. P.sourceInfo
    mn <- fromProto $ fr ^. P.moduleName
    tn <- fromProto $ fr ^. P.className
    pure $ PC.ForeignClassRef tn mn si

  toProto (PC.ForeignClassRef tn mn si) =
    defMessage
      & P.className .~ toProto tn
      & P.moduleName .~ toProto mn
      & P.sourceInfo .~ toProto si

instance IsMessage P.TyClassRef PC.TyClassRef where
  fromProto tr = case tr ^. P.maybe'classRef of
    Nothing -> throwOneOfError (messageName (Proxy @P.TyClassRef)) "class_ref"
    Just x -> case x of
      P.TyClassRef'LocalClassRef lr -> PC.LocalCI <$> fromProto lr
      P.TyClassRef'ForeignClassRef f -> PC.ForeignCI <$> fromProto f

  toProto = \case
    PC.LocalCI lr -> defMessage & P.localClassRef .~ toProto lr
    PC.ForeignCI fr -> defMessage & P.foreignClassRef .~ toProto fr

instance IsMessage P.ClassDef PC.ClassDef where
  fromProto cd = do
    ctx <- ask
    ctxMn <- case ctx of
      CtxModule mn -> return mn
      _ -> throwInternalError "Expected to be in Module Context"
    local (const $ CtxClassDef ctxMn cd) $ do
      si <- fromProto $ cd ^. P.sourceInfo
      cnm <- fromProto $ cd ^. P.className
      cargs <- traverse fromProto $ cd ^. P.classArgs
      carg <- case cargs of
        [] -> throwInternalError "Zero parameter type classes are not supported"
        [x] -> return x
        _ -> throwInternalError "Multi parameter type classes are not supported"
      sups <- traverse fromProto $ cd ^. P.supers
      let doc = cd ^. P.documentation
      pure $ PC.ClassDef cnm carg sups doc si

  toProto (PC.ClassDef cnm carg sups doc si) =
    defMessage
      & P.className .~ toProto cnm
      & P.classArgs .~ pure (toProto carg)
      & P.supers .~ (toProto <$> sups)
      & P.documentation .~ doc
      & P.sourceInfo .~ toProto si

instance IsMessage P.InstanceClause PC.InstanceClause where
  fromProto ic = do
    si <- fromProto $ ic ^. P.sourceInfo
    cnm <- fromProto $ ic ^. P.classRef
    csts <- traverse fromProto $ ic ^. P.constraints
    args <- traverse fromProto $ ic ^. P.args
    arg <- case args of
      [] -> throwInternalError "Zero instance arguments, but zero parameter type classes are not supported"
      [x] -> return x
      _ -> throwInternalError "Multiple instance arguments, but multi parameter type classes are not supported"
    pure $ PC.InstanceClause cnm arg csts si

  toProto (PC.InstanceClause cnm hd csts si) =
    defMessage
      & P.classRef .~ toProto cnm
      & P.args .~ pure (toProto hd)
      & P.constraints .~ (toProto <$> csts)
      & P.sourceInfo .~ toProto si

instance IsMessage P.Constraint PC.Constraint where
  fromProto c = do
    si <- fromProto $ c ^. P.sourceInfo
    cnm <- fromProto $ c ^. P.classRef
    args <- traverse fromProto $ c ^. P.args
    arg <- case args of
      [] -> throwInternalError "Zero constraint arguments, but zero parameter type classes are not supported"
      [x] -> return x
      _ -> throwInternalError "Multiple constraint arguments, but multi parameter type classes are not supported"
    pure $ PC.Constraint cnm arg si

  toProto (PC.Constraint cnm arg si) =
    defMessage
      & P.classRef .~ toProto cnm
      & P.args .~ pure (toProto arg)
      & P.sourceInfo .~ toProto si

{-
    Module, CompilerInput
-}

instance IsMessage P.Module PC.Module where
  fromProto m = do
    ctx <- ask
    case ctx of
      CtxCompilerInput -> return ()
      _ -> throwInternalError "Expected to be in CompilerInput Context"
    local (const $ CtxModule (m ^. P.moduleName)) $ do
      mnm <- fromProto $ m ^. P.moduleName
      (tydefs, mulTyDefs) <- parseAndIndex (\tyDef -> stripSourceInfo $ tyDef ^. #tyName) (m ^. P.typeDefs)
      (cldefs, mulClDefs) <- parseAndIndex (\cldef -> stripSourceInfo $ cldef ^. #className) (m ^. P.classDefs)
      (impts, mulImpts) <- parseAndIndex stripSourceInfo (m ^. P.imports)
      insts <- traverse fromProto $ m ^. P.instances
      si <- fromProto $ m ^. P.sourceInfo
      let mulTyDefsErrs =
            [ FPProtoParseError $
              defMessage
                & P.multipleTydefError . P.moduleName .~ (m ^. P.moduleName)
                & P.multipleTydefError . P.tyDefs .~ tds
            | (_tn, tds) <- Map.toList mulTyDefs
            ]
          mulClassDefsErrs =
            [ FPProtoParseError $
              defMessage
                & P.multipleClassdefError . P.moduleName .~ (m ^. P.moduleName)
                & P.multipleClassdefError . P.classDefs .~ cds
            | (_cn, cds) <- Map.toList mulClDefs
            ]
          mulImptsErrs =
            [ FPProtoParseError $
              defMessage
                & P.multipleImportError . P.moduleName .~ (m ^. P.moduleName)
                & P.multipleImportError . P.imports .~ ims
            | (_in, ims) <- Map.toList mulImpts
            ]
          protoParseErrs = mulTyDefsErrs ++ mulClassDefsErrs ++ mulImptsErrs
      if null protoParseErrs
        then pure $ PC.Module mnm tydefs cldefs insts (Map.keysSet impts) si
        else throwError protoParseErrs

  toProto (PC.Module mnm tdefs cdefs insts impts si) =
    defMessage
      & P.moduleName .~ toProto mnm
      & P.typeDefs .~ (toProto <$> toList tdefs)
      & P.classDefs .~ (toProto <$> toList cdefs)
      & P.instances .~ (toProto <$> insts)
      & P.imports .~ (toProto <$> Set.toList impts)
      & P.sourceInfo .~ toProto si

instance IsMessage P.CompilerInput PC.CompilerInput where
  fromProto ci = do
    local (const CtxCompilerInput) $ do
      (mods, mulModules) <- parseAndIndex (\m -> stripSourceInfo $ m ^. #moduleName) (ci ^. P.modules)
      let mulModulesErrs =
            [ FPProtoParseError $
              defMessage & P.multipleModuleError . P.modules .~ ms
            | (_mn, ms) <- Map.toList mulModules
            ]
      if null mulModulesErrs
        then pure $ PC.CompilerInput mods
        else throwError mulModulesErrs

  toProto (PC.CompilerInput ms) =
    defMessage
      & P.modules .~ (toProto <$> toList ms)

{-
  Outputs
-}

instance IsMessage P.KindCheckError PC.KindCheckError where
  fromProto kce =
    case kce ^. P.maybe'kindCheckError of
      Just x -> case x of
        P.KindCheckError'UnboundTyVarError' err ->
          PC.UnboundTyVarError
            <$> fromProto (err ^. P.tyDef)
            <*> fromProto (err ^. P.tyVar)
            <*> fromProto (err ^. P.moduleName)
        P.KindCheckError'UnboundTyRefError' err ->
          PC.UnboundTyRefError
            <$> fromProto (err ^. P.tyDef)
            <*> fromProto (err ^. P.tyRef)
            <*> fromProto (err ^. P.moduleName)
        P.KindCheckError'UnificationError' err ->
          PC.IncorrectApplicationError
            <$> fromProto (err ^. P.tyDef)
            <*> fromProto (err ^. P.tyKindLhs)
            <*> fromProto (err ^. P.tyKindRhs)
            <*> fromProto (err ^. P.moduleName)
        P.KindCheckError'CyclicKindError' err ->
          PC.RecursiveKindError
            <$> fromProto (err ^. P.tyDef)
            <*> fromProto (err ^. P.moduleName)
        P.KindCheckError'InconsistentTypeError' err ->
          PC.InconsistentTypeError
            <$> fromProto (err ^. P.tyDef)
            <*> fromProto (err ^. P.actualKind)
            <*> fromProto (err ^. P.expectedKind)
            <*> fromProto (err ^. P.moduleName)
      Nothing -> throwOneOfError (messageName (Proxy @P.KindCheckError)) "kind_check_error"

  toProto = \case
    PC.UnboundTyVarError tydef tyvar modname ->
      defMessage
        & (P.unboundTyVarError . P.tyDef) .~ toProto tydef
        & (P.unboundTyVarError . P.tyVar) .~ toProto tyvar
        & (P.unboundTyVarError . P.moduleName) .~ toProto modname
    PC.UnboundTyRefError tydef tyref modname ->
      defMessage
        & (P.unboundTyRefError . P.tyDef) .~ toProto tydef
        & (P.unboundTyRefError . P.tyRef) .~ toProto tyref
        & (P.unboundTyRefError . P.moduleName) .~ toProto modname
    PC.IncorrectApplicationError tydef k1 k2 modname ->
      defMessage
        & (P.unificationError . P.tyDef) .~ toProto tydef
        & (P.unificationError . P.tyKindLhs) .~ toProto k1
        & (P.unificationError . P.tyKindRhs) .~ toProto k2
        & (P.unificationError . P.moduleName) .~ toProto modname
    PC.RecursiveKindError tydef modname ->
      defMessage
        & (P.cyclicKindError . P.tyDef) .~ toProto tydef
        & (P.cyclicKindError . P.moduleName) .~ toProto modname
    PC.InconsistentTypeError tydef ki kd modname ->
      defMessage
        & (P.inconsistentTypeError . P.tyDef) .~ toProto tydef
        & (P.inconsistentTypeError . P.actualKind) .~ toProto ki
        & (P.inconsistentTypeError . P.expectedKind) .~ toProto kd
        & (P.inconsistentTypeError . P.moduleName) .~ toProto modname

instance IsMessage P.CompilerError PC.CompilerError where
  fromProto _ = throwInternalError "fromProto CompilerError not implemented"

  toProto = \case
    PC.CompKindCheckError err -> defMessage & P.kindCheckErrors .~ [toProto err]
    PC.InternalError err -> defMessage & P.internalErrors .~ [defMessage & P.msg .~ err]

instance IsMessage P.CompilerResult PC.CompilerResult where
  fromProto _ = throwInternalError "fromProto CompilerError not implemented"
  toProto PC.CompilerResult = defMessage

instance IsMessage P.CompilerOutput PC.CompilerOutput where
  fromProto _ = throwInternalError "fromProto CompilerError not implemented"

  toProto = \case
    Right res -> defMessage & P.compilerResult .~ toProto res
    Left err -> defMessage & P.compilerError .~ toProto err
