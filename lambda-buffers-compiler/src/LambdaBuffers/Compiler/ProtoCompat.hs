{-# OPTIONS_GHC -Wno-missing-import-lists #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module LambdaBuffers.Compiler.ProtoCompat (
  IsMessage (..),
  FromProtoErr (..),
  module X,
  protoKind2Kind,
  kind2ProtoKind,
  runFromProto,
) where

-- NOTE(cstml): I'm re-exporting the module from here as it makes more sense -
-- also avoids annoying errors.
import LambdaBuffers.Compiler.ProtoCompat.Types as X hiding (InternalError)

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
import LambdaBuffers.Compiler.KindCheck.Kind qualified as K
import LambdaBuffers.Compiler.NamingCheck (checkClassName, checkConstrName, checkFieldName, checkModuleNamePart, checkTyName, checkVarName)
import Proto.Compiler (NamingError)
import Proto.Compiler qualified as P
import Proto.Compiler_Fields qualified as P

data FromProtoErr
  = NamingError P.NamingError
  | InternalError P.InternalError
  | ProtoParseError P.ProtoParseError
  deriving stock (Show, Eq, Ord, Generic)

data FromProtoContext
  = CtxCompilerInput
  | CtxModule P.ModuleName
  | CtxTyDef P.ModuleName P.TyDef
  | CtxClassDef P.ModuleName P.ClassDef
  deriving stock (Show, Eq, Ord, Generic)

type FromProto a = ReaderT FromProtoContext (Except [FromProtoErr]) a

-- | Parse a Proto API CompilerInput into the internal CompilerInput representation or report errors.
runFromProto :: P.CompilerInput -> Either [FromProtoErr] CompilerInput
runFromProto compInp = do
  let exM = runReaderT (fromProto compInp) CtxCompilerInput
  runExcept exM

class IsMessage (proto :: Type) (good :: Type) where
  fromProto :: proto -> FromProto good

  toProto :: good -> proto

throwNamingError :: Either NamingError b -> FromProto b
throwNamingError = either (\err -> throwError [NamingError err]) return

throwOneOfError :: Text -> Text -> FromProto b
throwOneOfError protoMsgName protoFieldName =
  throwError
    [ ProtoParseError $
        defMessage
          & P.oneOfNotSetError . P.messageName .~ protoMsgName
          & P.oneOfNotSetError . P.fieldName .~ protoFieldName
    ]

throwInternalError :: Text -> FromProto b
throwInternalError msg = throwError [InternalError $ defMessage & P.msg .~ msg]

{-
    SourceInfo
-}

instance IsMessage P.SourcePosition SourcePosition where
  fromProto sp = do
    let col = fromIntegral $ sp ^. P.column
        row = fromIntegral $ sp ^. P.row
    pure $ SourcePosition col row

  toProto sp =
    defMessage
      & P.column .~ fromIntegral (sp ^. #column)
      & P.row .~ fromIntegral (sp ^. #row)

instance IsMessage P.SourceInfo SourceInfo where
  fromProto si = do
    let file = si ^. P.file
    pFrom <- fromProto $ si ^. P.posFrom
    pTo <- fromProto $ si ^. P.posTo
    pure $ SourceInfo file pFrom pTo

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

instance IsMessage P.ModuleNamePart ModuleNamePart where
  fromProto mnp = do
    si <- fromProto $ mnp ^. P.sourceInfo
    throwNamingError $ checkModuleNamePart mnp
    pure $ ModuleNamePart (mnp ^. P.name) si

  toProto (ModuleNamePart nm si) =
    defMessage
      & P.name .~ nm
      & P.sourceInfo .~ toProto si

instance IsMessage P.ModuleName ModuleName where
  fromProto mn = do
    si <- fromProto $ mn ^. P.sourceInfo
    parts <- traverse fromProto $ mn ^. P.parts
    pure $ ModuleName parts si

  toProto (ModuleName parts si) =
    defMessage
      & P.parts .~ (toProto <$> parts)
      & P.sourceInfo .~ toProto si

instance IsMessage P.FieldName FieldName where
  fromProto v = do
    throwNamingError $ checkFieldName v
    FieldName <$> fromProto (v ^. P.name) <*> fromProto (v ^. P.sourceInfo)

  toProto (FieldName n si) =
    defMessage
      & P.name .~ toProto n
      & P.sourceInfo .~ toProto si

instance IsMessage P.ConstrName ConstrName where
  fromProto v = do
    throwNamingError $ checkConstrName v
    ConstrName <$> fromProto (v ^. P.name) <*> fromProto (v ^. P.sourceInfo)

  toProto (ConstrName n si) =
    defMessage
      & P.name .~ toProto n
      & P.sourceInfo .~ toProto si

instance IsMessage P.TyName TyName where
  fromProto v = do
    throwNamingError $ checkTyName v
    TyName <$> fromProto (v ^. P.name) <*> fromProto (v ^. P.sourceInfo)

  toProto (TyName n si) =
    defMessage
      & P.name .~ toProto n
      & P.sourceInfo .~ toProto si

instance IsMessage P.ClassName ClassName where
  fromProto v = do
    throwNamingError $ checkClassName v
    ClassName <$> fromProto (v ^. P.name) <*> fromProto (v ^. P.sourceInfo)

  toProto (ClassName n si) =
    defMessage
      & P.name .~ toProto n
      & P.sourceInfo .~ toProto si

instance IsMessage P.VarName VarName where
  fromProto v = do
    throwNamingError $ checkVarName v
    VarName <$> fromProto (v ^. P.name) <*> fromProto (v ^. P.sourceInfo)

  toProto (VarName n si) =
    defMessage
      & P.name .~ toProto n
      & P.sourceInfo .~ toProto si

{-
    Ty & Components
-}

instance IsMessage P.TyVar TyVar where
  fromProto tv = do
    vn <- fromProto $ tv ^. P.varName
    si <- fromProto $ tv ^. P.sourceInfo
    pure $ TyVar vn si

  toProto (TyVar vn si) =
    defMessage
      & P.varName .~ toProto vn
      & P.sourceInfo .~ toProto si

instance IsMessage P.TyApp TyApp where
  fromProto ta = do
    tf <- fromProto $ ta ^. P.tyFunc
    si <- fromProto $ ta ^. P.sourceInfo
    targs <- traverse fromProto $ ta ^. P.tyArgs
    pure $ TyApp tf targs si

  toProto (TyApp tf args si) =
    defMessage
      & P.tyFunc .~ toProto tf
      & P.tyArgs .~ toList (toProto <$> args)
      & P.sourceInfo .~ toProto si

instance IsMessage P.Ty Ty where
  fromProto ti = case ti ^. P.maybe'ty of
    Nothing -> throwOneOfError (messageName (Proxy @P.Ty)) "ty"
    Just x -> case x of
      P.Ty'TyVar tv -> TyVarI <$> fromProto tv
      P.Ty'TyApp ta -> TyAppI <$> fromProto ta
      P.Ty'TyRef tr -> TyRefI <$> fromProto tr

  toProto = \case
    TyVarI tv -> defMessage & P.tyVar .~ toProto tv
    TyRefI tr -> defMessage & P.tyRef .~ toProto tr
    TyAppI ta -> defMessage & P.tyApp .~ toProto ta

instance IsMessage P.TyRef'Local LocalRef where
  fromProto lr = do
    si <- fromProto $ lr ^. P.sourceInfo
    nm <- fromProto $ lr ^. P.tyName
    pure $ LocalRef nm si

  toProto (LocalRef nm si) =
    defMessage
      & P.tyName .~ toProto nm
      & P.sourceInfo .~ toProto si

instance IsMessage P.TyRef'Foreign ForeignRef where
  fromProto fr = do
    si <- fromProto $ fr ^. P.sourceInfo
    mn <- fromProto $ fr ^. P.moduleName
    tn <- fromProto $ fr ^. P.tyName
    pure $ ForeignRef tn mn si

  toProto (ForeignRef tn mn si) =
    defMessage
      & P.tyName .~ toProto tn
      & P.moduleName .~ toProto mn
      & P.sourceInfo .~ toProto si

instance IsMessage P.TyRef TyRef where
  fromProto tr = case tr ^. P.maybe'tyRef of
    Nothing -> throwOneOfError (messageName (Proxy @P.TyRef)) "ty_ref"
    Just x -> case x of
      P.TyRef'LocalTyRef lr -> LocalI <$> fromProto lr
      P.TyRef'ForeignTyRef f -> ForeignI <$> fromProto f

  toProto = \case
    LocalI lr -> defMessage & P.localTyRef .~ toProto lr
    ForeignI fr -> defMessage & P.foreignTyRef .~ toProto fr

{-
    TyDef & Components
-}

instance IsMessage P.TyDef TyDef where
  fromProto td = do
    ctx <- ask
    ctxModuleName <- case ctx of
      CtxModule mn -> return mn
      _ -> throwInternalError "Expected to be in Module Context"
    local (const $ CtxTyDef ctxModuleName td) $ do
      tnm <- fromProto $ td ^. P.tyName
      tyabs <- fromProto $ td ^. P.tyAbs
      si <- fromProto $ td ^. P.sourceInfo
      pure $ TyDef tnm tyabs si

  toProto (TyDef tnm tyabs si) =
    defMessage
      & P.tyName .~ toProto tnm
      & P.tyAbs .~ toProto tyabs
      & P.sourceInfo .~ toProto si

instance IsMessage P.TyAbs TyAbs where
  fromProto ta = do
    (tyargs, mulTyArgs) <- parseAndIndex (\a -> stripSourceInfo $ a ^. #argName) (ta ^. P.tyArgs)
    tybody <- fromProto $ ta ^. P.tyBody
    si <- fromProto $ ta ^. P.sourceInfo
    ctx <- ask
    (ctxMn, ctxTyd) <- case ctx of
      CtxTyDef mn tyd -> return (mn, tyd)
      _ -> throwInternalError "Expected to be in TyDef Context"
    let mulArgsErrs =
          [ ProtoParseError $
            defMessage
              & P.multipleTyargError . P.moduleName .~ ctxMn
              & P.multipleTyargError . P.tyDef .~ ctxTyd
              & P.multipleTyargError . P.tyArgs .~ args
          | (_an, args) <- Map.toList mulTyArgs
          ]
    if null mulArgsErrs
      then pure $ TyAbs tyargs tybody si
      else throwError mulArgsErrs

  toProto (TyAbs tyargs tyabs si) =
    defMessage
      & P.tyArgs .~ (toProto <$> toList tyargs)
      & P.tyBody .~ toProto tyabs
      & P.sourceInfo .~ toProto si

instance IsMessage P.Kind'KindRef KindRefType where
  fromProto kr =
    ( \case
        P.Kind'KIND_REF_TYPE -> pure KType
        P.Kind'KIND_REF_UNSPECIFIED -> pure KUnspecified
        P.Kind'KindRef'Unrecognized v ->
          throwError
            [ ProtoParseError $
                defMessage
                  & P.unknownEnumError . P.enumName .~ Text.pack (showEnum kr)
                  & P.unknownEnumError . P.gotTag .~ (Text.pack . show $ v)
            ]
    )
      kr

  toProto = \case
    KType -> P.Kind'KIND_REF_TYPE
    KUnspecified -> P.Kind'KIND_REF_UNSPECIFIED

instance IsMessage P.Kind Kind where
  fromProto k = do
    kt <- case k ^. P.maybe'kind of
      Nothing -> throwOneOfError (messageName (Proxy @P.Kind)) "kind"
      Just k' -> case k' of
        P.Kind'KindRef r -> KindRef <$> fromProto r
        P.Kind'KindArrow' arr -> KindArrow <$> fromProto (arr ^. P.left) <*> fromProto (arr ^. P.right)
    pure $ Kind kt

  toProto (Kind (KindArrow l r)) = do
    defMessage
      & P.kindArrow . P.left .~ toProto l
      & P.kindArrow . P.right .~ toProto r
  toProto (Kind (KindRef r)) = do
    defMessage
      & P.kindRef .~ toProto r

instance IsMessage P.TyArg TyArg where
  fromProto ta = do
    argnm <- fromProto $ ta ^. P.argName
    si <- fromProto $ ta ^. P.sourceInfo
    kind <- fromProto $ ta ^. P.argKind
    pure $ TyArg argnm kind si

  toProto (TyArg argnm argkind si) =
    defMessage
      & P.argName .~ toProto argnm
      & P.argKind .~ toProto argkind
      & P.sourceInfo .~ toProto si

instance IsMessage P.TyBody TyBody where
  fromProto tb = case tb ^. P.maybe'tyBody of
    Nothing -> throwOneOfError (messageName (Proxy @P.TyBody)) "ty_body"
    Just x -> case x of
      P.TyBody'Opaque opq -> OpaqueI <$> fromProto (opq ^. P.sourceInfo)
      P.TyBody'Sum sumI -> SumI <$> fromProto sumI

  toProto = \case
    OpaqueI si ->
      let opaque = defMessage & P.sourceInfo .~ toProto si
       in defMessage & P.opaque .~ opaque
    SumI sb -> defMessage & P.sum .~ toProto sb

instance IsMessage P.Sum Sum where
  fromProto s = do
    (ctors, mulCtors) <- parseAndIndex (\c -> stripSourceInfo $ c ^. #constrName) (s ^. P.constructors)
    si <- fromProto $ s ^. P.sourceInfo
    ctx <- ask
    (ctxMn, ctxTyd) <- case ctx of
      CtxTyDef mn tyd -> return (mn, tyd)
      _ -> throwInternalError "Expected to be in TyDef Context"
    let mulCtorsErrs =
          [ ProtoParseError $
            defMessage
              & P.multipleConstructorError . P.moduleName .~ ctxMn
              & P.multipleConstructorError . P.tyDef .~ ctxTyd
              & P.multipleConstructorError . P.constructors .~ cs
          | (_cn, cs) <- Map.toList mulCtors
          ]
    if null mulCtorsErrs
      then pure $ Sum ctors si
      else throwError mulCtorsErrs

  toProto (Sum ctors si) =
    defMessage
      & P.constructors .~ toList (toProto <$> ctors)
      & P.sourceInfo .~ toProto si

instance IsMessage P.Sum'Constructor Constructor where
  fromProto c = do
    cnm <- fromProto $ c ^. P.constrName
    prod <- fromProto $ c ^. P.product
    pure $ Constructor cnm prod

  toProto (Constructor cnm prod) =
    defMessage
      & P.constrName .~ toProto cnm
      & P.product .~ toProto prod

instance IsMessage P.Product'Record Record where
  fromProto r = do
    (fields, mulFields) <- parseAndIndex (\f -> stripSourceInfo $ f ^. #fieldName) (r ^. P.fields)
    si <- fromProto $ r ^. P.sourceInfo
    ctx <- ask
    (ctxMn, ctxTyd) <- case ctx of
      CtxTyDef mn tyd -> return (mn, tyd)
      _ -> throwInternalError "Expected to be in TyDef Context"
    let mulFieldsErrs =
          [ ProtoParseError $
            defMessage
              & P.multipleFieldError . P.moduleName .~ ctxMn
              & P.multipleFieldError . P.tyDef .~ ctxTyd
              & P.multipleFieldError . P.fields .~ fs
          | (_fn, fs) <- Map.toList mulFields
          ]
    if null mulFieldsErrs
      then pure $ Record fields si
      else throwError mulFieldsErrs

  toProto (Record fs si) =
    defMessage
      & P.fields .~ (toProto <$> toList fs)
      & P.sourceInfo .~ toProto si

instance IsMessage P.Product'NTuple Tuple where
  fromProto r = do
    fs <- traverse fromProto $ r ^. P.fields
    si <- fromProto $ r ^. P.sourceInfo
    pure $ Tuple fs si

  toProto (Tuple fs si) =
    defMessage
      & P.fields .~ (toProto <$> fs)
      & P.sourceInfo .~ toProto si

instance IsMessage P.Product Product where
  fromProto p = case p ^. P.maybe'product of
    Nothing -> throwOneOfError (messageName (Proxy @P.Product)) "product"
    Just x -> case x of
      --- wrong, fix
      P.Product'Record' r -> do
        recrd <- fromProto r
        pure $ RecordI recrd
      P.Product'Ntuple t -> do
        tup <- fromProto t
        pure $ TupleI tup

  toProto = \case
    RecordI r -> defMessage & P.record .~ toProto r
    TupleI t -> defMessage & P.ntuple .~ toProto t

instance IsMessage P.Product'Record'Field Field where
  fromProto f = do
    fnm <- fromProto $ f ^. P.fieldName
    fty <- fromProto $ f ^. P.fieldTy
    pure $ Field fnm fty

  toProto (Field fnm fty) =
    defMessage
      & P.fieldName .~ toProto fnm
      & P.fieldTy .~ toProto fty

{-
    Classes, instances, constraints
-}

instance IsMessage P.TyClassRef'Local LocalClassRef where
  fromProto lr = do
    si <- fromProto $ lr ^. P.sourceInfo
    nm <- fromProto $ lr ^. P.className
    pure $ LocalClassRef nm si

  toProto (LocalClassRef nm si) =
    defMessage
      & P.className .~ toProto nm
      & P.sourceInfo .~ toProto si

instance IsMessage P.TyClassRef'Foreign ForeignClassRef where
  fromProto fr = do
    si <- fromProto $ fr ^. P.sourceInfo
    mn <- fromProto $ fr ^. P.moduleName
    tn <- fromProto $ fr ^. P.className
    pure $ ForeignClassRef tn mn si

  toProto (ForeignClassRef tn mn si) =
    defMessage
      & P.className .~ toProto tn
      & P.moduleName .~ toProto mn
      & P.sourceInfo .~ toProto si

instance IsMessage P.TyClassRef TyClassRef where
  fromProto tr = case tr ^. P.maybe'classRef of
    Nothing -> throwOneOfError (messageName (Proxy @P.TyClassRef)) "class_ref"
    Just x -> case x of
      P.TyClassRef'LocalClassRef lr -> LocalCI <$> fromProto lr
      P.TyClassRef'ForeignClassRef f -> ForeignCI <$> fromProto f

  toProto = \case
    LocalCI lr -> defMessage & P.localClassRef .~ toProto lr
    ForeignCI fr -> defMessage & P.foreignClassRef .~ toProto fr

instance IsMessage P.ClassDef ClassDef where
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
      pure $ ClassDef cnm carg sups doc si

  toProto (ClassDef cnm carg sups doc si) =
    defMessage
      & P.className .~ toProto cnm
      & P.classArgs .~ pure (toProto carg)
      & P.supers .~ (toProto <$> sups)
      & P.documentation .~ doc
      & P.sourceInfo .~ toProto si

instance IsMessage P.InstanceClause InstanceClause where
  fromProto ic = do
    si <- fromProto $ ic ^. P.sourceInfo
    cnm <- fromProto $ ic ^. P.classRef
    csts <- traverse fromProto $ ic ^. P.constraints
    args <- traverse fromProto $ ic ^. P.args
    arg <- case args of
      [] -> throwInternalError "Zero instance arguments, but zero parameter type classes are not supported"
      [x] -> return x
      _ -> throwInternalError "Multiple instance arguments, but multi parameter type classes are not supported"
    pure $ InstanceClause cnm arg csts si

  toProto (InstanceClause cnm hd csts si) =
    defMessage
      & P.classRef .~ toProto cnm
      & P.args .~ pure (toProto hd)
      & P.constraints .~ (toProto <$> csts)
      & P.sourceInfo .~ toProto si

instance IsMessage P.Constraint Constraint where
  fromProto c = do
    si <- fromProto $ c ^. P.sourceInfo
    cnm <- fromProto $ c ^. P.classRef
    args <- traverse fromProto $ c ^. P.arguments
    arg <- case args of
      [] -> throwInternalError "Zero constraint arguments, but zero parameter type classes are not supported"
      [x] -> return x
      _ -> throwInternalError "Multiple constraint arguments, but multi parameter type classes are not supported"
    pure $ Constraint cnm arg si

  toProto (Constraint cnm arg si) =
    defMessage
      & P.classRef .~ toProto cnm
      & P.arguments .~ pure (toProto arg)
      & P.sourceInfo .~ toProto si

{-
    Module, CompilerInput
-}

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

stripSourceInfo :: HasField "sourceInfo" s t a SourceInfo => s -> t
stripSourceInfo x = x & #sourceInfo .~ defSourceInfo

instance IsMessage P.Module Module where
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
            [ ProtoParseError $
              defMessage
                & P.multipleTydefError . P.moduleName .~ (m ^. P.moduleName)
                & P.multipleTydefError . P.tyDefs .~ tds
            | (_tn, tds) <- Map.toList mulTyDefs
            ]
          mulClassDefsErrs =
            [ ProtoParseError $
              defMessage
                & P.multipleClassdefError . P.moduleName .~ (m ^. P.moduleName)
                & P.multipleClassdefError . P.classDefs .~ cds
            | (_cn, cds) <- Map.toList mulClDefs
            ]
          mulImptsErrs =
            [ ProtoParseError $
              defMessage
                & P.multipleImportError . P.moduleName .~ (m ^. P.moduleName)
                & P.multipleImportError . P.imports .~ ims
            | (_in, ims) <- Map.toList mulImpts
            ]
          protoParseErrs = mulTyDefsErrs ++ mulClassDefsErrs ++ mulImptsErrs
      if null protoParseErrs
        then pure $ Module mnm tydefs cldefs insts (Map.keysSet impts) si
        else throwError protoParseErrs

  toProto (Module mnm tdefs cdefs insts impts si) =
    defMessage
      & P.moduleName .~ toProto mnm
      & P.typeDefs .~ (toProto <$> toList tdefs)
      & P.classDefs .~ (toProto <$> toList cdefs)
      & P.instances .~ (toProto <$> insts)
      & P.imports .~ (toProto <$> Set.toList impts)
      & P.sourceInfo .~ toProto si

instance IsMessage P.CompilerInput CompilerInput where
  fromProto ci = do
    local (const CtxCompilerInput) $ do
      (mods, mulModules) <- parseAndIndex (\m -> stripSourceInfo $ m ^. #moduleName) (ci ^. P.modules)
      let mulModulesErrs =
            [ ProtoParseError $
              defMessage & P.multipleModuleError . P.modules .~ ms
            | (_mn, ms) <- Map.toList mulModules
            ]
      if null mulModulesErrs
        then pure $ CompilerInput mods
        else throwError mulModulesErrs

  toProto (CompilerInput ms) =
    defMessage
      & P.modules .~ (toProto <$> toList ms)

{-
  Outputs
-}

instance IsMessage P.KindCheckError KindCheckError where
  fromProto kce =
    case kce ^. P.maybe'kindCheckError of
      Just x -> case x of
        P.KindCheckError'UnboundTermError' err ->
          UnboundTermError
            <$> fromProto (err ^. P.tyName)
            <*> fromProto (err ^. P.varName)
        P.KindCheckError'UnificationError err ->
          IncorrectApplicationError
            <$> fromProto (err ^. P.tyName)
            <*> fromProto (err ^. P.tyKind1)
            <*> fromProto (err ^. P.tyKind2)
        P.KindCheckError'RecursiveSubsError err ->
          RecursiveKindError
            <$> fromProto (err ^. P.tyName)
        P.KindCheckError'InconsistentTypeError' err ->
          InconsistentTypeError
            <$> fromProto (err ^. P.tyName)
            <*> fromProto (err ^. P.inferredKind)
            <*> fromProto (err ^. P.definedKind)
      Nothing -> throwOneOfError (messageName (Proxy @P.KindCheckError)) "kind_check_error"

  toProto = \case
    UnboundTermError tyname varname ->
      defMessage
        & (P.unboundTermError . P.tyName) .~ toProto tyname
        & (P.unboundTermError . P.varName) .~ toProto varname
    IncorrectApplicationError name k1 k2 ->
      defMessage
        & (P.unificationError . P.tyName) .~ toProto name
        & (P.unificationError . P.tyKind1) .~ toProto k1
        & (P.unificationError . P.tyKind2) .~ toProto k2
    RecursiveKindError err ->
      defMessage
        & (P.recursiveSubsError . P.tyName) .~ toProto err
    InconsistentTypeError name ki kd ->
      defMessage
        & (P.inconsistentTypeError . P.tyName) .~ toProto name
        & (P.inconsistentTypeError . P.inferredKind) .~ toProto ki
        & (P.inconsistentTypeError . P.definedKind) .~ toProto kd

-- instance IsMessage P.CompilerError CompilerError where
--   fromProto cErr = case cErr ^. P.maybe'compilerError of
--     Just x -> case x of
--       P.CompilerError'KindCheckError err -> CompKindCheckError <$> fromProto err
--       P.CompilerError'InternalError err -> InternalError <$> fromProto (err ^. P.internalError)
--     Nothing -> throwProtoError EmptyField

--   toProto = \case
--     CompKindCheckError err -> defMessage & P.kindCheckError .~ toProto err
--     InternalError err -> defMessage & (P.internalError . P.internalError) .~ toProto err

-- instance IsMessage P.CompilerResult CompilerResult where
--   fromProto c =
--     if c == defMessage
--       then pure CompilerResult
--       else throwProtoError EmptyField
--   toProto CompilerResult = defMessage

-- instance IsMessage P.CompilerOutput CompilerOutput where
--   fromProto co = case co ^. P.maybe'compilerOutput of
--     Just (P.CompilerOutput'CompilerResult res) -> Right <$> fromProto res
--     Just (P.CompilerOutput'CompilerError err) -> Left <$> fromProto err
--     Nothing -> throwProtoError EmptyField

--   toProto = \case
--     Right res -> defMessage & P.compilerResult .~ toProto res
--     Left err -> defMessage & P.compilerError .~ toProto err

-- | Convert from internal Kind to Proto Kind.
kind2ProtoKind :: K.Kind -> Kind
kind2ProtoKind = \case
  k1 K.:->: k2 -> Kind $ KindArrow (kind2ProtoKind k1) (kind2ProtoKind k2)
  K.Type -> Kind . KindRef $ KType
  K.KVar _ -> Kind . KindRef $ KUnspecified -- this shouldn't happen.

-- | Convert from internal Kind to Proto Kind.
protoKind2Kind :: Kind -> K.Kind
protoKind2Kind = \case
  Kind k -> case k of
    KindArrow k1 k2 -> protoKind2Kind k1 K.:->: protoKind2Kind k2
    KindRef KType -> K.Type
    KindRef KUnspecified -> K.KVar "Unspecified"
