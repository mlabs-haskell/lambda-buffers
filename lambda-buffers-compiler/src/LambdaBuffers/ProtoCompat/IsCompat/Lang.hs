{-# OPTIONS_GHC -Wno-orphans #-}

module LambdaBuffers.ProtoCompat.IsCompat.Lang (
  ) where

import Control.Lens ((&), (.~), (^.))
import Control.Monad.Except (MonadError (throwError))
import Control.Monad.Reader (MonadReader (ask, local))
import Data.Foldable (toList)
import Data.Map qualified as Map
import Data.ProtoLens (Message (messageName), MessageEnum (showEnum), defMessage)
import Data.Proxy (Proxy (Proxy))
import Data.Text (Text)
import Data.Text qualified as Text
import LambdaBuffers.Compiler.NamingCheck (checkClassName, checkConstrName, checkFieldName, checkModuleNamePart, checkTyName, checkVarName)
import LambdaBuffers.ProtoCompat.InfoLess (mkInfoLess)
import LambdaBuffers.ProtoCompat.IsCompat.FromProto (FromProtoContext (CtxClassDef, CtxModule, CtxTop, CtxTyDef), FromProtoErr (FPProtoParseError), IsCompat (fromProto, toProto), throwInternalError, throwNamingError, throwOneOfError)
import LambdaBuffers.ProtoCompat.IsCompat.Utils (parseAndIndex, parseAndIndex')
import LambdaBuffers.ProtoCompat.Types qualified as Compat
import Proto.Compiler_Fields qualified as Compiler
import Proto.Lang qualified as Lang
import Proto.Lang_Fields qualified as Lang

{-
    SourceInfo
-}

instance IsCompat Lang.SourcePosition Compat.SourcePosition where
  fromProto sp = do
    let col = fromIntegral $ sp ^. Lang.column
        row = fromIntegral $ sp ^. Lang.row
    pure $ Compat.SourcePosition col row

  toProto sp =
    defMessage
      & Lang.column .~ fromIntegral (sp ^. #column)
      & Lang.row .~ fromIntegral (sp ^. #row)

instance IsCompat Lang.SourceInfo Compat.SourceInfo where
  fromProto si = do
    let file = si ^. Lang.file
    pFrom <- fromProto $ si ^. Lang.posFrom
    pTo <- fromProto $ si ^. Lang.posTo
    pure $ Compat.SourceInfo file pFrom pTo

  toProto si =
    defMessage
      & Lang.file .~ (si ^. #file)
      & Lang.posFrom .~ toProto (si ^. #posFrom)
      & Lang.posTo .~ toProto (si ^. #posTo)

{-
    Names
-}

instance IsCompat Text Text where
  fromProto = pure
  toProto = id

instance IsCompat Lang.ModuleNamePart Compat.ModuleNamePart where
  fromProto mnp = do
    si <- fromProto $ mnp ^. Compiler.sourceInfo
    throwNamingError $ checkModuleNamePart mnp
    pure $ Compat.ModuleNamePart (mnp ^. Lang.name) si

  toProto (Compat.ModuleNamePart nm si) =
    defMessage
      & Lang.name .~ nm
      & Compiler.sourceInfo .~ toProto si

instance IsCompat Lang.ModuleName Compat.ModuleName where
  fromProto mn = do
    si <- fromProto $ mn ^. Compiler.sourceInfo
    parts <- traverse fromProto $ mn ^. Lang.parts
    pure $ Compat.ModuleName parts si

  toProto (Compat.ModuleName parts si) =
    defMessage
      & Lang.parts .~ (toProto <$> parts)
      & Lang.sourceInfo .~ toProto si

instance IsCompat Lang.FieldName Compat.FieldName where
  fromProto v = do
    throwNamingError $ checkFieldName v
    Compat.FieldName <$> fromProto (v ^. Lang.name) <*> fromProto (v ^. Compiler.sourceInfo)

  toProto (Compat.FieldName n si) =
    defMessage
      & Lang.name .~ toProto n
      & Lang.sourceInfo .~ toProto si

instance IsCompat Lang.ConstrName Compat.ConstrName where
  fromProto v = do
    throwNamingError $ checkConstrName v
    Compat.ConstrName <$> fromProto (v ^. Lang.name) <*> fromProto (v ^. Lang.sourceInfo)

  toProto (Compat.ConstrName n si) =
    defMessage
      & Lang.name .~ toProto n
      & Lang.sourceInfo .~ toProto si

instance IsCompat Lang.TyName Compat.TyName where
  fromProto v = do
    throwNamingError $ checkTyName v
    Compat.TyName <$> fromProto (v ^. Lang.name) <*> fromProto (v ^. Lang.sourceInfo)

  toProto (Compat.TyName n si) =
    defMessage
      & Lang.name .~ toProto n
      & Lang.sourceInfo .~ toProto si

instance IsCompat Lang.ClassName Compat.ClassName where
  fromProto v = do
    throwNamingError $ checkClassName v
    Compat.ClassName <$> fromProto (v ^. Lang.name) <*> fromProto (v ^. Lang.sourceInfo)

  toProto (Compat.ClassName n si) =
    defMessage
      & Lang.name .~ toProto n
      & Lang.sourceInfo .~ toProto si

instance IsCompat Lang.VarName Compat.VarName where
  fromProto v = do
    throwNamingError $ checkVarName v
    Compat.VarName <$> fromProto (v ^. Lang.name) <*> fromProto (v ^. Lang.sourceInfo)

  toProto (Compat.VarName n si) =
    defMessage
      & Lang.name .~ toProto n
      & Lang.sourceInfo .~ toProto si

{-
    Ty & Components
-}

instance IsCompat Lang.TyVar Compat.TyVar where
  fromProto tv = do
    vn <- fromProto $ tv ^. Lang.varName
    pure $ Compat.TyVar vn

  toProto (Compat.TyVar vn) =
    defMessage
      & Lang.varName .~ toProto vn

instance IsCompat Lang.TyApp Compat.TyApp where
  fromProto ta = do
    tf <- fromProto $ ta ^. Lang.tyFunc
    si <- fromProto $ ta ^. Lang.sourceInfo
    targs <- traverse fromProto $ ta ^. Lang.tyArgs
    pure $ Compat.TyApp tf targs si

  toProto (Compat.TyApp tf args si) =
    defMessage
      & Lang.tyFunc .~ toProto tf
      & Lang.tyArgs .~ toList (toProto <$> args)
      & Lang.sourceInfo .~ toProto si

instance IsCompat Lang.Ty Compat.Ty where
  fromProto ti = case ti ^. Lang.maybe'ty of
    Nothing -> throwOneOfError (messageName (Proxy @Lang.Ty)) "ty"
    Just x -> case x of
      Lang.Ty'TyVar tv -> Compat.TyVarI <$> fromProto tv
      Lang.Ty'TyApp ta -> Compat.TyAppI <$> fromProto ta
      Lang.Ty'TyRef tr -> Compat.TyRefI <$> fromProto tr

  toProto = \case
    Compat.TyVarI tv -> defMessage & Lang.tyVar .~ toProto tv
    Compat.TyRefI tr -> defMessage & Lang.tyRef .~ toProto tr
    Compat.TyAppI ta -> defMessage & Lang.tyApp .~ toProto ta

instance IsCompat Lang.TyRef'Local Compat.LocalRef where
  fromProto lr = do
    si <- fromProto $ lr ^. Lang.sourceInfo
    nm <- fromProto $ lr ^. Lang.tyName
    pure $ Compat.LocalRef nm si

  toProto (Compat.LocalRef nm si) =
    defMessage
      & Lang.tyName .~ toProto nm
      & Lang.sourceInfo .~ toProto si

instance IsCompat Lang.TyRef'Foreign Compat.ForeignRef where
  fromProto fr = do
    si <- fromProto $ fr ^. Lang.sourceInfo
    mn <- fromProto $ fr ^. Lang.moduleName
    tn <- fromProto $ fr ^. Lang.tyName
    pure $ Compat.ForeignRef tn mn si

  toProto (Compat.ForeignRef tn mn si) =
    defMessage
      & Lang.tyName .~ toProto tn
      & Lang.moduleName .~ toProto mn
      & Lang.sourceInfo .~ toProto si

instance IsCompat Lang.TyRef Compat.TyRef where
  fromProto tr = case tr ^. Lang.maybe'tyRef of
    Nothing -> throwOneOfError (messageName (Proxy @Lang.TyRef)) "ty_ref"
    Just x -> case x of
      Lang.TyRef'LocalTyRef lr -> Compat.LocalI <$> fromProto lr
      Lang.TyRef'ForeignTyRef f -> Compat.ForeignI <$> fromProto f

  toProto = \case
    Compat.LocalI lr -> defMessage & Lang.localTyRef .~ toProto lr
    Compat.ForeignI fr -> defMessage & Lang.foreignTyRef .~ toProto fr

{-
    TyDef & Components
-}

instance IsCompat Lang.TyDef Compat.TyDef where
  fromProto td = do
    ctx <- ask
    ctxModuleName <- case ctx of
      CtxModule mn -> return mn
      _ -> throwInternalError "Expected to be in Module Context"
    local (const $ CtxTyDef ctxModuleName td) $ do
      tnm <- fromProto $ td ^. Lang.tyName
      tyabs <- fromProto $ td ^. Lang.tyAbs
      si <- fromProto $ td ^. Lang.sourceInfo
      pure $ Compat.TyDef tnm tyabs si

  toProto (Compat.TyDef tnm tyabs si) =
    defMessage
      & Lang.tyName .~ toProto tnm
      & Lang.tyAbs .~ toProto tyabs
      & Lang.sourceInfo .~ toProto si

instance IsCompat Lang.TyAbs Compat.TyAbs where
  fromProto ta = do
    (tyargs, mulTyArgs) <- parseAndIndex' (\a -> mkInfoLess $ a ^. #argName) (ta ^. Lang.tyArgs)
    tybody <- fromProto $ ta ^. Lang.tyBody
    si <- fromProto $ ta ^. Lang.sourceInfo
    ctx <- ask
    (ctxMn, ctxTyd) <- case ctx of
      CtxTyDef mn tyd -> return (mn, tyd)
      _ -> throwInternalError "Expected to be in TyDef Context"
    let mulArgsErrs =
          [ FPProtoParseError $
            defMessage
              & Compiler.multipleTyargError . Compiler.moduleName .~ ctxMn
              & Compiler.multipleTyargError . Compiler.tyDef .~ ctxTyd
              & Compiler.multipleTyargError . Compiler.tyArgs .~ args
          | (_an, args) <- Map.toList mulTyArgs
          ]
    if null mulArgsErrs
      then pure $ Compat.TyAbs tyargs tybody si
      else throwError mulArgsErrs

  toProto (Compat.TyAbs tyargs tyabs si) =
    defMessage
      & Lang.tyArgs .~ (toProto <$> toList tyargs)
      & Lang.tyBody .~ toProto tyabs
      & Lang.sourceInfo .~ toProto si

instance IsCompat Lang.Kind'KindRef Compat.KindRefType where
  fromProto kr =
    ( \case
        Lang.Kind'KIND_REF_TYPE -> pure Compat.KType
        Lang.Kind'KIND_REF_UNSPECIFIED -> pure Compat.KUnspecified
        Lang.Kind'KindRef'Unrecognized v ->
          throwError
            [ FPProtoParseError $
                defMessage
                  & Compiler.unknownEnumError . Compiler.enumName .~ Text.pack (showEnum kr)
                  & Compiler.unknownEnumError . Compiler.gotTag .~ (Text.pack . show $ v)
            ]
    )
      kr

  toProto = \case
    Compat.KType -> Lang.Kind'KIND_REF_TYPE
    Compat.KUnspecified -> Lang.Kind'KIND_REF_UNSPECIFIED

instance IsCompat Lang.Kind Compat.Kind where
  fromProto k = do
    kt <- case k ^. Lang.maybe'kind of
      Nothing -> throwOneOfError (messageName (Proxy @Lang.Kind)) "kind"
      Just k' -> case k' of
        Lang.Kind'KindRef r -> Compat.KindRef <$> fromProto r
        Lang.Kind'KindArrow' arr -> Compat.KindArrow <$> fromProto (arr ^. Lang.left) <*> fromProto (arr ^. Lang.right)
    pure $ Compat.Kind kt

  toProto (Compat.Kind (Compat.KindArrow l r)) = do
    defMessage
      & Lang.kindArrow . Lang.left .~ toProto l
      & Lang.kindArrow . Lang.right .~ toProto r
  toProto (Compat.Kind (Compat.KindRef r)) = do
    defMessage
      & Lang.kindRef .~ toProto r

instance IsCompat Lang.TyArg Compat.TyArg where
  fromProto ta = do
    argnm <- fromProto $ ta ^. Lang.argName
    si <- fromProto $ ta ^. Lang.sourceInfo
    kind <- fromProto $ ta ^. Lang.argKind
    pure $ Compat.TyArg argnm kind si

  toProto (Compat.TyArg argnm argkind si) =
    defMessage
      & Lang.argName .~ toProto argnm
      & Lang.argKind .~ toProto argkind
      & Lang.sourceInfo .~ toProto si

instance IsCompat Lang.TyBody Compat.TyBody where
  fromProto tb = case tb ^. Lang.maybe'tyBody of
    Nothing -> throwOneOfError (messageName (Proxy @Lang.TyBody)) "ty_body"
    Just x -> case x of
      Lang.TyBody'Opaque opq -> Compat.OpaqueI <$> fromProto (opq ^. Lang.sourceInfo)
      Lang.TyBody'Sum sumI -> Compat.SumI <$> fromProto sumI
      Lang.TyBody'Product prodI -> Compat.ProductI <$> fromProto prodI
      Lang.TyBody'Record recI -> Compat.RecordI <$> fromProto recI

  toProto = \case
    Compat.OpaqueI si ->
      let opaque = defMessage & Lang.sourceInfo .~ toProto si
       in defMessage & Lang.opaque .~ opaque
    Compat.SumI s -> defMessage & Lang.sum .~ toProto s
    Compat.ProductI p -> defMessage & Lang.product .~ toProto p
    Compat.RecordI r -> defMessage & Lang.record .~ toProto r

instance IsCompat Lang.Sum Compat.Sum where
  fromProto s = do
    (ctors, mulCtors) <- parseAndIndex' (\c -> mkInfoLess $ c ^. #constrName) (s ^. Lang.constructors)
    si <- fromProto $ s ^. Lang.sourceInfo
    ctx <- ask
    (ctxMn, ctxTyd) <- case ctx of
      CtxTyDef mn tyd -> return (mn, tyd)
      _ -> throwInternalError "Expected to be in TyDef Context"
    let mulCtorsErrs =
          [ FPProtoParseError $
            defMessage
              & Compiler.multipleConstructorError . Compiler.moduleName .~ ctxMn
              & Compiler.multipleConstructorError . Compiler.tyDef .~ ctxTyd
              & Compiler.multipleConstructorError . Compiler.constructors .~ cs
          | (_cn, cs) <- Map.toList mulCtors
          ]
    if null mulCtorsErrs
      then pure $ Compat.Sum ctors si
      else throwError mulCtorsErrs

  toProto (Compat.Sum ctors si) =
    defMessage
      & Lang.constructors .~ toList (toProto <$> ctors)
      & Lang.sourceInfo .~ toProto si

instance IsCompat Lang.Sum'Constructor Compat.Constructor where
  fromProto c = do
    cnm <- fromProto $ c ^. Lang.constrName
    prod <- fromProto $ c ^. Lang.product
    pure $ Compat.Constructor cnm prod

  toProto (Compat.Constructor cnm prod) =
    defMessage
      & Lang.constrName .~ toProto cnm
      & Lang.product .~ toProto prod

instance IsCompat Lang.Record Compat.Record where
  fromProto r = do
    (fields, mulFields) <- parseAndIndex' (\f -> mkInfoLess $ f ^. #fieldName) (r ^. Lang.fields)
    si <- fromProto $ r ^. Lang.sourceInfo
    ctx <- ask
    (ctxMn, ctxTyd) <- case ctx of
      CtxTyDef mn tyd -> return (mn, tyd)
      _ -> throwInternalError "Expected to be in TyDef Context"
    let mulFieldsErrs =
          [ FPProtoParseError $
            defMessage
              & Compiler.multipleFieldError . Compiler.moduleName .~ ctxMn
              & Compiler.multipleFieldError . Compiler.tyDef .~ ctxTyd
              & Compiler.multipleFieldError . Compiler.fields .~ fs
          | (_fn, fs) <- Map.toList mulFields
          ]
    if null mulFieldsErrs
      then pure $ Compat.Record fields si
      else throwError mulFieldsErrs

  toProto (Compat.Record fs si) =
    defMessage
      & Lang.fields .~ (toProto <$> toList fs)
      & Lang.sourceInfo .~ toProto si

instance IsCompat Lang.Product Compat.Product where
  fromProto r = do
    fs <- traverse fromProto $ r ^. Lang.fields
    si <- fromProto $ r ^. Lang.sourceInfo
    pure $ Compat.Product fs si

  toProto (Compat.Product fs si) =
    defMessage
      & Lang.fields .~ (toProto <$> fs)
      & Lang.sourceInfo .~ toProto si

instance IsCompat Lang.Record'Field Compat.Field where
  fromProto f = do
    fnm <- fromProto $ f ^. Lang.fieldName
    fty <- fromProto $ f ^. Lang.fieldTy
    pure $ Compat.Field fnm fty

  toProto (Compat.Field fnm fty) =
    defMessage
      & Lang.fieldName .~ toProto fnm
      & Lang.fieldTy .~ toProto fty

{-
    Classes, instances, constraints
-}

instance IsCompat Lang.TyClassRef'Local Compat.LocalClassRef where
  fromProto lr = do
    si <- fromProto $ lr ^. Lang.sourceInfo
    nm <- fromProto $ lr ^. Lang.className
    pure $ Compat.LocalClassRef nm si

  toProto (Compat.LocalClassRef nm si) =
    defMessage
      & Lang.className .~ toProto nm
      & Lang.sourceInfo .~ toProto si

instance IsCompat Lang.TyClassRef'Foreign Compat.ForeignClassRef where
  fromProto fr = do
    si <- fromProto $ fr ^. Lang.sourceInfo
    mn <- fromProto $ fr ^. Lang.moduleName
    tn <- fromProto $ fr ^. Lang.className
    pure $ Compat.ForeignClassRef tn mn si

  toProto (Compat.ForeignClassRef tn mn si) =
    defMessage
      & Lang.className .~ toProto tn
      & Lang.moduleName .~ toProto mn
      & Lang.sourceInfo .~ toProto si

instance IsCompat Lang.TyClassRef Compat.TyClassRef where
  fromProto tr = case tr ^. Lang.maybe'classRef of
    Nothing -> throwOneOfError (messageName (Proxy @Lang.TyClassRef)) "class_ref"
    Just x -> case x of
      Lang.TyClassRef'LocalClassRef lr -> Compat.LocalCI <$> fromProto lr
      Lang.TyClassRef'ForeignClassRef f -> Compat.ForeignCI <$> fromProto f

  toProto = \case
    Compat.LocalCI lr -> defMessage & Lang.localClassRef .~ toProto lr
    Compat.ForeignCI fr -> defMessage & Lang.foreignClassRef .~ toProto fr

instance IsCompat Lang.ClassDef Compat.ClassDef where
  fromProto cd = do
    ctx <- ask
    ctxMn <- case ctx of
      CtxModule mn -> return mn
      _ -> throwInternalError "Expected to be in Module Context"
    local (const $ CtxClassDef ctxMn cd) $ do
      si <- fromProto $ cd ^. Lang.sourceInfo
      cnm <- fromProto $ cd ^. Lang.className
      cargs <- traverse fromProto $ cd ^. Lang.classArgs
      carg <- case cargs of
        [] -> throwInternalError "Zero parameter type classes are not supported"
        [x] -> return x
        _ -> throwInternalError "Multi parameter type classes are not supported"
      sups <- traverse fromProto $ cd ^. Lang.supers
      let doc = cd ^. Lang.documentation
      pure $ Compat.ClassDef cnm carg sups doc si

  toProto (Compat.ClassDef cnm carg sups doc si) =
    defMessage
      & Lang.className .~ toProto cnm
      & Lang.classArgs .~ pure (toProto carg)
      & Lang.supers .~ (toProto <$> sups)
      & Lang.documentation .~ doc
      & Lang.sourceInfo .~ toProto si

instance IsCompat Lang.ClassConstraint Compat.ClassConstraint where
  fromProto cc = do
    cr <- fromProto $ cc ^. Lang.classRef
    args <- traverse fromProto $ cc ^. Lang.args
    arg <- case args of
      [] -> throwInternalError "ClassConstraint: Zero parameter type classes are not supported"
      [x] -> return x
      _ -> throwInternalError "ClassConstraint: Multi parameter type classes are not supported"
    pure $ Compat.ClassConstraint cr arg

  toProto (Compat.ClassConstraint cr arg) =
    defMessage
      & Lang.classRef .~ toProto cr
      & Lang.args .~ [toProto arg]

instance IsCompat Lang.InstanceClause Compat.InstanceClause where
  fromProto ic = do
    si <- fromProto $ ic ^. Lang.sourceInfo
    hd <- fromProto $ ic ^. Lang.head
    body <- traverse fromProto $ ic ^. Lang.constraints
    pure $ Compat.InstanceClause hd body si

  toProto (Compat.InstanceClause hd cstrs si) =
    defMessage
      & Lang.head .~ toProto hd
      & Lang.constraints .~ (toProto <$> cstrs)
      & Lang.sourceInfo .~ toProto si

instance IsCompat Lang.Derive Compat.Derive where
  fromProto c = do
    cstr <- fromProto $ c ^. Lang.constraint
    pure $ Compat.Derive cstr

  toProto (Compat.Derive cstr) =
    defMessage
      & Lang.constraint .~ toProto cstr

instance IsCompat Lang.Constraint Compat.Constraint where
  fromProto c = do
    si <- fromProto $ c ^. Lang.sourceInfo
    cnm <- fromProto $ c ^. Lang.classRef
    args <- traverse fromProto $ c ^. Lang.args
    arg <- case args of
      [] -> throwInternalError "Zero constraint arguments, but zero parameter type classes are not supported"
      [x] -> return x
      _ -> throwInternalError "Multiple constraint arguments, but multi parameter type classes are not supported"
    pure $ Compat.Constraint cnm arg si

  toProto (Compat.Constraint cnm arg si) =
    defMessage
      & Lang.classRef .~ toProto cnm
      & Lang.args .~ pure (toProto arg)
      & Lang.sourceInfo .~ toProto si

{-
    Module
-}

instance IsCompat Lang.Module Compat.Module where
  fromProto m = do
    ctx <- ask
    case ctx of
      CtxTop -> return ()
      _ -> throwInternalError "Expected to be in the Top Context"
    local (const $ CtxModule (m ^. Lang.moduleName)) $ do
      mnm <- fromProto $ m ^. Lang.moduleName
      (tydefs, mulTyDefs) <- parseAndIndex (\tyDef -> mkInfoLess $ tyDef ^. #tyName) (m ^. Lang.typeDefs)
      (cldefs, mulClDefs) <- parseAndIndex (\cldef -> mkInfoLess $ cldef ^. #className) (m ^. Lang.classDefs)
      (impts, mulImpts) <- parseAndIndex mkInfoLess (m ^. Lang.imports)
      insts <- traverse fromProto $ m ^. Lang.instances
      derives <- traverse fromProto $ m ^. Lang.derives
      si <- fromProto $ m ^. Lang.sourceInfo
      let mulTyDefsErrs =
            [ FPProtoParseError $
              defMessage
                & Compiler.multipleTydefError . Compiler.moduleName .~ (m ^. Compiler.moduleName)
                & Compiler.multipleTydefError . Compiler.tyDefs .~ tds
            | (_tn, tds) <- Map.toList mulTyDefs
            ]
          mulClassDefsErrs =
            [ FPProtoParseError $
              defMessage
                & Compiler.multipleClassdefError . Compiler.moduleName .~ (m ^. Compiler.moduleName)
                & Compiler.multipleClassdefError . Compiler.classDefs .~ cds
            | (_cn, cds) <- Map.toList mulClDefs
            ]
          mulImptsErrs =
            [ FPProtoParseError $
              defMessage
                & Compiler.multipleImportError . Compiler.moduleName .~ (m ^. Compiler.moduleName)
                & Compiler.multipleImportError . Compiler.imports .~ ims
            | (_in, ims) <- Map.toList mulImpts
            ]
          protoParseErrs = mulTyDefsErrs ++ mulClassDefsErrs ++ mulImptsErrs
      if null protoParseErrs
        then pure $ Compat.Module mnm tydefs cldefs insts derives impts si
        else throwError protoParseErrs

  toProto (Compat.Module mnm tdefs cdefs insts drv impts si) =
    defMessage
      & Lang.moduleName .~ toProto mnm
      & Lang.typeDefs .~ (toProto <$> toList tdefs)
      & Lang.classDefs .~ (toProto <$> toList cdefs)
      & Lang.instances .~ (toProto <$> insts)
      & Lang.derives .~ (toProto <$> drv)
      & Lang.imports .~ (toProto <$> toList impts)
      & Lang.sourceInfo .~ toProto si
