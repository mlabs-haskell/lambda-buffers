{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE UndecidableInstances #-}

module LambdaBuffers.Common.ProtoCompat.IsMessage (IsMessage (..)) where

import Control.Lens (Getter, to, (&), (.~), (^.))
import Data.Foldable (toList)
import Data.Generics.Labels ()
import Data.Kind (Type)
import Data.List.NonEmpty (nonEmpty)
import Data.ProtoLens (defMessage)
import Data.Text (Text)
import GHC.Generics (Generic)
import Proto.Compiler qualified as P
import Proto.Compiler_Fields qualified as P

import LambdaBuffers.Common.ProtoCompat.NameValidation (
  NameValidator,
  classname,
  constrname,
  fieldname,
  tyname,
  varname,
 )
import LambdaBuffers.Common.ProtoCompat.Types (
  ClassDef (ClassDef),
  CompilerInput (CompilerInput),
  ConstrName (ConstrName),
  Constraint (Constraint),
  Constructor (Constructor),
  Field (Field),
  FieldName (FieldName),
  ForeignRef (ForeignRef),
  InstanceClause (InstanceClause),
  Kind (Kind),
  KindRefType (KType, KUnspecified),
  KindType (KindArrow, KindRef),
  LocalRef (LocalRef),
  Module (Module),
  ModuleName (ModuleName),
  ModuleNamePart (ModuleNamePart),
  Product (RecordI, TupleI),
  Record (Record),
  SourceInfo (SourceInfo),
  SourcePosition (SourcePosition),
  Sum (Sum),
  Tuple (Tuple),
  Ty (TyAppI, TyRefI, TyVarI),
  TyAbs (TyAbs),
  TyApp (TyApp),
  TyArg (TyArg),
  TyBody (OpaqueI, SumI),
  TyDef (TyDef),
  TyName (TyName),
  TyRef (ForeignI, LocalI),
  TyVar (TyVar),
  VarName (VarName), ClassName (ClassName),
 )
import Text.Megaparsec (parseMaybe)

note :: e -> Maybe a -> Either e a
note e = \case
  Nothing -> Left e
  Just a -> Right a

-- something like this probably exists in lens but i can't find it
traversing :: (Applicative f, Traversable t) => (a -> f b) -> Getter (t a) (f (t b))
traversing f = to $ \ta -> traverse f ta

data FromProtoErr
  = MultipleInstanceHeads TyRef [Ty] SourceInfo
  | NoInstanceHead TyRef SourceInfo
  | NoConstraintArgs TyRef SourceInfo
  | MultipleConstraintArgs TyRef [Ty] SourceInfo
  | NoClassArgs ClassName SourceInfo
  | MultipleClassArgs ClassName SourceInfo
  | NoTyAppArgs SourceInfo
  | EmptyRecordBody SourceInfo -- Ideally we should catch & rethrow this and the next one when we have access to the tyname
  | EmptySumBody SourceInfo
  | EmptyName SourceInfo
  | EmptyField
  | NegativeArity VarName SourceInfo
  | OneOfNotSet Text
  | BadName Text SourceInfo
  deriving stock (Show, Eq, Ord, Generic)

checkName :: NameValidator -> SourceInfo -> Text -> Either FromProtoErr Text
checkName v si txt = note (BadName txt si) $ parseMaybe v txt

class IsMessage (proto :: Type) (good :: Type) where
  fromProto :: proto -> Either FromProtoErr good

  toProto :: good -> proto

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

instance IsMessage P.FieldName FieldName where
  fromProto v = do
    n' <- fromProto $ v ^. P.name
    si <- fromProto $ v ^. P.sourceInfo
    n <- checkName fieldname si n'
    pure $ FieldName n si

  toProto (FieldName n si) =
    defMessage
      & P.name .~ toProto n
      & P.sourceInfo .~ toProto si

instance IsMessage P.ConstrName ConstrName where
  fromProto v = do
    n' <- fromProto $ v ^. P.name
    si <- fromProto $ v ^. P.sourceInfo
    n <- checkName constrname si n'
    pure $ ConstrName n si

  toProto (ConstrName n si) =
    defMessage
      & P.name .~ toProto n
      & P.sourceInfo .~ toProto si

instance IsMessage P.TyName TyName where
  fromProto v = do
    n' <- fromProto $ v ^. P.name
    si <- fromProto $ v ^. P.sourceInfo
    n <- checkName tyname si n'
    pure $ TyName n si

  toProto (TyName n si) =
    defMessage
      & P.name .~ toProto n
      & P.sourceInfo .~ toProto si

instance IsMessage P.VarName VarName where
  fromProto v = do
    n' <- fromProto $ v ^. P.name
    si <- fromProto $ v ^. P.sourceInfo
    n <- checkName varname si n'
    pure $ VarName n si

  toProto (VarName n si) =
    defMessage
      & P.name .~ toProto n
      & P.sourceInfo .~ toProto si

instance IsMessage P.ClassName ClassName where
  fromProto v = do
    n' <- fromProto $ v ^. P.name
    si <- fromProto $ v ^. P.sourceInfo
    n <- checkName varname si n'
    pure $ ClassName n si

  toProto (ClassName n si) =
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
    targs' <- ta ^. (P.tyArgs . traversing fromProto)
    targs <- note (NoTyAppArgs si) $ nonEmpty targs'
    pure $ TyApp tf targs si

  toProto (TyApp tf args si) =
    defMessage
      & P.tyFunc .~ toProto tf
      & P.tyArgs .~ toList (toProto <$> args)
      & P.sourceInfo .~ toProto si

instance IsMessage P.Ty Ty where
  fromProto ti = case ti ^. P.maybe'ty of
    Nothing -> Left $ OneOfNotSet "ty"
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
    Nothing -> Left $ OneOfNotSet "ty_ref"
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
    tyvars <- traverse fromProto $ ta ^. P.tyArgs
    tybody <- fromProto $ ta ^. P.tyBody
    si <- fromProto $ ta ^. P.sourceInfo
    pure $ TyAbs tyvars tybody si

  toProto (TyAbs tyvars tyabs si) =
    defMessage
      & P.tyArgs .~ (toProto <$> tyvars)
      & P.tyBody .~ toProto tyabs
      & P.sourceInfo .~ toProto si

instance IsMessage P.Kind'KindRef KindRefType where
  fromProto = \case
    P.Kind'KIND_REF_TYPE -> pure KType
    P.Kind'KIND_REF_UNSPECIFIED -> pure KUnspecified
    P.Kind'KindRef'Unrecognized _ -> undefined -- FIXME

  toProto = \case
    KType -> P.Kind'KIND_REF_TYPE
    KUnspecified -> P.Kind'KIND_REF_UNSPECIFIED

instance IsMessage P.Kind Kind where
  fromProto k = do
    si <- fromProto $ k ^. P.sourceInfo
    kt <- case k ^. P.maybe'kind of
      Just (P.Kind'KindRef r) -> KindRef <$> fromProto r
      Just (P.Kind'KindArrow' arr) -> KindArrow <$> fromProto (arr ^. P.left) <*> fromProto (arr ^. P.right)
      _ -> Left EmptyField
    pure $ Kind kt si

  toProto (Kind (KindArrow l r) si) = do
    defMessage
      & P.kindArrow . P.left .~ toProto l
      & P.kindArrow . P.right .~ toProto r
      & P.sourceInfo .~ toProto si
  toProto (Kind (KindRef r) si) = do
    defMessage
      & P.kindRef .~ toProto r
      & P.sourceInfo .~ toProto si

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
    Nothing -> Left $ OneOfNotSet "tyBody"
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
    si <- fromProto $ s ^. P.sourceInfo
    ctors' <- s ^. (P.constructors . traversing fromProto)
    ctors <- note (EmptySumBody si) $ nonEmpty ctors'
    pure $ Sum ctors si

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
    fs' <- traverse fromProto $ r ^. P.fields
    si <- fromProto $ r ^. P.sourceInfo
    fs <- note (EmptyRecordBody si) $ nonEmpty fs'
    pure $ Record fs si

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
    Nothing -> Left $ OneOfNotSet "product"
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

instance IsMessage P.ClassDef ClassDef where
  fromProto cd = do
    si <- fromProto $ cd ^. P.sourceInfo
    cnm <- fromProto $ cd ^. P.className
    cargs <- traverse fromProto $ cd ^. P.classArgs
    carg <- case cargs of
      [] -> Left $ NoClassArgs cnm si
      [x] -> Right x
      _ -> Left $ MultipleClassArgs cnm si
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
    hds <- ic ^. (P.heads . traversing fromProto)
    hd <- case hds of
      [] -> Left $ NoInstanceHead cnm si
      [x] -> Right x
      xs -> Left $ MultipleInstanceHeads cnm xs si
    pure $ InstanceClause cnm hd csts si

  toProto (InstanceClause cnm hd csts si) =
    defMessage
      & P.classRef .~ toProto cnm
      & P.heads .~ pure (toProto hd)
      & P.constraints .~ (toProto <$> csts)
      & P.sourceInfo .~ toProto si

instance IsMessage P.Constraint Constraint where
  fromProto c = do
    si <- fromProto $ c ^. P.sourceInfo
    cnm <- fromProto $ c ^. P.classRef
    args <- c ^. (P.arguments . traversing fromProto)
    arg <- case args of
      [] -> Left $ NoConstraintArgs cnm si
      [x] -> Right x
      xs -> Left $ MultipleConstraintArgs cnm xs si
    pure $ Constraint cnm arg si

  toProto (Constraint cnm arg si) =
    defMessage
      & P.classRef .~ toProto cnm
      & P.arguments .~ pure (toProto arg)
      & P.sourceInfo .~ toProto si

{-
    Module, CompilerInput
-}

instance IsMessage P.Module Module where
  fromProto m = do
    mnm <- fromProto $ m ^. P.moduleName
    tdefs <- traverse fromProto $ m ^. P.typeDefs
    cdefs <- traverse fromProto $ m ^. P.classDefs
    insts <- traverse fromProto $ m ^. P.instances
    si <- fromProto $ m ^. P.sourceInfo
    pure $ Module mnm tdefs cdefs insts si

  toProto (Module mnm tdefs cdefs insts si) =
    defMessage
      & P.moduleName .~ toProto mnm
      & P.typeDefs .~ (toProto <$> tdefs)
      & P.classDefs .~ (toProto <$> cdefs)
      & P.instances .~ (toProto <$> insts)
      & P.sourceInfo .~ toProto si

instance IsMessage P.CompilerInput CompilerInput where
  fromProto ci = do
    ms <- traverse fromProto $ ci ^. P.modules
    pure $ CompilerInput ms

  toProto (CompilerInput ms) =
    defMessage
      & P.modules .~ (toProto <$> ms)

{-
    Names
-}

instance IsMessage Text Text where
  fromProto = pure
  toProto = id

instance IsMessage P.ModuleNamePart ModuleNamePart where
  fromProto mnp = do
    nm <- fromProto $ mnp ^. P.name
    si <- fromProto $ mnp ^. P.sourceInfo
    pure $ ModuleNamePart nm si

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
