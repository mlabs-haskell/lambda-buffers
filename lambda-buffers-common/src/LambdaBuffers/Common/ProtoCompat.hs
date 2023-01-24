{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedLabels #-}

module LambdaBuffers.Common.ProtoCompat () where

import Control.Lens (Getter, to, (&), (.~), (^.))
import Data.Foldable (toList)
import Data.Generics.Labels ()
import Data.Kind (Type)
import Data.List.NonEmpty (NonEmpty, nonEmpty)
import Data.ProtoLens (defMessage)
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Generics (Generic)
import Proto.Compiler qualified as P
import Proto.Compiler_Fields qualified as P

note :: e -> Maybe a -> Either e a
note e = \case
  Nothing -> Left e
  Just a -> Right a

-- something like this probably exists in lens but i can't find it
traversing :: (Applicative f, Traversable t) => (a -> f b) -> Getter (t a) (f (t b))
traversing f = to $ \ta -> traverse f ta

data SourceInfo = SourceInfo
  { file :: Text
  , posFrom :: SourcePosition
  , posTo :: SourcePosition
  }
  deriving stock (Show, Eq, Ord, Generic)

data SourcePosition = SourcePosition
  { column :: Int
  , row :: Int
  }
  deriving stock (Show, Eq, Ord, Generic)

-- for our purposes we really only need a single name type
data LBName = LBName {name :: Text, sourceInfo :: SourceInfo}
  deriving stock (Show, Eq, Ord, Generic)

data TyName = TyName {name :: Text, sourceInfo :: SourceInfo}
  deriving stock (Show, Eq, Ord, Generic)

data ConstrName = ConstrName {name :: Text, sourceInfo :: SourceInfo}
  deriving stock (Show, Eq, Ord, Generic)

data ModuleName = ModuleName {parts :: [ModuleNamePart], sourceInfo :: SourceInfo}
  deriving stock (Show, Eq, Ord, Generic)

data ModuleNamePart = ModuleNamePart {name :: Text, sourceInfo :: SourceInfo}
  deriving stock (Show, Eq, Ord, Generic)

type ArgName = LBName

data VarName = VarName {name :: Text, sourceInfo :: SourceInfo}
  deriving stock (Show, Eq, Ord, Generic)

data FieldName = FieldName {name :: Text, sourceInfo :: SourceInfo}
  deriving stock (Show, Eq, Ord, Generic)

data ClassName = ClassName {name :: Text, sourceInfo :: SourceInfo}
  deriving stock (Show, Eq, Ord, Generic)

data Kind = Kind
  { kind :: KindType
  , sourceInfo :: SourceInfo
  }
  deriving stock (Show, Eq, Ord, Generic)

data KindType
  = KindRef KindRefType
  | KindArrow Kind Kind
  deriving stock (Show, Eq, Ord, Generic)

data KindRefType
  = KUnspecified
  | KType
  deriving stock (Show, Eq, Ord, Generic)

data TyVar = TyVar
  { varName :: VarName
  , sourceInfo :: SourceInfo
  }
  deriving stock (Show, Eq, Ord, Generic)

data TyInner
  = TyVarI TyVar
  | TyAppI TyApp
  | TyRefI TyRef
  deriving stock (Show, Eq, Ord, Generic)

data Ty = Ty
  { ty :: TyInner
  , sourceInfo :: SourceInfo
  }
  deriving stock (Show, Eq, Ord, Generic)

data TyApp = TyApp
  { tyFunc :: Ty
  , tyArgs :: NonEmpty Ty
  , sourceInfo :: SourceInfo
  }
  deriving stock (Show, Eq, Ord, Generic)

data ForeignRef = ForeignRef
  { tyName :: TyName
  , moduleName :: ModuleName
  }
  deriving stock (Show, Eq, Ord, Generic)

data TyRefInner
  = LocalI TyName
  | ForeignI ForeignRef
  deriving stock (Show, Eq, Ord, Generic)

data TyRef = TyRef
  { tyRef :: TyRefInner
  , sourceInfo :: SourceInfo
  }
  deriving stock (Show, Eq, Ord, Generic)

data TyDef = TyDef
  { tyName :: TyName
  , tyAbs :: TyAbs
  , sourceInfo :: SourceInfo
  }
  deriving stock (Show, Eq, Ord, Generic)

data TyAbs = TyAbs
  { tyVars :: [TyVar]
  , tyBody :: TyBody
  , sourceInfo :: SourceInfo
  }
  deriving stock (Show, Eq, Ord, Generic)

data TyArg = TyArg
  { argName :: VarName
  , argKind :: Kind
  , sourceInfo :: SourceInfo
  }
  deriving stock (Show, Eq, Ord, Generic)

data TyBodyInner
  = OpaqueI SourceInfo
  | SumI Sum
  deriving stock (Show, Eq, Ord, Generic)

data TyBody = TyBody
  { tyBody :: TyBodyInner
  , sourceInfo :: SourceInfo
  }
  deriving stock (Show, Eq, Ord, Generic)

data Constructor = Constructor
  { constrName :: ConstrName
  , product :: Product
  }
  deriving stock (Show, Eq, Ord, Generic)

data Sum = Sum
  { constructors :: NonEmpty Constructor
  , sourceInfo :: SourceInfo
  }
  deriving stock (Show, Eq, Ord, Generic)

data Field = Field
  { fieldName :: FieldName
  , fieldTy :: Ty
  }
  deriving stock (Show, Eq, Ord, Generic)

data ProductInner
  = EmptyI
  | RecordI (NonEmpty Field)
  | TupleI [Ty]
  deriving stock (Show, Eq, Ord, Generic)

data Product = Product
  { product :: ProductInner
  , sourceInfo :: SourceInfo
  }
  deriving stock (Show, Eq, Ord, Generic)

data ClassDef = ClassDef
  { className :: ClassName
  , classArgs :: TyArg
  , supers :: [Constraint]
  , documentation :: Text
  , sourceInfo :: SourceInfo
  }
  deriving stock (Show, Eq, Ord, Generic)

data InstanceClause = InstanceClause
  { className :: ClassName
  , head :: Ty
  , constraints :: [Constraint]
  , sourceInfo :: SourceInfo
  }
  deriving stock (Show, Eq, Ord, Generic)

data Constraint = Constraint
  { className :: ClassName
  , argument :: Ty
  , sourceInfo :: SourceInfo
  }
  deriving stock (Show, Eq, Ord, Generic)

data Module = Module
  { moduleName :: ModuleName
  , typeDefs :: [TyDef]
  , classDefs :: [ClassDef]
  , instances :: [InstanceClause]
  , sourceInfo :: SourceInfo
  }
  deriving stock (Show, Eq, Ord, Generic)

newtype CompilerInput = CompilerInput {modules :: [Module]}
  deriving stock (Show, Eq, Ord, Generic)

data FromProtoErr
  = MultipleInstanceHeads ClassName [Ty] SourceInfo
  | NoInstanceHead ClassName SourceInfo
  | NoConstraintArgs ClassName SourceInfo
  | MultipleConstraintArgs ClassName [Ty] SourceInfo
  | NoClassArgs ClassName SourceInfo
  | MultipleClassArgs ClassName SourceInfo
  | NoTyAppArgs SourceInfo
  | EmptyRecordBody SourceInfo -- Ideally we should catch & rethrow this and the next one when we have access to the tyname
  | EmptySumBody SourceInfo
  | EmptyName SourceInfo
  | EmptyField
  | NegativeArity ArgName SourceInfo
  | OneOfNotSet Text SourceInfo -- catchall
  deriving stock (Show, Eq, Ord, Generic)

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
    n <- fromProto $ v ^. P.name
    si <- fromProto $ v ^. P.sourceInfo
    pure $ FieldName n si

  toProto (FieldName n si) =
    defMessage
      & P.name .~ toProto n
      & P.sourceInfo .~ toProto si

instance IsMessage P.ConstrName ConstrName where
  fromProto v = do
    n <- fromProto $ v ^. P.name
    si <- fromProto $ v ^. P.sourceInfo
    pure $ ConstrName n si

  toProto (ConstrName n si) =
    defMessage
      & P.name .~ toProto n
      & P.sourceInfo .~ toProto si

instance IsMessage P.TyName TyName where
  fromProto v = do
    n <- fromProto $ v ^. P.name
    si <- fromProto $ v ^. P.sourceInfo
    pure $ TyName n si

  toProto (TyName n si) =
    defMessage
      & P.name .~ toProto n
      & P.sourceInfo .~ toProto si

instance IsMessage P.ClassName ClassName where
  fromProto v = do
    n <- fromProto $ v ^. P.name
    si <- fromProto $ v ^. P.sourceInfo
    pure $ ClassName n si

  toProto (ClassName n si) =
    defMessage
      & P.name .~ toProto n
      & P.sourceInfo .~ toProto si

instance IsMessage P.VarName VarName where
  fromProto v = do
    n <- fromProto $ v ^. P.name
    si <- fromProto $ v ^. P.sourceInfo
    pure $ VarName n si

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
    targs' <- ta ^. (P.tyArgs . traversing fromProto)
    targs <- note (NoTyAppArgs si) $ nonEmpty targs'
    pure $ TyApp tf targs si

  toProto (TyApp tf args si) =
    defMessage
      & P.tyFunc .~ toProto tf
      & P.tyArgs .~ toList (toProto <$> args)
      & P.sourceInfo .~ toProto si

instance IsMessage P.Ty Ty where
  fromProto ti = do
    si <- fromProto $ ti ^. P.sourceInfo
    case ti ^. P.maybe'ty of
      Nothing -> Left $ OneOfNotSet "ty" si
      Just x -> case x of
        P.Ty'TyVar tv -> (\y -> Ty (TyVarI y) si) <$> fromProto tv
        P.Ty'TyApp ta -> (\y -> Ty (TyAppI y) si) <$> fromProto ta
        P.Ty'TyRef tr -> (\y -> Ty (TyRefI y) si) <$> fromProto tr

  toProto ti =
    defMessage
      & P.sourceInfo .~ toProto (ti ^. #sourceInfo)
      & case ti ^. #ty of
        TyVarI tv -> P.tyVar .~ toProto tv
        TyAppI ta -> P.tyApp .~ toProto ta
        TyRefI tr -> P.tyRef .~ toProto tr

instance IsMessage P.TyRef TyRef where
  fromProto tr = do
    si <- fromProto $ tr ^. P.sourceInfo
    case tr ^. P.maybe'tyRef of
      Nothing -> Left $ OneOfNotSet "ty_ref" si
      Just x -> case x of
        P.TyRef'LocalTyRef lr -> (\l -> TyRef (LocalI l) si) <$> fromProto (lr ^. P.tyName)
        P.TyRef'ForeignTyRef f -> do
          tn <- fromProto $ f ^. P.tyName
          mn <- fromProto $ f ^. P.moduleName
          pure $ TyRef (ForeignI $ ForeignRef tn mn) si

  toProto (TyRef inner si) =
    defMessage
      & P.sourceInfo .~ toProto si
      & case inner of
        LocalI lnm ->
          let local = defMessage & P.tyName .~ toProto lnm
           in P.localTyRef .~ local
        ForeignI (ForeignRef tnm mnm) ->
          let foreign' =
                defMessage
                  & P.tyName .~ toProto tnm
                  & P.moduleName .~ toProto mnm
           in P.foreignTyRef .~ foreign'

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
    tyvars <- traverse fromProto $ ta ^. P.tyVars
    tybody <- fromProto $ ta ^. P.tyBody
    si <- fromProto $ ta ^. P.sourceInfo
    pure $ TyAbs tyvars tybody si

  toProto (TyAbs tyvars tyabs si) =
    defMessage
      & P.tyVars .~ (toProto <$> tyvars)
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
  fromProto tb = do
    si <- fromProto $ tb ^. P.sourceInfo
    case tb ^. P.maybe'tyBody of
      Nothing -> Left $ OneOfNotSet "tyBody" si
      Just x -> case x of
        P.TyBody'Opaque opq -> (\i -> TyBody (OpaqueI i) si) <$> fromProto (opq ^. P.sourceInfo)
        P.TyBody'Sum sumI -> (\s -> TyBody (SumI s) si) <$> fromProto sumI

  toProto (TyBody inner si) =
    defMessage
      & P.sourceInfo .~ toProto si
      & case inner of
        OpaqueI opqsi ->
          let opaque = defMessage & P.sourceInfo .~ toProto opqsi
           in P.opaque .~ opaque
        SumI sumI -> P.sum .~ toProto sumI

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

instance IsMessage P.Product Product where
  fromProto p = do
    si <- fromProto $ p ^. P.sourceInfo
    case p ^. P.maybe'product of
      Nothing -> Left $ OneOfNotSet "product" si
      Just x -> case x of
        P.Product'Record' r -> do
          fields' <- r ^. (P.fields . traversing fromProto)
          fields <- note (EmptyRecordBody si) $ nonEmpty fields'
          pure $ Product (RecordI fields) si
        P.Product'Ntuple t -> do
          args <- traverse fromProto $ t ^. P.fields
          pure $ Product (TupleI args) si

  -- sourceInfo for the inner types == sourceInfo for the outer type here
  toProto (Product inner si) =
    let si' :: P.SourceInfo = toProto si
     in defMessage
          & P.sourceInfo .~ toProto si
          & case inner of
            TupleI args -> P.ntuple .~ (defMessage & P.fields .~ (toProto <$> args))
            RecordI fs ->
              let fields = toProto <$> toList fs
               in P.record
                    .~ ( defMessage
                          & P.fields .~ fields
                          & P.sourceInfo .~ si'
                       )
            EmptyI -> undefined -- FIXME

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
    cnm <- fromProto $ ic ^. P.className
    csts <- traverse fromProto $ ic ^. P.constraints
    hds <- ic ^. (P.heads . traversing fromProto)
    hd <- case hds of
      [] -> Left $ NoInstanceHead cnm si
      [x] -> Right x
      xs -> Left $ MultipleInstanceHeads cnm xs si
    pure $ InstanceClause cnm hd csts si

  toProto (InstanceClause cnm hd csts si) =
    defMessage
      & P.className .~ toProto cnm
      & P.heads .~ pure (toProto hd)
      & P.constraints .~ (toProto <$> csts)
      & P.sourceInfo .~ toProto si

instance IsMessage P.Constraint Constraint where
  fromProto c = do
    si <- fromProto $ c ^. P.sourceInfo
    cnm <- fromProto $ c ^. P.className
    args <- c ^. (P.arguments . traversing fromProto)
    arg <- case args of
      [] -> Left $ NoConstraintArgs cnm si
      [x] -> Right x
      xs -> Left $ MultipleConstraintArgs cnm xs si
    pure $ Constraint cnm arg si

  toProto (Constraint cnm arg si) =
    defMessage
      & P.className .~ toProto cnm
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
instance IsMessage P.TyName LBName where
  fromProto n = do
    si <- fromProto $ n ^. P.sourceInfo
    nm <- (\x -> if T.null x then Left $ EmptyName si else Right x) (n ^. P.name)
    pure $ LBName nm si

  toProto (LBName tn si) =
    defMessage
      & P.name .~ tn
      & P.sourceInfo .~ toProto si

instance IsMessage P.ConstrName LBName where
  fromProto n = do
    si <- fromProto $ n ^. P.sourceInfo
    nm <- (\x -> if T.null x then Left $ EmptyName si else Right x) (n ^. P.name)
    pure $ LBName nm si

  toProto (LBName tn si) =
    defMessage
      & P.name .~ tn
      & P.sourceInfo .~ toProto si

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

instance IsMessage P.VarName LBName where
  fromProto n = do
    si <- fromProto $ n ^. P.sourceInfo
    nm <- (\x -> if T.null x then Left $ EmptyName si else Right x) (n ^. P.name)
    pure $ LBName nm si

  toProto (LBName tn si) =
    defMessage
      & P.name .~ tn
      & P.sourceInfo .~ toProto si

instance IsMessage P.FieldName LBName where
  fromProto n = do
    si <- fromProto $ n ^. P.sourceInfo
    nm <- (\x -> if T.null x then Left $ EmptyName si else Right x) (n ^. P.name)
    pure $ LBName nm si

  toProto (LBName tn si) =
    defMessage
      & P.name .~ tn
      & P.sourceInfo .~ toProto si

instance IsMessage P.ClassName LBName where
  fromProto n = do
    si <- fromProto $ n ^. P.sourceInfo
    nm <- (\x -> if T.null x then Left $ EmptyName si else Right x) (n ^. P.name)
    pure $ LBName nm si

  toProto (LBName tn si) =
    defMessage
      & P.name .~ tn
      & P.sourceInfo .~ toProto si
