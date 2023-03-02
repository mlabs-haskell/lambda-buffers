{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module LambdaBuffers.Compiler.KindCheck.Type (
  Type (Abs, VoidT, Product, Sum, Constructor, Opaque, Var, UnitT, App),
  tyProd,
  tyUnit,
  tySum,
  tyVoid,
  Variable (TyVar, QualifiedTyRef, QualifiedTyClassRef),
  QualifiedTyRefName (..),
  QualifiedTyClassRefName (..),
  qTyRef'tyName,
  qTyRef'moduleName,
  qTyRef'sourceInfo,

  -- * Isomorphisms.
  ltrISOqtr,
  ftrISOqtr,
  ltrISOftr,
  fcrISOqtcr,
  lcrISOftcr,
  lcrISOqtcr,
) where

import Control.Lens (iso, makeLenses, withIso, (^.))
import Control.Lens.Combinators (Iso')
import GHC.Generics (Generic)
import Generics.SOP qualified as SOP
import LambdaBuffers.Compiler.ProtoCompat.InfoLess (InfoLess, InfoLessC, withInfoLess)
import LambdaBuffers.Compiler.ProtoCompat.Types qualified as PC
import Prettyprinter (Pretty (pretty), viaShow)

-- NOTE(cstml): Remove the Arbitrary instances and replaces them with Gens.

data QualifiedTyRefName = QualifiedTyRefName
  { _qTyRef'tyName :: PC.TyName
  , _qTyRef'moduleName :: PC.ModuleName
  , _qTyRef'sourceInfo :: PC.SourceInfo
  }
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (SOP.Generic)
instance InfoLessC QualifiedTyRefName

makeLenses ''QualifiedTyRefName

data QualifiedTyClassRefName = QualifiedTyClassRefName
  { _qTyClass'className :: PC.ClassName
  , _qTyClass'moduleName :: PC.ModuleName
  , _qTyClass'sourceInfo :: PC.SourceInfo
  }
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (SOP.Generic)
instance InfoLessC QualifiedTyClassRefName

makeLenses ''QualifiedTyClassRefName

instance Pretty (InfoLess Variable) where
  pretty x = withInfoLess x pretty

{- | All TyRefs and ClassNames are fully qualified. The context determines if
 they are local or not.
-}
data Variable
  = QualifiedTyRef QualifiedTyRefName
  | QualifiedTyClassRef QualifiedTyClassRefName
  | TyVar PC.VarName
  deriving stock (Eq, Show, Ord, Generic)
  deriving anyclass (SOP.Generic)

instance InfoLessC Variable

instance Pretty Variable where
  pretty = viaShow

data Type
  = Abs PC.TyAbs
  | App Type Type
  | Var Variable
  | Product Type Type
  | Sum Type Type
  | Constructor PC.Constructor
  | Opaque PC.SourceInfo
  | UnitT
  | VoidT
  deriving stock (Eq, Show)

tyProd :: Type -> Type -> Type
tyProd = Product

tySum :: Type -> Type -> Type
tySum = Sum

tyUnit :: Type
tyUnit = UnitT

tyVoid :: Type
tyVoid = VoidT

instance Pretty Type where
  pretty = viaShow

--------------------------------------------------------------------------------
-- Qualified TyRef ISOs.

ltrISOqtr :: Iso' (PC.LocalRef, PC.ModuleName) QualifiedTyRefName
ltrISOqtr = iso goRight goLeft
  where
    goRight :: (PC.LocalRef, PC.ModuleName) -> QualifiedTyRefName
    goRight (lr, mn) = QualifiedTyRefName (lr ^. #tyName) mn (lr ^. #sourceInfo)

    goLeft qtr =
      ( PC.LocalRef (qtr ^. qTyRef'tyName) (qtr ^. qTyRef'sourceInfo)
      , qtr ^. qTyRef'moduleName
      )

ltrISOftr :: Iso' (PC.LocalRef, PC.ModuleName) PC.ForeignRef
ltrISOftr = iso goRight goLeft
  where
    goRight :: (PC.LocalRef, PC.ModuleName) -> PC.ForeignRef
    goRight (lr, m) = PC.ForeignRef (lr ^. #tyName) m (lr ^. #sourceInfo)

    goLeft :: PC.ForeignRef -> (PC.LocalRef, PC.ModuleName)
    goLeft fr = (PC.LocalRef (fr ^. #tyName) (fr ^. #sourceInfo), fr ^. #moduleName)

ftrISOqtr :: Iso' PC.ForeignRef QualifiedTyRefName
ftrISOqtr = iso goRight goLeft
  where
    goRight :: PC.ForeignRef -> QualifiedTyRefName
    goRight = withIso ltrISOftr $ \_ f2l -> withIso ltrISOqtr $ \l2q _ -> l2q . f2l

    goLeft :: QualifiedTyRefName -> PC.ForeignRef
    goLeft = withIso ltrISOftr $ \l2f _ -> withIso ltrISOqtr $ \_ q2l -> l2f . q2l

--------------------------------------------------------------------------------
-- Qualified TyClass Name ISOs.

lcrISOqtcr :: Iso' (PC.LocalClassRef, PC.ModuleName) QualifiedTyClassRefName
lcrISOqtcr = iso goRight goLeft
  where
    goRight :: (PC.LocalClassRef, PC.ModuleName) -> QualifiedTyClassRefName
    goRight (lcr, mn) = QualifiedTyClassRefName (lcr ^. #className) mn (lcr ^. #sourceInfo)

    goLeft :: QualifiedTyClassRefName -> (PC.LocalClassRef, PC.ModuleName)
    goLeft qtcn =
      ( PC.LocalClassRef (qtcn ^. qTyClass'className) (qtcn ^. qTyClass'sourceInfo)
      , qtcn ^. qTyClass'moduleName
      )

lcrISOftcr :: Iso' (PC.LocalClassRef, PC.ModuleName) PC.ForeignClassRef
lcrISOftcr = iso goRight goLeft
  where
    goRight :: (PC.LocalClassRef, PC.ModuleName) -> PC.ForeignClassRef
    goRight (lr, mn) = PC.ForeignClassRef (lr ^. #className) mn (lr ^. #sourceInfo)

    goLeft :: PC.ForeignClassRef -> (PC.LocalClassRef, PC.ModuleName)
    goLeft fr = (PC.LocalClassRef (fr ^. #className) (fr ^. #sourceInfo), fr ^. #moduleName)

fcrISOqtcr :: Iso' PC.ForeignClassRef QualifiedTyClassRefName
fcrISOqtcr = iso goRight goLeft
  where
    goRight :: PC.ForeignClassRef -> QualifiedTyClassRefName
    goRight = withIso lcrISOftcr $ \_ f2l -> withIso lcrISOqtcr $ \l2q _ -> l2q . f2l

    goLeft :: QualifiedTyClassRefName -> PC.ForeignClassRef
    goLeft = withIso lcrISOftcr $ \l2f _ -> withIso lcrISOqtcr $ \_ q2l -> l2f . q2l
