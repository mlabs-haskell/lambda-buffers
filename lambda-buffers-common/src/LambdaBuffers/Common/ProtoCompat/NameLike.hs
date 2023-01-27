{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE UndecidableInstances #-}

module LambdaBuffers.Common.ProtoCompat.NameLike (NameLike (..), coerceName) where

import Control.Lens (Iso', iso, (^.))
import Data.Generics.Labels (Field')
import Data.Text (Text)
import LambdaBuffers.Common.ProtoCompat.Types (
  ConstrName (ConstrName),
  FieldName (FieldName),
  LBName (LBName),
  ClassName (ClassName),
  ModuleNamePart (ModuleNamePart),
  SourceInfo,
  TyName (TyName),
  VarName (VarName),
 )

class (Field' "name" s Text, Field' "sourceInfo" s SourceInfo) => NameLike s where
  ctorName :: Text -> SourceInfo -> s

  isoName :: forall t. NameLike t => Iso' s t
  isoName = iso goTo goFro
    where
      goTo :: forall x. NameLike x => s -> x
      goTo s = ctorName (s ^. #name) (s ^. #sourceInfo)

      goFro :: forall x. NameLike x => x -> s
      goFro t = ctorName (t ^. #name) (t ^. #sourceInfo)

instance NameLike LBName where
  ctorName = LBName

instance NameLike TyName where
  ctorName = TyName

instance NameLike ConstrName where
  ctorName = ConstrName

instance NameLike ModuleNamePart where
  ctorName = ModuleNamePart

instance NameLike VarName where
  ctorName = VarName

instance NameLike FieldName where
  ctorName = FieldName

instance NameLike ClassName where
  ctorName = ClassName

coerceName :: forall s t. (NameLike s, NameLike t) => s -> t
coerceName s = s ^. isoName
