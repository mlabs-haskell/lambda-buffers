module LambdaBuffers.Codegen.Rust.Print.Syntax (printRsQTyName, printCtorName, printFieldName, printVarName, printTyName, printMkCtor, printModName, printRsQValName, printRsTraitMethodName, printRsQTraitName, printRsValName, QTyName, QTraitName, QValName, Qualified (..), CrateName (..), ModuleName (..), TyName (..), TraitName (..), ValueName (..), fromLbModuleName, crateFromLbModuleName, fromLbTyName, fromLbForeignRef, filepathFromModuleName, TyDefKw (..), crateNameToText, printQualifiedCtorName, printTyVar, printTyArg, doubleColon, printRsTyName, qualifiedToCrate, qForeignRef, qBuiltin, printTyRef, qualifiedEntity, crateNameToCargoText, encloseGenerics, PkgMap) where

import Control.Lens ((^.))
import Data.Char qualified as Char
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import GHC.Generics (Generic)
import LambdaBuffers.ProtoCompat qualified as PC
import Prettyprinter (Doc, Pretty (pretty), colon, comma, enclose, encloseSep, group, langle, lparen, rangle, rparen)

data Qualified a
  = Qualified'Builtin !a
  | Qualified'LibRef !CrateName ![ModuleName] !a
  deriving stock (Eq, Ord, Show)

type QValName = Qualified ValueName
type QTyName = Qualified TyName
type QTraitName = Qualified TraitName

type PkgMap = Map (PC.InfoLess PC.ModuleName) CrateName

qForeignRef :: (Text -> a) -> Text -> [Text] -> Text -> Qualified a
qForeignRef mkA cn ms a = Qualified'LibRef (MkCrateName cn) (MkModuleName <$> ms) (mkA a)

qBuiltin :: (Text -> a) -> Text -> Qualified a
qBuiltin mkA = Qualified'Builtin . mkA

qualifiedToCrate :: Qualified a -> Maybe CrateName
qualifiedToCrate (Qualified'Builtin _) = Nothing
qualifiedToCrate (Qualified'LibRef c _ _) = Just c

qualifiedEntity :: Qualified a -> a
qualifiedEntity (Qualified'Builtin a) = a
qualifiedEntity (Qualified'LibRef _ _ a) = a

newtype CrateName = MkCrateName Text deriving stock (Eq, Ord, Show, Generic)
newtype ModuleName = MkModuleName {unModuleName :: Text} deriving stock (Eq, Ord, Show, Generic)
newtype TyName = MkTyName Text deriving stock (Eq, Ord, Show, Generic)
newtype TraitName = MkTraitName Text deriving stock (Eq, Ord, Show, Generic)
newtype ValueName = MkValueName Text deriving stock (Eq, Ord, Show, Generic)

data TyDefKw = StructTyDef | EnumTyDef | SynonymTyDef deriving stock (Eq, Ord, Show, Generic)

fromLbTyName :: PC.TyName -> TyName
fromLbTyName tn = MkTyName $ tn ^. #name

fromLbModuleName :: PC.ModuleName -> [ModuleName]
fromLbModuleName mn = [MkModuleName $ Text.replace "-" "_" $ Text.toLower $ p ^. #name | p <- mn ^. #parts]

crateFromLbModuleName :: PkgMap -> PC.ModuleName -> CrateName
crateFromLbModuleName pkgs mn =
  let defCrateName = MkCrateName $ Text.intercalate "_" ("lbf" : [Text.toLower $ p ^. #name | p <- mn ^. #parts])
   in fromMaybe defCrateName (Map.lookup (PC.mkInfoLess mn) pkgs)

-- | Converts a crate name to how it appears in the Cargo manifest file
crateNameToCargoText :: CrateName -> Text
crateNameToCargoText (MkCrateName cpn) = cpn

-- | Converts a crate name to how it appears in the Rust code
crateNameToText :: CrateName -> Text
crateNameToText (MkCrateName cpn) = Text.replace "-" "_" cpn

fromLbForeignRef :: PkgMap -> PC.ForeignRef -> QTyName
fromLbForeignRef pkgs fr =
  let mn = fr ^. #moduleName
   in Qualified'LibRef
        (crateFromLbModuleName pkgs mn)
        (fromLbModuleName mn)
        (fromLbTyName $ fr ^. #tyName)

filepathFromModuleName :: PC.ModuleName -> FilePath
filepathFromModuleName mn = Text.unpack (Text.intercalate "/" (unModuleName <$> fromLbModuleName mn)) <> ".rs"

printModName :: PC.ModuleName -> Doc ann
printModName = printRsModules . fromLbModuleName

printRsModules :: [ModuleName] -> Doc ann
printRsModules rsMods = pretty $ Text.intercalate "::" $ unModuleName <$> rsMods

printQualified :: (a -> Doc ann) -> Qualified a -> Doc ann
printQualified p (Qualified'Builtin entity) = p entity
printQualified p (Qualified'LibRef (MkCrateName crateName) rsMods entity) =
  let modules = if null rsMods then mempty else doubleColon <> printRsModules rsMods
   in pretty (Text.replace "-" "_" crateName) <> modules <> doubleColon <> p entity

printRsTyName :: TyName -> Doc ann
printRsTyName (MkTyName rsTyName) = pretty rsTyName

printRsTraitName :: TraitName -> Doc ann
printRsTraitName (MkTraitName rsTraitName) = pretty rsTraitName

printRsValName :: ValueName -> Doc ann
printRsValName (MkValueName rsValName) = case Text.uncons rsValName of
  Nothing -> "TODO(bladyjoker): Got an empty Rust value name"
  Just (c, _) | Char.isAlpha c -> pretty rsValName
  _other -> enclose lparen rparen $ pretty rsValName

printRsQTyName :: QTyName -> Doc ann
printRsQTyName = printQualified printRsTyName

printRsQTraitName :: QTraitName -> Doc ann
printRsQTraitName = printQualified printRsTraitName

printRsQValName :: QValName -> Doc ann
printRsQValName = printQualified printRsValName

{- | Print the Rust class method name (ie. (==), toJSON etc.).
 This doesn't require a qualified print as it's treated special, we just need to
 import the class and the class methods are made available in the scope.
-}
printRsTraitMethodName :: QValName -> Doc ann
printRsTraitMethodName (Qualified'Builtin rsValName) = printRsValName rsValName
printRsTraitMethodName (Qualified'LibRef _ _ rsValName) = printRsValName rsValName

printCtorName :: PC.ConstrName -> Doc ann
printCtorName (PC.ConstrName n _) = pretty n

printQualifiedCtorName :: PC.TyName -> PC.ConstrName -> Doc ann
printQualifiedCtorName tyN (PC.ConstrName n _) = printTyName tyN <> doubleColon <> pretty n

printMkCtor :: PC.TyName -> Doc ann
printMkCtor = printTyName

printFieldName :: PC.FieldName -> Doc ann
printFieldName (PC.FieldName n _) = pretty . toSnakeCase $ n

toSnakeCase :: Text -> Text
toSnakeCase = Text.concatMap f
  where
    f c
      | Char.isUpper c = "_" <> Text.toLower (Text.singleton c)
      | otherwise = Text.singleton c

printVarName :: PC.VarName -> Doc ann
printVarName (PC.VarName n _) = pretty n

printTyArg :: PC.TyArg -> Doc ann
printTyArg (PC.TyArg (PC.VarName n _) _ _) = pretty $ Text.toTitle n

printTyVar :: PC.TyVar -> Doc ann
printTyVar (PC.TyVar (PC.VarName n _)) = pretty $ Text.toTitle n

printTyName :: PC.TyName -> Doc ann
printTyName (PC.TyName n _) = pretty n

doubleColon :: Doc ann
doubleColon = colon <> colon

printTyRef :: PkgMap -> PC.TyRef -> Doc ann
printTyRef _ (PC.LocalI (PC.LocalRef tn _)) = group $ printTyName tn
printTyRef pkgs (PC.ForeignI fr) =
  let qTyName = fromLbForeignRef pkgs fr
   in printRsQTyName qTyName

encloseGenerics :: [Doc ann] -> Doc ann
encloseGenerics args =
  if null args
    then mempty
    else encloseSep langle rangle comma args
