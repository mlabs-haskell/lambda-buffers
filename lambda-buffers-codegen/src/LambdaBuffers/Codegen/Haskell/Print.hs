module LambdaBuffers.Codegen.Haskell.Print (printTyDefOpaque, printTyDefNonOpaque, NonOpaqueTyBody (..), printModule) where

import Control.Lens ((^.))
import Data.Char qualified as Char
import Data.Foldable (Foldable (toList))
import Data.Map.Ordered (OMap)
import Data.Map.Ordered qualified as OMap
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text qualified as Text
import LambdaBuffers.Codegen.Haskell.Syntax (fromLbModuleName)
import LambdaBuffers.Codegen.Haskell.Syntax qualified as H
import LambdaBuffers.Compiler.ProtoCompat.InfoLess qualified as PC
import LambdaBuffers.Compiler.ProtoCompat.Types qualified as PC
import Prettyprinter (Doc, Pretty (pretty), align, colon, comma, dot, encloseSep, equals, group, hsep, lbrace, line, lparen, parens, pipe, rbrace, rparen, sep, space, squote, vsep, (<+>))

printModuleHeader :: PC.ModuleName -> Set (PC.InfoLess PC.TyName) -> Doc a
printModuleHeader mn exports =
  let typeExportsDoc = align $ group $ encloseSep lparen rparen (comma <> space) ((`PC.withInfoLess` printTyName) <$> toList exports)
   in "module" <+> printModName mn <+> typeExportsDoc <+> "where"

printImports :: Set PC.QTyName -> Doc a
printImports imports =
  let grouped = Set.fromList [PC.withInfoLess mn id | (mn, _tn) <- toList imports]
      typeImportsDocs = (\mn -> "import qualified" <+> printModName mn) <$> toList grouped
      typeImportsDoc = vsep typeImportsDocs
   in typeImportsDoc

printTyDefs :: [Doc a] -> Doc a
printTyDefs = vsep

printModule :: PC.ModuleName -> Set (PC.InfoLess PC.TyName) -> Set PC.QTyName -> [Doc a] -> Doc a
printModule modName tyExports tyImports tyDefDocs =
  align . vsep $
    [ printModuleHeader modName tyExports
    , line
    , printImports tyImports
    , line
    , printTyDefs tyDefDocs
    ]

printTyVar :: PC.TyVar -> Doc a
printTyVar (PC.TyVar vn) = printVarName vn

printVarName :: PC.VarName -> Doc a
printVarName (PC.VarName n _) = pretty n

printTyName :: PC.TyName -> Doc a
printTyName (PC.TyName n _) = pretty n

printModName :: PC.ModuleName -> Doc a
printModName mn = let H.MkModuleName hmn = fromLbModuleName mn in pretty hmn

{- | Creates an alias to the specified 'native' type.

opaque Maybe a

translates to

type Maybe = Prelude.Maybe
-}
printTyDefOpaque :: PC.TyName -> (H.CabalPackageName, H.ModuleName, H.TyName) -> Doc a
printTyDefOpaque tyN hsTyRef = "type" <+> printTyName tyN <+> equals <+> printHsTyRef hsTyRef

printHsTyRef :: H.QTyName -> Doc a
printHsTyRef (_, H.MkModuleName hsModName, H.MkTyName hsTyName) = pretty hsModName <> dot <> pretty hsTyName

-- | Used to distinguish from Opaques.
newtype NonOpaqueTyBody = Sum PC.Sum

printTyDefNonOpaque :: PC.TyName -> OMap (PC.InfoLess PC.VarName) PC.TyArg -> NonOpaqueTyBody -> Doc a
printTyDefNonOpaque tyN args body =
  let argsDoc = if OMap.empty == args then mempty else sep (printTyArg <$> toList args)
      (keyword, bodyDoc) = printTyBody tyN body
   in group $ keyword <+> printTyName tyN <> argsDoc <+> align (equals <+> bodyDoc)

-- TODO(bladyjoker): Add Record/Tuple.
printTyBody :: PC.TyName -> NonOpaqueTyBody -> (Doc a, Doc a)
printTyBody tyN (Sum s) = ("data", printTyBodySum tyN s)

printTyArg :: forall {a}. PC.TyArg -> Doc a
printTyArg (PC.TyArg vn _ _) = printVarName vn

printTyBodySum :: PC.TyName -> PC.Sum -> Doc a
printTyBodySum tyN (PC.Sum ctors _) =
  let ctorDocs = printCtor tyN <$> toList ctors
   in group $
        if null ctors
          then mempty
          else sep $ zipWith (<>) (mempty : repeat (pipe <> space)) ctorDocs

printCtor :: PC.TyName -> PC.Constructor -> Doc a
printCtor tyN (PC.Constructor ctorName prod) =
  let ctorNDoc = printCtorName tyN ctorName
      prodDoc = printProd tyN prod
   in group (ctorNDoc <+> prodDoc)

{- | Translate LambdaBuffer sum constructor names into Haskell sum constructor names.
 sum Sum = Foo Int | Bar String
 translates to
 data Sum = Sum'Foo Int | Sum'Bar String
-}
printCtorName :: PC.TyName -> PC.ConstrName -> Doc a
printCtorName tyN (PC.ConstrName n _) = group $ printTyName tyN <> squote <> pretty n

printProd :: PC.TyName -> PC.Product -> Doc a
printProd tyN (PC.RecordI rc) = printRec tyN rc
printProd _ (PC.TupleI tup) = printTup tup

printRec :: PC.TyName -> PC.Record -> Doc a
printRec tyN (PC.Record fields _) =
  if null fields
    then mempty
    else
      let fieldDocs = printField tyN <$> toList fields
       in group $ encloseSep lbrace rbrace (space <> comma <> space) fieldDocs

printTup :: PC.Tuple -> Doc a
printTup (PC.Tuple fields _) = if null fields then mempty else group . align $ sep (printTy <$> fields)

printField :: PC.TyName -> PC.Field -> Doc a
printField tyN (PC.Field fn ty) = printFieldName tyN fn <+> colon <> colon <+> printTy ty

{- | Translate LambdaBuffer record field names into Haskell record field names
 rec Rec = { foo :: Int, bar :: String }
 translates to
 data Rec = MkRec { rec'foo :: Int, rec'bar :: String }
-}
printFieldName :: PC.TyName -> PC.FieldName -> Doc a
printFieldName tyN (PC.FieldName n _) =
  let prefix = case Text.uncons (tyN ^. #name) of
        Nothing -> "<empty type name>" -- NOTE(bladyjoker): Should not happen :shrug:.
        Just (h, t) -> Text.cons (Char.toLower h) t
   in pretty prefix <> squote <> pretty n

printTy :: PC.Ty -> Doc a
printTy (PC.TyVarI v) = printTyVar v
printTy (PC.TyRefI r) = printTyRef r
printTy (PC.TyAppI a) = printTyApp a

printTyApp :: PC.TyApp -> Doc a
printTyApp (PC.TyApp f args _) =
  let fDoc = printTy f
      argsDoc = printTy <$> args
   in group $ parens $ fDoc <+> align (sep argsDoc)

printTyRef :: PC.TyRef -> Doc a
printTyRef (PC.LocalI (PC.LocalRef tn _)) = group $ printTyName tn
printTyRef (PC.ForeignI fr) = let (_, H.MkModuleName hmn, H.MkTyName htn) = H.fromLbForeignRef fr in pretty hmn <> dot <> pretty htn
