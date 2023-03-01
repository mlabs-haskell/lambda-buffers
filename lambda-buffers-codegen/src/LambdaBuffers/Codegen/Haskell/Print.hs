module LambdaBuffers.Codegen.Haskell.Print (printTyDefOpaque, printTyDefNonOpaque, NonOpaqueTyBody (..), printModule) where

import Control.Lens ((^.))
import Data.Char qualified as Char
import Data.Foldable (Foldable (toList))
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text qualified as Text
import LambdaBuffers.Codegen.Haskell.Syntax qualified as H
import LambdaBuffers.Compiler.ProtoCompat.Types qualified as PC
import Prettyprinter (Doc, Pretty (pretty), align, colon, comma, concatWith, dot, encloseSep, equals, group, lbrace, line, lparen, parens, pipe, rbrace, rparen, sep, space, squote, surround, vsep, (<+>))

printModuleHeader :: H.ModuleName -> Set H.TyName -> Doc a
printModuleHeader (H.MkModuleName mn) exports =
  let typeExportsDoc = align $ group $ encloseSep lparen rparen comma ((\(H.MkTyName tn) -> pretty tn) <$> toList exports)
   in "module" <+> pretty mn <+> typeExportsDoc <+> "where"

printImports :: Set H.QTyName -> Doc a
printImports imports =
  let grouped = Map.unionsWith Set.union [Map.singleton (c, mn) (Set.singleton tn) | (c, mn, tn) <- toList imports]
      typeImportsDocs = (\((_, H.MkModuleName mn), tns) -> "import qualified" <+> pretty mn <+> encloseSep lparen rparen comma ((\(H.MkTyName tn) -> pretty tn) <$> toList tns)) <$> Map.toList grouped
      typeImportsDoc = vsep typeImportsDocs
   in typeImportsDoc

printTyDefs :: [Doc a] -> Doc a
printTyDefs = vsep

printModule :: H.ModuleName -> Set H.TyName -> Set H.QTyName -> [Doc a] -> Doc a
printModule modName tyExports tyImports tyDefDocs =
  vsep
    [ printModuleHeader modName tyExports
    , line
    , printImports tyImports
    , line
    , printTyDefs tyDefDocs
    , line
    ]

printTyVar :: PC.TyVar -> Doc a
printTyVar (PC.TyVar vn) = printVarName vn

printVarName :: PC.VarName -> Doc a
printVarName (PC.VarName n _) = pretty n

printTyName :: PC.TyName -> Doc a
printTyName (PC.TyName n _) = pretty n

printModName :: PC.ModuleName -> Doc a
printModName (PC.ModuleName parts _) = group $ concatWith (surround dot) [pretty p | PC.ModuleNamePart p _ <- parts]

{- | Creates an alias to the specified 'native' type.

opaque Maybe a

translates to

type Maybe = Prelude.Maybe
-}
printTyDefOpaque :: PC.TyName -> (H.CabalPackageName, H.ModuleName, H.TyName) -> Doc a
printTyDefOpaque tyN hsTyRef = "type" <+> printTyName tyN <+> equals <+> printHsTyRef hsTyRef

printHsTyRef :: (H.CabalPackageName, H.ModuleName, H.TyName) -> Doc a
printHsTyRef (_, H.MkModuleName hsModName, H.MkTyName hsTyName) = pretty hsModName <> dot <> pretty hsTyName

-- | Used to distinguish from Opaques.
newtype NonOpaqueTyBody = Sum PC.Sum

printTyDefNonOpaque :: PC.TyName -> Map PC.VarName PC.TyArg -> NonOpaqueTyBody -> Doc a
printTyDefNonOpaque tyN args body =
  let argsDoc = sep (printTyArg <$> toList args) -- FIXME(bladyjoker): OMap on Constructors
      (keyword, bodyDoc) = printTyBody tyN body
   in group $ keyword <+> printTyName tyN <+> argsDoc <+> equals <+> bodyDoc

-- TODO(bladyjoker): Add Record/Tuple.
printTyBody :: PC.TyName -> NonOpaqueTyBody -> (Doc a, Doc a)
printTyBody tyN (Sum s) = ("data", printTyBodySum tyN s)

printTyArg :: forall {a}. PC.TyArg -> Doc a
printTyArg (PC.TyArg vn _ _) = printVarName vn

printTyBodySum :: PC.TyName -> PC.Sum -> Doc a
printTyBodySum tyN (PC.Sum ctors _) =
  let ctorDocs = printCtor tyN <$> toList ctors -- FIXME(bladyjoker): OMap on Constructors
   in group $
        if null ctors
          then mempty
          else align $ encloseSep mempty mempty (space <> pipe <> space) ctorDocs

printCtor :: PC.TyName -> PC.Constructor -> Doc a
printCtor tyN (PC.Constructor ctorName prod) =
  let ctorNDoc = printCtorName tyN ctorName
      prodDoc = printProd tyN prod
   in align $ group (ctorNDoc <+> prodDoc)

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
  let fieldDocs = printField tyN <$> toList fields -- FIXME(bladyjoker): OMap on Fields
   in group $ encloseSep lbrace rbrace (space <> comma <> space) fieldDocs

printTup :: PC.Tuple -> Doc a
printTup (PC.Tuple fields _) = group $ sep (printTy <$> fields)

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
printTyRef (PC.ForeignI (PC.ForeignRef tn mn _)) = group $ printModName mn <> dot <> printTyName tn
