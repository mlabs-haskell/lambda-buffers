module LambdaBuffers.Codegen.Typescript.Print.Ty (printTyInner, printTyTopLevel, printTyAbs) where

import Control.Lens (view, (^.))
import Control.Monad.Reader.Class (asks)
import Data.Foldable (Foldable (toList))
import Data.Map qualified as Map
import Data.Map.Ordered qualified as OMap
import Data.Traversable (for)
import LambdaBuffers.Codegen.Config (cfgOpaques)
import LambdaBuffers.Codegen.Print (throwInternalError)
import LambdaBuffers.Codegen.Print qualified as Print
import LambdaBuffers.Codegen.Typescript.Print.MonadPrint (MonadPrint)
import LambdaBuffers.Codegen.Typescript.Print.Names (printCtorName, printFieldName, printTsQTyName, printTyName, printVarName)
import LambdaBuffers.Codegen.Typescript.Syntax qualified as Ts
import LambdaBuffers.ProtoCompat qualified as PC
import Prettyprinter (Doc, Pretty (pretty), align, colon, comma, dot, encloseSep, equals, flatAlt, group, langle, lbrace, lbracket, pipe, rangle, rbrace, rbracket, sep, space, squotes, vcat, vsep, (<+>))

{- | `printTyAbs tyN tyAbs` prints the type abstraction `tyAbs` for a type name `tyN`.

Loosely, this will print things like

\$a $b = { name: 'MkFoo',  fields: $a } | { name: 'MkBar', fields: $b }
\$a = Prelude.Maybe $a
-}
printTyAbs :: MonadPrint m => PC.TyName -> PC.TyAbs -> m (Doc ann)
printTyAbs tyN (PC.TyAbs args body _) = do
  let argsDoc =
        if OMap.empty == args
          then mempty
          else encloseSep langle rangle comma (printTyArg <$> toList args)
  bodyDoc <- printTyBody tyN (toList args) body
  return (group $ argsDoc <+> align (equals <+> bodyDoc))

{- | Prints the type body.

For the above examples it prints

TODO(jaredponn) Empty records + document some examples here.
-}
printTyBody :: MonadPrint m => PC.TyName -> [PC.TyArg] -> PC.TyBody -> m (Doc ann)
printTyBody tyN _ (PC.SumI s) =
  -- Data type def
  printSum tyN s
printTyBody _tyN _ (PC.ProductI p) = return (printProd p)
printTyBody tyN _ (PC.RecordI r) = printRec tyN r >>= \recDoc -> return recDoc
printTyBody tyN args (PC.OpaqueI si) = do
  opqs <- asks (view $ Print.ctxConfig . cfgOpaques)
  mn <- asks (view $ Print.ctxModule . #moduleName)
  case Map.lookup (PC.mkInfoLess mn, PC.mkInfoLess tyN) opqs of
    Nothing -> throwInternalError si ("Internal error: Should have an Opaque configured for " <> show tyN)
    Just hqtyn -> return (printTsQTyName hqtyn <> if null args then mempty else space <> sep (printVarName . view #argName <$> args))

printTyArg :: PC.TyArg -> Doc ann
printTyArg (PC.TyArg vn _ _) = printVarName vn

printSum :: MonadPrint m => PC.TyName -> PC.Sum -> m (Doc ann)
printSum tyN (PC.Sum ctors _) = do
  let ctorDocs = printCtor tyN <$> toList ctors
  return $
    group $
      if null ctors
        then mempty
        else align $ vsep $ map (pipe <+>) ctorDocs -- TODO(bladyjoker): Make it align on the ConstrName.

-- | The name of the field which specifies a type constructor
ctorNameFieldName :: Doc ann
ctorNameFieldName = "name"

-- | The name of the field which specifies the arguments of a type constructor
fieldsFieldName :: Doc ann
fieldsFieldName = "fields"

{- |
 Three cases when printing a constructor.

 1. Product is empty i.e., `Branch` ===> print  `{ name : 'Branch' }`
 2. Product is size one i.e., `Branch ty` ===> print  `{ name : 'Branch' , fields : ty }`
 3. Otherwise, product has size > 1 i.e., `Branch ty1 ... tyN` ===> print  `{ name : 'Branch', fields : [ty1, ..., tyN] }`
-}
printCtor :: PC.TyName -> PC.Constructor -> Doc ann
printCtor tyN (PC.Constructor ctorName prod) =
  let ctorNDoc = printCtorName tyN ctorName
   in group $
        align $
          vcat $
            [ lbrace <+> ctorNameFieldName <+> colon <+> squotes ctorNDoc
            ]
              ++ ( case prod of
                    PC.Product fields _
                      | null fields -> []
                      | otherwise -> [comma <+> fieldsFieldName <+> colon <+> printProd prod]
                 )
              ++
              -- Note [flatAlt rbrace]
              -- ~~~~~~~~~~~~~~~~~~~~~
              -- This `flatAlt` thing will ensure that we print things like
              --   ```
              --   { blah : blargh }
              --                  ^~~ note this space
              --   ```
              -- and
              --   ```
              --   { reallylong : blargh
              --   , alsoreally long : blargh
              --   }
              --  ^~~ note there is no space
              --   ```
              --
              [flatAlt rbrace (space <> rbrace)]

-- | Just prints out the same record as given in the .lbf file
printRec :: MonadPrint m => PC.TyName -> PC.Record -> m (Doc ann)
printRec tyN (PC.Record fields _) = do
  fieldsDoc <- for (toList fields) $ printField tyN
  return $ group $ align $ case fieldsDoc of
    [] -> lbrace <+> rbrace
    f : fs ->
      vcat $
        ((lbrace <+> f) : map (comma <+>) fs)
          ++
          -- see Note [flatAlt rbrace]
          [flatAlt rbrace (space <> rbrace)]

{- | Three cases for printing a product.

 1. Product is empty ===> print the empty list e.g. []

 2. Product is size one ===> just print the type e.g. $a

 3. Otherwise, the product is greater than one ===> print a fixed length list
 with all of the types in order e.g. [$a,$b]
-}
printProd :: PC.Product -> Doc ann
printProd (PC.Product fields _) = align $ case fields of
  [ty] -> printTyInner ty
  -- TODO(jaredponn): put spaces after the comma. Similar to Note [flatAlt
  -- rbrace]
  _ -> encloseSep lbracket rbracket comma $ map printTyInner fields

printField :: MonadPrint m => PC.TyName -> PC.Field -> m (Doc ann)
printField tyN f@(PC.Field fn ty) = do
  fnDoc <-
    maybe
      (throwInternalError (fn ^. #sourceInfo) ("Failed printing `FieldName` for field\n" <> show (tyN, f)))
      return
      $ printFieldName tyN fn
  let tyDoc = printTyTopLevel ty
  return $ fnDoc <+> colon <+> tyDoc

printTyInner :: PC.Ty -> Doc ann
printTyInner (PC.TyVarI v) = printTyVar v
printTyInner (PC.TyRefI r) = printTyRef r
printTyInner (PC.TyAppI a) = printTyAppInner a

printTyAppInner :: PC.TyApp -> Doc ann
printTyAppInner (PC.TyApp f args _) =
  let fDoc = printTyInner f
      argsDoc = printTyInner <$> args
   in group $ fDoc <> align (encloseSep langle rangle comma argsDoc)

printTyTopLevel :: PC.Ty -> Doc ann
printTyTopLevel (PC.TyVarI v) = printTyVar v
printTyTopLevel (PC.TyRefI r) = printTyRef r
printTyTopLevel (PC.TyAppI a) = printTyAppTopLevel a

printTyAppTopLevel :: PC.TyApp -> Doc ann
printTyAppTopLevel (PC.TyApp f args _) =
  let fDoc = printTyInner f
      argsDoc = printTyInner <$> args
   in group $ fDoc <> align (encloseSep langle rangle comma argsDoc)

printTyRef :: PC.TyRef -> Doc ann
printTyRef (PC.LocalI (PC.LocalRef tn _)) = group $ printTyName tn
printTyRef (PC.ForeignI fr) = let (_, Ts.MkModuleName hmn, Ts.MkTyName htn) = Ts.fromLbForeignRef fr in pretty hmn <> dot <> pretty htn

printTyVar :: PC.TyVar -> Doc ann
printTyVar (PC.TyVar vn) = printVarName vn
