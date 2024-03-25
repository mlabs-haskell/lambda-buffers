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
import LambdaBuffers.Codegen.Typescript.Print.Names (printCtorName, printFieldName, printTsQTyName, printTsQTyNameKey, printTyName, printVarName)
import LambdaBuffers.Codegen.Typescript.Syntax qualified as Ts
import LambdaBuffers.ProtoCompat qualified as PC
import Prettyprinter (Doc, Pretty (pretty), align, colon, comma, dot, encloseSep, equals, flatAlt, group, langle, lbrace, lbracket, parens, pipe, rangle, rbrace, rbracket, space, squotes, vcat, vsep, (<+>))

{- | `printTyAbs tyN tyAbs` prints the type abstraction `tyAbs` for a type name `tyN`.

Loosely, this will print things like

\$a $b = { name: 'MkFoo',  fields: $a } | { name: 'MkBar', fields: $b }
\$a = Prelude.Maybe $a
-}
printTyAbs :: MonadPrint m => Ts.PkgMap -> PC.TyName -> PC.TyAbs -> m (Doc ann, Doc ann)
printTyAbs pkgMap tyN (PC.TyAbs args body _) = do
  let argsDoc =
        if OMap.empty == args
          then mempty
          else encloseSep langle rangle comma (printTyArg <$> toList args)
  (bodyDoc, symbolDoc) <- printTyBody pkgMap tyN (toList args) body
  return (group $ argsDoc <+> align (equals <+> bodyDoc), symbolDoc)

{- | Prints the type body AND the symbol (unique runtime evidence) of the type.

   For example, this will print things like

    @
        : unique symbol = Symbol('TyName')
    @
    and
    @
        { name: 'MkFoo',  fields: $a } | { name: 'MkBar', fields: $b }
    @

Q. What is the symbol (unique runtime evidence) of the type
We print something like:
```
export const TyName: unique symbol = Symbol('TyName');
```
Why? This is to play nicely with LambdaBuffer's module system
and type classes.

Note: opaque types are _assumed_ to have such unique symbols
already defined.
-}
printTyBody :: MonadPrint m => Ts.PkgMap -> PC.TyName -> [PC.TyArg] -> PC.TyBody -> m (Doc ann, Doc ann)
printTyBody pkgMap tyN args tyBody =
  let
    -- E.g.
    -- : unique symbol = Symbol('TyName')
    newSymbolDoc = colon <+> "unique" <+> "symbol" <+> equals <+> "Symbol" <> parens (squotes $ printTyName tyN)
   in
    case tyBody of
      PC.SumI s -> (,newSymbolDoc) <$> printSum pkgMap tyN s
      PC.ProductI p -> return (printProd pkgMap p, newSymbolDoc)
      PC.RecordI r -> printRec pkgMap tyN r >>= \recDoc -> return (recDoc, newSymbolDoc)
      PC.OpaqueI si -> do
        opqs <- asks (view $ Print.ctxConfig . cfgOpaques)
        mn <- asks (view $ Print.ctxModule . #moduleName)
        case Map.lookup (PC.mkInfoLess mn, PC.mkInfoLess tyN) opqs of
          Nothing -> throwInternalError si ("Internal error: Should have an Opaque configured for " <> show tyN)
          Just hqtyn ->
            return
              ( printTsQTyName hqtyn
                  <> if null args
                    then mempty
                    else encloseSep langle rangle comma $ map (printVarName . view #argName) args
              , -- Note: we assume that the unique symbol exists for the opaque type
                colon <+> "unique" <+> "symbol" <+> equals <+> printTsQTyNameKey hqtyn
              )

printTyArg :: PC.TyArg -> Doc ann
printTyArg (PC.TyArg vn _ _) = printVarName vn

printSum :: MonadPrint m => Ts.PkgMap -> PC.TyName -> PC.Sum -> m (Doc ann)
printSum pkgMap tyN (PC.Sum ctors _) = do
  let ctorDocs = printCtor pkgMap tyN <$> toList ctors
  return $
    group $
      if null ctors
        then -- TODO(jaredponn): is this what we want? The empty sum
        -- corresponds to the `never` type in the sense that there are no
        -- values which satisfy this type
          "never"
        else align $ vsep $ map (pipe <+>) ctorDocs

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
printCtor :: Ts.PkgMap -> PC.TyName -> PC.Constructor -> Doc ann
printCtor pkgMap tyN (PC.Constructor ctorName prod) =
  let ctorNDoc = printCtorName tyN ctorName
   in group $
        align $
          vcat $
            [ lbrace <+> ctorNameFieldName <+> colon <+> squotes ctorNDoc
            ]
              ++ ( case prod of
                    PC.Product fields _
                      | null fields -> []
                      | otherwise -> [comma <+> fieldsFieldName <+> colon <+> printProd pkgMap prod]
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
printRec :: MonadPrint m => Ts.PkgMap -> PC.TyName -> PC.Record -> m (Doc ann)
printRec pkgMap tyN (PC.Record fields _) = do
  fieldsDoc <- for (toList fields) $ printField pkgMap tyN
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
printProd :: Ts.PkgMap -> PC.Product -> Doc ann
printProd pkgMap (PC.Product fields _) = align $ case fields of
  [ty] -> printTyInner pkgMap ty
  -- TODO(jaredponn): put spaces after the comma. Similar to Note [flatAlt
  -- rbrace]
  _ -> encloseSep lbracket rbracket comma $ map (printTyInner pkgMap) fields

printField :: MonadPrint m => Ts.PkgMap -> PC.TyName -> PC.Field -> m (Doc ann)
printField pkgMap tyN f@(PC.Field fn ty) = do
  fnDoc <-
    maybe
      (throwInternalError (fn ^. #sourceInfo) ("Failed printing `FieldName` for field\n" <> show (tyN, f)))
      return
      $ printFieldName tyN fn
  let tyDoc = printTyTopLevel pkgMap ty
  return $ fnDoc <+> colon <+> tyDoc

printTyInner :: Ts.PkgMap -> PC.Ty -> Doc ann
printTyInner _pkgMap (PC.TyVarI v) = printTyVar v
printTyInner pkgMap (PC.TyRefI r) = printTyRef pkgMap r
printTyInner pkgMap (PC.TyAppI a) = printTyAppInner pkgMap a

printTyAppInner :: Ts.PkgMap -> PC.TyApp -> Doc ann
printTyAppInner pkgMap (PC.TyApp f args _) =
  let fDoc = printTyInner pkgMap f
      argsDoc = printTyInner pkgMap <$> args
   in group $ fDoc <> align (encloseSep langle rangle comma argsDoc)

printTyTopLevel :: Ts.PkgMap -> PC.Ty -> Doc ann
printTyTopLevel _pkgMap (PC.TyVarI v) = printTyVar v
printTyTopLevel pkgMap (PC.TyRefI r) = printTyRef pkgMap r
printTyTopLevel pkgMap (PC.TyAppI a) = printTyAppTopLevel pkgMap a

printTyAppTopLevel :: Ts.PkgMap -> PC.TyApp -> Doc ann
printTyAppTopLevel pkgMap (PC.TyApp f args _) =
  let fDoc = printTyInner pkgMap f
      argsDoc = printTyInner pkgMap <$> args
   in group $ fDoc <> align (encloseSep langle rangle comma argsDoc)

printTyRef :: Ts.PkgMap -> PC.TyRef -> Doc ann
printTyRef _ (PC.LocalI (PC.LocalRef tn _)) = group $ printTyName tn
printTyRef pkgMap (PC.ForeignI fr) = let (_, Ts.MkModuleName hmn, Ts.MkTyName htn) = Ts.fromLbForeignRef pkgMap fr in pretty hmn <> dot <> pretty htn

printTyVar :: PC.TyVar -> Doc ann
printTyVar (PC.TyVar vn) = printVarName vn
