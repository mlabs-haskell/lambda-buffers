module LambdaBuffers.Codegen.Typescript.Print.InstanceDef (
  printExportInstanceDecl,
  printInstanceDict,
  ExportInstanceDecl (..),
) where

import Data.Text (Text)
import LambdaBuffers.Codegen.Typescript.Print.MonadPrint (MonadPrint)
import LambdaBuffers.Codegen.Typescript.Print.Names (
  printTsQClassName,
  printTsUnqualifiedQClassName,
  printVarName,
 )
import LambdaBuffers.Codegen.Typescript.Print.Ty (printTyInner)
import LambdaBuffers.Codegen.Typescript.Syntax qualified as Ts
import LambdaBuffers.ProtoCompat qualified as PC

-- import Data.Text qualified as Text
import Prettyprinter (Doc, align, comma, encloseSep, group, langle, line, lparen, pretty, rangle, rparen, surround, (<+>))

data ExportInstanceDecl a
  = ExportConstInstanceDecl a
  | ExportFunctionInstanceDecl a

printExportInstanceDecl :: MonadPrint m => Ts.QClassName -> PC.Ty -> m (ExportInstanceDecl (Doc ann))
printExportInstanceDecl tsQClassName ty =
  let
    instanceName = printInstanceDecl tsQClassName ty
    instanceType = printInstanceType tsQClassName ty
    instanceDeclTypeVars = collectInstanceDeclTypeVars ty
   in
    case instanceDeclTypeVars of
      [] ->
        return $
          ExportConstInstanceDecl $
            -- "instance" <+> headDoc <+> "where" <> hardline <> space <> space <> implDoc
            "export" <+> "const" <+> instanceName <+> ":" <+> surround instanceType langle rangle <> line
      _ ->
        return $
          ExportFunctionInstanceDecl $
            "export"
              <+> "function"
              <+> instanceName
                <> printInstanceContext tsQClassName instanceDeclTypeVars
              <+> ":"
              <+> instanceType
                <> line

{- | Prints the name of the instance defn. e.g., given instance `Prelude.Eq
 Integer`,  this prints `eqInteger`.

 TODO(jaredponn): this unfortunately makes it impossible to have two modules
 with the same class name with instances of the same type to exist.

 There's a similar problem with different types with the same name.

 There are some ways around this: a quick sketch.
  - Every type introduces a `Symbol`
  - So, every instance gets mapped to
  `ClassName[typeSymbol]`.
 It really doesn't seem worth the extra overhead to do this.
-}
printInstanceDecl :: forall ann. Ts.QClassName -> PC.Ty -> Doc ann
printInstanceDecl qcn ty =
  -- See 4.3.2 of the Haskell report
  -- https://www.haskell.org/onlinereport/haskell2010/haskellch4.html#x10-630004.1
  -- for the form we assume the instance declarations to be in i.e., the
  -- "leftmost" type determines the instance.
  --
  -- TODO: when the leftmost type is a type variable, there is at most one
  -- instance that unambiguous instance that may be applied, so we simply
  -- print out the type class name (in lower case).
  let go :: PC.Ty -> Doc ann
      go (PC.TyVarI _) = mempty
      go (PC.TyAppI PC.TyApp {PC.tyFunc = tyFunc}) = go tyFunc
      go (PC.TyRefI tyRef) = pretty $
        case tyRef of
          PC.LocalI PC.LocalRef {PC.tyName = tyName} -> getTyName tyName
          PC.ForeignI PC.ForeignRef {PC.tyName = tyName} ->
            -- TODO: this is broken for foreign types with the same name as
            -- a type declared in this module.
            getTyName tyName
   in printTsUnqualifiedQClassName qcn <> go ty

getTyName :: PC.TyName -> Text
getTyName (PC.TyName tyName _) = tyName

printInstanceDict :: forall ann. Ts.QClassName -> PC.Ty -> Doc ann
printInstanceDict qcn ty =
  let go :: PC.Ty -> Doc ann
      go (PC.TyVarI PC.TyVar {PC.varName = varName}) = "dict" <> printVarName varName
      go (PC.TyAppI PC.TyApp {PC.tyFunc = tyFunc}) = go tyFunc
      go (PC.TyRefI tyRef) =
        mconcat
          [ printTsUnqualifiedQClassName qcn
          , pretty $
              case tyRef of
                PC.LocalI PC.LocalRef {PC.tyName = tyName} -> getTyName tyName
                PC.ForeignI PC.ForeignRef {PC.tyName = tyName} ->
                  -- TODO: this is broken for foreign types with the same name as
                  -- a type declared in this module.
                  getTyName tyName
          ]
   in go ty

printInstanceType :: Ts.QClassName -> PC.Ty -> Doc ann
printInstanceType qcn ty =
  let crefDoc = printTsQClassName qcn
      tyDoc = printTyInner ty
   in crefDoc <> surround tyDoc langle rangle

{- | Prints an instance context argument e.g.

 Eq a => (a,b)
 ^~~~ translates this part to

 dict$a : Eq<$a>
-}
printInstanceContextArg :: Ts.QClassName -> PC.Ty -> Doc ann
printInstanceContextArg qcn ty =
  printInstanceDict qcn ty
    <+> ":"
    <+> printInstanceType qcn ty

-- Prints e.g.
-- > <$a,$b>(dict$a : Eq<$a>, dict$b : Eq<$b>)
printInstanceContext :: Ts.QClassName -> [PC.Ty] -> Doc ann
printInstanceContext hsQClassName tys =
  align . group $
    encloseSep langle rangle comma (map printTyInner tys)
      <> encloseSep lparen rparen comma (printInstanceContextArg hsQClassName <$> tys)

-- printFieldName :: PC.TyName -> PC.FieldName -> Maybe (Doc ann)
-- printFieldName tyN (PC.FieldName n _) = do
--   prefix <- case Text.uncons (tyN ^. #name) of
--     Nothing -> Nothing
--     Just (h, t) -> return $ Text.cons (Char.toLower h) t
--   return $ pretty prefix <> squote <> pretty n

-- TODO: we need to check that all the variables are "simple type variables"
-- See 4.3.2 https://www.haskell.org/onlinereport/haskell2010/haskellch4.html#x10-630004.1

collectInstanceDeclTypeVars :: PC.Ty -> [PC.Ty]
collectInstanceDeclTypeVars (PC.TyAppI (PC.TyApp _ args _)) = args
collectInstanceDeclTypeVars _ = mempty

-- collectTyVars :: PC.Ty -> [PC.VarName]
-- collectTyVars = fmap (`PC.withInfoLess` id) . toList . collectVars

-- collectVars :: PC.Ty -> Set (PC.InfoLess PC.VarName)
-- collectVars = collectVars' mempty
--
-- collectVars' :: Set (PC.InfoLess PC.VarName) -> PC.Ty -> Set (PC.InfoLess PC.VarName)
-- collectVars' collected (PC.TyVarI tv) = Set.insert (PC.mkInfoLess . view #varName $ tv) collected
-- collectVars' collected (PC.TyAppI (PC.TyApp _ args _)) = collected `Set.union` (Set.unions . fmap collectVars $ args)
-- collectVars' collected _ = collected
