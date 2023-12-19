module LambdaBuffers.Codegen.Typescript.Print.InstanceDef (
  printExportInstanceDecl,
  printInstanceDict,
  InstanceDict (..),
  dict,
  dictClassPart,
  dictTypePart,
) where

import Control.Lens qualified as Lens
import Control.Lens.TH qualified as Lens.TH
import Data.Default (Default (def))
import Data.Text (Text)
import LambdaBuffers.Codegen.Print (throwInternalError)
import LambdaBuffers.Codegen.Typescript.Print.MonadPrint (MonadPrint)
import LambdaBuffers.Codegen.Typescript.Print.Names (
  printTsQClassName,
  printTsQTyNameKey,
  printTsUnqualifiedQClassName,
  printVarName,
 )
import LambdaBuffers.Codegen.Typescript.Print.Ty (printTyInner)
import LambdaBuffers.Codegen.Typescript.Syntax qualified as Ts
import LambdaBuffers.ProtoCompat qualified as PC

-- import Data.Text qualified as Text
import Prettyprinter (
  Doc,
  align,
  brackets,
  colon,
  comma,
  dquotes,
  encloseSep,
  equals,
  group,
  indent,
  langle,
  lbrace,
  lparen,
  pretty,
  rangle,
  rbrace,
  rparen,
  surround,
  vsep,
  (<+>),
 )

-- | See 'printInstanceDict'
data InstanceDict a
  = TopLevelInstanceDict
      { _dict :: a
      -- ^ name of the instance dictionary e.g. @Prelude.Eq[Prelude.Integer]@
      , _dictClassPart :: a
      -- ^ the class part of the instance dictionary e.g. @Prelude.Eq@
      , _dictTypePart :: a
      -- ^ the instance of the type e.g. @Prelude.Integer@
      }
  | ArgumentInstanceDict
      { _dict :: a
      -- ^ name of the instance dictionary as an argument e.g. @dict$a@
      }

$(Lens.TH.makeLenses ''InstanceDict)

{- | Prints the name of the instance defn. e.g., given instance `Prelude.Eq
 Integer`,  this prints `eqInteger`.

 TODO(jaredponn): legacy remove this.
-}

-- printInstanceDecl :: forall ann. Ts.QClassName -> PC.Ty -> Doc ann
-- printInstanceDecl qcn ty =
--   -- See 4.3.2 of the Haskell report
--   -- https://www.haskell.org/onlinereport/haskell2010/haskellch4.html#x10-630004.1
--   -- for the form we assume the instance declarations to be in i.e., the
--   -- "leftmost" type determines the instance.
--   --
--   -- TODO: when the leftmost type is a type variable, there is at most one
--   -- instance that unambiguous instance that may be applied, so we simply
--   -- print out the type class name (in lower case).
--   let go :: PC.Ty -> Doc ann
--       go (PC.TyVarI _) = mempty
--       go (PC.TyAppI PC.TyApp {PC.tyFunc = tyFunc}) = go tyFunc
--       go (PC.TyRefI tyRef) = pretty $
--         case tyRef of
--           PC.LocalI PC.LocalRef {PC.tyName = tyName} -> getTyName tyName
--           PC.ForeignI PC.ForeignRef {PC.tyName = tyName} ->
--             -- TODO: this is broken for foreign types with the same name as
--             -- a type declared in this module.
--             getTyName tyName
--    in printTsQClassName qcn <> go ty

getTyName :: PC.TyName -> Text
getTyName (PC.TyName tyName _) = tyName

{- | Prints the instance dictionary corresponding to the class name and the
 given type.

 Note:
  If the given type is a type variable, we print the variable prefixed with
  @dict@ e.g. given type variable @$a@, we print @dict$a@. This is because
  dictionaries for a type variable will be passed as a parameter.
-}
printInstanceDict :: forall ann. Ts.QClassName -> PC.Ty -> InstanceDict (Doc ann)
printInstanceDict qcn ty =
  let go :: PC.Ty -> InstanceDict (Doc ann)
      go (PC.TyVarI PC.TyVar {PC.varName = varName}) =
        ArgumentInstanceDict $ "dict" <> printVarName varName
      go (PC.TyAppI PC.TyApp {PC.tyFunc = tyFunc}) = go tyFunc
      go (PC.TyRefI tyRef) =
        let qClassNameDoc = printTsQClassName qcn
            tyRefDoc = case tyRef of
              PC.LocalI PC.LocalRef {PC.tyName = tyName} -> pretty $ getTyName tyName
              PC.ForeignI foreignRef ->
                printTsQTyNameKey (Ts.fromLbForeignRef foreignRef)
         in -- PC.ForeignI PC.ForeignRef {PC.tyName = tyName} ->
            --   -- TODO: this is broken for foreign types with the same name as
            --   -- a type declared in this module.
            --   getTyName tyName
            TopLevelInstanceDict
              (mconcat [qClassNameDoc, brackets tyRefDoc])
              qClassNameDoc
              tyRefDoc
   in go ty

-- Prints e.g.
-- Eq<$a>
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
  Lens.view dict (printInstanceDict qcn ty)
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

printExportInstanceDecl :: MonadPrint m => Ts.QClassName -> PC.Ty -> m (Doc ann -> Doc ann)
printExportInstanceDecl tsQClassName ty = do
  let
    -- instanceName = printInstanceDecl tsQClassName ty
    instanceType = printInstanceType tsQClassName ty

    lhsInstanceDecl = printInstanceDict tsQClassName ty

    instanceDeclTypeVars = collectInstanceDeclTypeVars ty

  (dictDoc, _classDoc, tyDoc) <- case lhsInstanceDecl of
    TopLevelInstanceDict dictDoc classDoc tyDoc ->
      return (dictDoc, classDoc, tyDoc)
    _ ->
      -- TODO(jaredponn): get the source info right..
      throwInternalError def "TODO(jaredponn): Invalid type class instance for TypeScript backend"

  return $ \bodyDoc ->
    let
      declarationMergingInstance =
        -- INVARIANT:
        -- The file which declares the class must have code like
        -- (e.g. with @Eq@)
        -- ```
        -- interface EqInstances {};
        -- export const Eq : EqInstances = {} as EqInstances;
        -- ```
        -- So, by declaration merging [1], we can extend the
        -- variable @Eq@ with new instances with something like
        -- ```
        -- declare module "./Eq.js" {
        --     export interface EqInstances {
        --         [Prelude.Bool]: Prelude.Eq<Prelude.Bool>
        --     }
        -- }
        -- where `Bool` is a unique symbol for the type `Bool`
        -- ```
        --
        -- References
        --  [1]: https://www.typescriptlang.org/docs/handbook/declaration-merging.html
        vsep
          [ "declare"
              <+> "module"
              <+> dquotes (Lens.view (Lens._1 . Lens.to (pretty . Ts.pkgNameToText)) tsQClassName)
          , lbrace
          , indent 2 $
              vsep
                [ "export" <+> "interface" <+> (printTsUnqualifiedQClassName tsQClassName <> "Instances")
                , lbrace
                , indent 2 $
                    vsep
                      [ brackets tyDoc
                          <+> colon
                          <+> case instanceDeclTypeVars of
                            [] -> instanceType
                            _ -> printInstanceContext tsQClassName instanceDeclTypeVars <+> "=>" <+> instanceType
                      ]
                , rbrace
                ]
          , rbrace
          ]

      dictDeclDoc =
        vsep
          [ case instanceDeclTypeVars of
              [] -> dictDoc <+> equals <+> bodyDoc
              _ ->
                vsep
                  [ dictDoc <+> equals <+> "function" <> printInstanceContext tsQClassName instanceDeclTypeVars <+> colon <+> instanceType
                  , indent 2 $
                      vsep
                        [ lbrace
                        , "return"
                            <+> indent
                              2
                              ( vsep
                                  [ lbrace
                                  , bodyDoc
                                  , rbrace
                                  ]
                              )
                        , rbrace
                        ]
                  ]
          ]
     in
      vsep
        [ declarationMergingInstance
        , dictDeclDoc
        ]

{-
case instanceDeclTypeVars of
  [] ->
    return $
      ExportConstInstanceDecl $
        "export" <+> "const" <+> instanceName <+> ":" <+> instanceType <> line
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
-}
