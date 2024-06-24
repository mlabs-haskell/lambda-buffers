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
import Data.Set (Set)
import Data.Set qualified as Set
import LambdaBuffers.Codegen.Print (throwInternalError)
import LambdaBuffers.Codegen.Typescript.Backend (MonadTypescriptBackend)
import LambdaBuffers.Codegen.Typescript.Print.Names (
  printTsQClassName,
  printTsQTyNameKey,
  printTsUnqualifiedQClassName,
  printVarName,
 )
import LambdaBuffers.Codegen.Typescript.Print.Ty (printTyInner)
import LambdaBuffers.Codegen.Typescript.Syntax qualified as Ts
import LambdaBuffers.ProtoCompat qualified as PC
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
      { _dict :: !a
      -- ^ name of the instance dictionary e.g. @Prelude.Eq[Prelude.Integer]@
      , _dictClassPart :: !a
      -- ^ the class part of the instance dictionary e.g. @Prelude.Eq@
      , _dictTypePart :: !a
      -- ^ the instance of the type e.g. @Prelude.Integer@
      }
  | ArgumentInstanceDict
      { _dict :: !a
      -- ^ name of the instance dictionary as an argument e.g. @dict$a@
      }

$(Lens.TH.makeLenses ''InstanceDict)

{- | Prints the instance dictionary corresponding to the class name and the
 given type.

 There's two cases:
    - The type is a variable, say @$a@, so print something like @dict$a@ since
      this will be a dictionary which is "passed as a parameter".

    - The type is not a variable, so the leftmost "tip" should be a type
      constructor [see WARNING below], say @TyConstr@, so print something like
      @ClassModule.ClassName[TypeModule.TyConstr]@ i.e., we refer to the
      dictionary at the toplevel.

  WARNING: This assumes that all variables are "simple type variables" as
  given in 4.3.2 of
  https://www.haskell.org/onlinereport/haskell2010/haskellch4.html#x10-630004.1
-}
printInstanceDict :: forall ann. Ts.PkgMap -> Ts.QClassName -> PC.Ty -> InstanceDict (Doc ann)
printInstanceDict pkgMap qcn ty =
  let go :: PC.Ty -> InstanceDict (Doc ann)
      go (PC.TyVarI PC.TyVar {PC.varName = varName}) =
        ArgumentInstanceDict $ "dict" <> printVarName varName
      go (PC.TyAppI PC.TyApp {PC.tyFunc = tyFunc}) = go tyFunc
      go (PC.TyRefI tyRef) =
        let qClassNameDoc = printTsQClassName qcn
            tyRefDoc = case tyRef of
              PC.LocalI PC.LocalRef {PC.tyName = tyName} -> pretty $ Lens.view #name tyName
              PC.ForeignI foreignRef ->
                printTsQTyNameKey (Ts.fromLbForeignRef pkgMap foreignRef)
         in TopLevelInstanceDict
              (mconcat [qClassNameDoc, brackets tyRefDoc])
              qClassNameDoc
              tyRefDoc
   in go ty

-- Prints e.g.
-- Eq<$a>
printInstanceType :: Ts.PkgMap -> Ts.QClassName -> PC.Ty -> Doc ann
printInstanceType pkgMap qcn ty =
  let crefDoc = printTsQClassName qcn
      tyDoc = printTyInner pkgMap ty
   in crefDoc <> surround tyDoc langle rangle

{- | Prints an instance context argument e.g.

 Eq a => (a,b)
 ^~~~ translates this part to

 dict$a : Eq<$a>
-}
printInstanceContextArg :: Ts.PkgMap -> Ts.QClassName -> PC.Ty -> Doc ann
printInstanceContextArg pkgMap qcn ty =
  Lens.view dict (printInstanceDict pkgMap qcn ty)
    <+> ":"
    <+> printInstanceType pkgMap qcn ty

-- Prints e.g.
-- > <$a,$b>(dict$a : Eq<$a>, dict$b : Eq<$b>)
printInstanceContext :: Ts.PkgMap -> Ts.QClassName -> [PC.Ty] -> Doc ann
printInstanceContext pkgMap hsQClassName tys =
  align . group $
    encloseSep langle rangle comma (map (printTyInner pkgMap) tys)
      <> encloseSep lparen rparen comma (printInstanceContextArg pkgMap hsQClassName <$> tys)

collectInstanceDeclTypeVars :: PC.Ty -> [PC.Ty]
collectInstanceDeclTypeVars = collectTyVars

collectTyVars :: PC.Ty -> [PC.Ty]
collectTyVars = Set.toList . collectVars

collectVars :: PC.Ty -> Set PC.Ty
collectVars = collectVars' mempty

collectVars' :: Set PC.Ty -> PC.Ty -> Set PC.Ty
collectVars' collected ty@(PC.TyVarI _tv) = Set.insert ty collected
collectVars' collected (PC.TyAppI (PC.TyApp _ args _)) = collected `Set.union` (Set.unions . fmap collectVars $ args)
collectVars' collected _ = collected

-- See the INVARIANT note below
printExportInstanceDecl :: MonadTypescriptBackend m => Ts.PkgMap -> Ts.QClassName -> PC.Ty -> m (Doc ann -> Doc ann)
printExportInstanceDecl pkgMap tsQClassName ty = do
  let
    instanceType = printInstanceType pkgMap tsQClassName ty

    lhsInstanceDecl = printInstanceDict pkgMap tsQClassName ty

    instanceDeclTypeVars = collectInstanceDeclTypeVars ty

  (dictDoc, _classDoc, tyDoc) <- case lhsInstanceDecl of
    TopLevelInstanceDict dictDoc classDoc tyDoc ->
      return (dictDoc, classDoc, tyDoc)
    _other ->
      -- TODO(jaredponn): the 'def' is the default source info, but we really
      -- should put the proper source information in here.
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
        -- export type Eq = ...
        -- ```
        -- So, by declaration merging [1], we can extend the
        -- value @Eq@ with new instances with something like
        -- ```
        -- declare module "./Eq.js" {
        --     export interface EqInstances {
        --         [Prelude.Bool]: Prelude.Eq<Prelude.Bool>
        --     }
        -- }
        -- where `Bool` is a unique symbol for the type `Bool`
        -- ```
        --
        -- This is exactly what is being printed.
        --
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
                [ -- TODO(jaredponn): typeclasses probably aren't supported..
                  "export" <+> "interface" <+> (printTsUnqualifiedQClassName tsQClassName <> "Instances")
                , lbrace
                , indent 2 $
                    vsep
                      [ brackets tyDoc
                          <+> colon
                          <+> case instanceDeclTypeVars of
                            [] -> instanceType
                            _other -> printInstanceContext pkgMap tsQClassName instanceDeclTypeVars <+> "=>" <+> instanceType
                      ]
                , rbrace
                ]
          , rbrace
          ]

      dictDeclDoc =
        vsep
          [ case instanceDeclTypeVars of
              [] -> dictDoc <+> equals <+> vsep [lbrace, indent 2 bodyDoc, rbrace]
              _other ->
                vsep
                  [ dictDoc <+> equals <+> "function" <> printInstanceContext pkgMap tsQClassName instanceDeclTypeVars <+> colon <+> instanceType
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
