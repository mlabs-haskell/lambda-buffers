{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module LambdaBuffers.Compiler.KindCheck (
  -- * Kindchecking functions.
  check,
  check_,

  -- * Testing Utils.
  foldWithSum,
  foldWithArrowToType,
  foldWithProduct,
  foldWithApp,
) where

import Control.Lens (view, (&), (.~), (^.))
import Control.Monad (void)
import Control.Monad.Freer (Eff, Member, Members, interpret, reinterpret, run)
import Control.Monad.Freer.Error (Error, runError, throwError)
import Control.Monad.Freer.Reader (Reader, ask, runReader)
import Control.Monad.Freer.State (State, evalState, modify)
import Control.Monad.Freer.TH (makeEffect)
import Control.Monad.Freer.Writer (Writer, runWriter, tell)
import Data.Foldable (Foldable (foldl', toList), traverse_)
import Data.Map qualified as M
import Data.Text (Text, pack)
import LambdaBuffers.Compiler.KindCheck.Context (Context)
import LambdaBuffers.Compiler.KindCheck.Inference (
  InferErr (
    InferImpossibleErr,
    InferRecursiveSubstitutionErr,
    InferUnboundTermErr,
    InferUnifyTermErr
  ),
  Kind (Type, (:->:)),
  Type (Abs, Var),
  context,
  infer,
 )
import LambdaBuffers.Compiler.KindCheck.Inference qualified as I
import LambdaBuffers.Compiler.KindCheck.Kind (kind2ProtoKind)
import LambdaBuffers.Compiler.KindCheck.Type (Type (App), tyOpaque, tyProd, tySum, tyUnit, tyVoid)
import LambdaBuffers.Compiler.KindCheck.Variable (Variable (ForeignRef, LocalRef))
import LambdaBuffers.Compiler.ProtoCompat ()
import LambdaBuffers.Compiler.ProtoCompat.Types qualified as PC
import Prettyprinter (Pretty (pretty))

--------------------------------------------------------------------------------
-- Types

-- | Kind Check failure types.
type CompilerErr = PC.CompilerError

type Err = Error CompilerErr

type ModName = [Text]

-- | Main interface to the Kind Checker.
data Check a where
  KCheck :: PC.CompilerInput -> Check Context

makeEffect ''Check

-- | Interactions that happen at the level of the Global Checker.
data GlobalCheck a where
  CreateContext :: PC.CompilerInput -> GlobalCheck Context
  ValidateModule :: Context -> PC.Module -> GlobalCheck ()

makeEffect ''GlobalCheck

-- | Interactions that happen at the level of the
data ModuleCheck a where -- Module
  KCTypeDefinition :: ModName -> Context -> PC.TyDef -> ModuleCheck Kind

-- NOTE(cstml & gnumonik): Lets reach consensus on these - Note(1).
--  KCClassInstance :: Context -> P.InstanceClause -> ModuleCheck ()
--  KCClass :: Context -> P.ClassDef -> ModuleCheck ()

makeEffect ''ModuleCheck

data KindCheck a where
  TypesFromTyDef :: ModName -> PC.TyDef -> KindCheck [Type]
  InferTypeKind :: ModName -> PC.TyDef -> Context -> Type -> KindCheck Kind
  CheckKindConsistency :: ModName -> PC.TyDef -> Context -> Kind -> KindCheck Kind

makeEffect ''KindCheck

--------------------------------------------------------------------------------

-- | The Check effect runner.
runCheck :: Eff '[Check, Err] a -> Either CompilerErr a
runCheck = run . runError . runKindCheck . localStrategy . moduleStrategy . globalStrategy

{- | Run the check - return the validated context or the failure. The main API
 function of the library.
-}
check :: PC.CompilerInput -> PC.CompilerOutput
check = fmap (const PC.CompilerResult) . runCheck . kCheck

-- | Run the check - drop the result if it succeeds - useful for testing.
check_ :: PC.CompilerInput -> Either CompilerErr ()
check_ = void . runCheck . kCheck

--------------------------------------------------------------------------------

{- | A transformation (in the context of the Kind Checker) is a mapping from one
 Effect to another. All effects can fial via the `Err` effect - which is
 essentially the Kind Check failure.
-}
type Transform x y = forall effs {a}. Member Err effs => Eff (x ': effs) a -> Eff (y ': effs) a

-- Transformation strategies
globalStrategy :: Transform Check GlobalCheck
globalStrategy = reinterpret $ \case
  KCheck ci -> do
    ctx <- createContext ci
    void $ validateModule ctx `traverse` (ci ^. #modules)
    pure ctx

moduleStrategy :: Transform GlobalCheck ModuleCheck
moduleStrategy = reinterpret $ \case
  CreateContext ci -> evalState (mempty @(M.Map Variable PC.TyDef)) . resolveCreateContext $ ci
  ValidateModule cx md -> do
    traverse_ (kCTypeDefinition (module2ModuleName md) cx) (md ^. #typeDefs)

localStrategy :: Transform ModuleCheck KindCheck
localStrategy = reinterpret $ \case
  KCTypeDefinition mname ctx tydef -> do
    typesFromTyDef mname tydef
      >>= traverse (inferTypeKind mname tydef ctx)
      >>= traverse (checkKindConsistency mname tydef ctx)
      >>= traverse (checkKindConsistency mname tydef ctx)
      >>= \case
        [] -> pure Type -- Void
        x : _ -> pure x -- The Kind of the first constructor ~ already checked
        -- and consistent.
        {- See note (1).
        --  KCClassInstance _ctx _instClause -> pure ()
        --  KCClass _ctx _classDef -> pure ()
        -}

-- | Internal to External term association map ~ a mapping between a Variable and the term it originated from. Allows us to throw meaningful errors.
type IETermMap = M.Map Variable (Either PC.TyVar PC.TyRef)

type HandleErrorEnv a = Eff '[Reader ModName, Writer IETermMap] a

runKindCheck :: forall effs {a}. Member Err effs => Eff (KindCheck ': effs) a -> Eff effs a
runKindCheck = interpret $ \case
  TypesFromTyDef modName tydef -> runReader modName (tyDef2Types tydef)
  InferTypeKind modName tyDef ctx ty -> either (handleErr modName tyDef) pure $ infer ctx ty
  CheckKindConsistency modName def ctx k -> runReader modName $ resolveKindConsistency def ctx k
  where
    handleErr :: forall {b}. ModName -> PC.TyDef -> InferErr -> Eff effs b
    handleErr modName td = \case
      InferUnboundTermErr uA -> do
        tt <- getTermType modName td uA
        throwError . PC.CompKindCheckError $ case tt of
          Right r -> PC.UnboundTyRefError td r
          Left l -> PC.UnboundTyVarError td l
      InferUnifyTermErr (I.Constraint (k1, k2)) ->
        throwError . PC.CompKindCheckError $ PC.IncorrectApplicationError (tyDef2TyName td) (kind2ProtoKind k1) (kind2ProtoKind k2)
      InferRecursiveSubstitutionErr _ ->
        throwError . PC.CompKindCheckError $ PC.RecursiveKindError $ tyDef2TyName td
      InferImpossibleErr t ->
        throwError . PC.InternalError $ t

    -- Gets the original term associated with the Variable.
    getTermType :: ModName -> PC.TyDef -> Variable -> Eff effs (Either PC.TyVar PC.TyRef)
    getTermType modName td va = do
      let termMap = snd . run . runWriter . runReader modName $ tyDef2Map td
      case termMap M.!? va of
        Just x -> pure x
        Nothing ->
          throwError . PC.InternalError . pack . unlines $
            [ "Could not find the corresponding source info for:"
            , show . pretty $ va
            , "This should never happen."
            , "Please report error."
            ]
      where
        {- Conversion functions that associate a variable with its KC term.
        The Monad has a Writer Map instance which is then used to retrieve sourceInfo
        about the term.
        -}

        tyDef2Map :: PC.TyDef -> HandleErrorEnv ()
        tyDef2Map = tyAbs2Map . view #tyAbs

        -- Note(cstml): Is there any purpose for anything from the tyArg - be sure to cover with tests.
        tyAbs2Map :: PC.TyAbs -> HandleErrorEnv ()
        tyAbs2Map tyAbs = tyBody2Map (tyAbs ^. #tyBody)

        tyBody2Map :: PC.TyBody -> HandleErrorEnv ()
        tyBody2Map = \case
          PC.OpaqueI _ -> pure ()
          PC.SumI s -> sum2Map s

        sum2Map :: PC.Sum -> HandleErrorEnv ()
        sum2Map (PC.Sum constr _) = traverse_ constr2Map $ M.elems constr

        constr2Map :: PC.Constructor -> HandleErrorEnv ()
        constr2Map = product2Map . view #product

        product2Map :: PC.Product -> HandleErrorEnv ()
        product2Map = \case
          PC.RecordI r -> record2Map r
          PC.TupleI t -> tuple2Map t

        record2Map :: PC.Record -> HandleErrorEnv ()
        record2Map r = traverse_ field2Map $ r ^. #fields

        field2Map :: PC.Field -> HandleErrorEnv ()
        field2Map = ty2Map . view #fieldTy

        ty2Map :: PC.Ty -> HandleErrorEnv ()
        ty2Map = \case
          PC.TyVarI tvar ->
            tell @IETermMap $ M.singleton (tyVar2Variable tvar) (Left tvar)
          PC.TyAppI tapp -> do
            ty2Map $ tapp ^. #tyFunc
            traverse_ ty2Map $ tapp ^. #tyArgs
          PC.TyRefI tyref -> do
            var <- tyRef2Variable tyref
            tell @IETermMap $ M.singleton var (Right tyref)

        tuple2Map :: PC.Tuple -> HandleErrorEnv ()
        tuple2Map = traverse_ ty2Map . view #fields

-- Resolvers

resolveKindConsistency ::
  forall effs.
  Members '[Reader ModName, Err] effs =>
  PC.TyDef ->
  Context ->
  Kind ->
  Eff effs Kind
resolveKindConsistency tydef _ctx inferredKind = do
  mName <- ask @ModName
  let tyName = tyDef2TyName tydef
  (_, k) <- tyDef2NameAndKind mName tydef
  guard tyName k inferredKind
  pure inferredKind
  where
    guard :: PC.TyName -> Kind -> Kind -> Eff effs ()
    guard n i d
      | i == d = pure ()
      | otherwise =
          throwError . PC.CompKindCheckError $
            PC.InconsistentTypeError n (kind2ProtoKind i) (kind2ProtoKind d)

tyDef2TyName :: PC.TyDef -> PC.TyName
tyDef2TyName (PC.TyDef n _ _) = n

--------------------------------------------------------------------------------
-- Context Creation

{- | Resolver function for the context creation - it fails if two identical
 declarations are found.
-}
resolveCreateContext ::
  forall effs.
  Member (State (M.Map Variable PC.TyDef)) effs =>
  Member Err effs =>
  PC.CompilerInput ->
  Eff effs Context
resolveCreateContext ci = do
  ctxs <- traverse module2Context (toList $ ci ^. #modules)
  pure $ mconcat ctxs

module2Context ::
  forall effs.
  Member (State (M.Map Variable PC.TyDef)) effs =>
  Member Err effs =>
  PC.Module ->
  Eff effs Context
module2Context m = do
  let typeDefinitions = toList $ m ^. #typeDefs
  ctxs <- runReader (m ^. #moduleName) $ do
    traverse (tyDef2Context (moduleName2ModName (m ^. #moduleName))) typeDefinitions
  pure $ mconcat ctxs

-- | Creates a Context entry from one type definition.
tyDef2Context ::
  forall effs.
  Member (Reader PC.ModuleName) effs =>
  Member (State (M.Map Variable PC.TyDef)) effs =>
  Member Err effs =>
  ModName ->
  PC.TyDef ->
  Eff effs Context
tyDef2Context curModName tyDef = do
  r@(v, _) <- tyDef2NameAndKind curModName tyDef
  ctx2 <- tyDefArgs2Context tyDef
  associateName v tyDef
  pure $ mempty & context .~ uncurry M.singleton r <> ctx2
  where
    -- Ads the name to our map - we can use its SourceLocation in the case of a
    -- double use. If it's already in our map - that means we've double declared it.
    associateName :: Variable -> PC.TyDef -> Eff effs ()
    associateName v curTyDef = modify (M.insert v curTyDef)

{- | Gets the kind of the variables from the definition and adds them to the
 context.
-}
tyDefArgs2Context :: PC.TyDef -> Eff effs (M.Map Variable Kind)
tyDefArgs2Context tydef = do
  let ds = g <$> M.elems (tydef ^. #tyAbs . #tyArgs)
  pure $ M.fromList ds
  where
    g :: PC.TyArg -> (Variable, Kind)
    g tyarg = (v, k)
      where
        v = LocalRef (tyarg ^. #argName . #name)
        k = pKind2Kind (tyarg ^. #argKind)

{- | Converts the Proto Module name to a local modname - dropping the
 information.
-}
moduleName2ModName :: PC.ModuleName -> ModName
moduleName2ModName mName = (\p -> p ^. #name) <$> mName ^. #parts

tyDef2NameAndKind :: forall effs. ModName -> PC.TyDef -> Eff effs (Variable, Kind)
tyDef2NameAndKind curModName tyDef = do
  -- all names are qualified
  let name = ForeignRef curModName (tyDef ^. #tyName . #name)
  let k = tyAbsLHS2Kind (tyDef ^. #tyAbs)
  pure (name, k)

tyAbsLHS2Kind :: PC.TyAbs -> Kind
tyAbsLHS2Kind tyAbs = foldWithArrowToType $ pKind2Kind . (\x -> x ^. #argKind) <$> toList (tyAbs ^. #tyArgs)

{- | Folds kinds and appends them to a Kind result type. In essence creates a
 curried function with a Type final kind.

 ghc> foldWithArrowToType []
 Type

 ghc> foldWithArrowToType [Type]
 Type -> Type

 ghc> foldWithArrowToType [Type, (Type -> Type)]
 Type -> (Type -> Type) -> Type
-}
foldWithArrowToType :: [Kind] -> Kind
foldWithArrowToType = foldr (:->:) Type

-- =============================================================================
-- To Kind Conversion functions

pKind2Kind :: PC.Kind -> Kind
pKind2Kind k =
  case k ^. #kind of
    PC.KindRef PC.KType -> Type
    PC.KindArrow l r -> pKind2Kind l :->: pKind2Kind r
    -- NOTE(cstml): What is an undefined Kind type meant to mean?
    _ -> error "Fixme undefined type"

-- =============================================================================
-- X To Canonical type conversion functions.

tyAbsLHS2Type ::
  forall eff.
  PC.TyAbs ->
  Eff eff (Type -> Type)
tyAbsLHS2Type tyab = tyArgs2Type (toList $ tyab ^. #tyArgs)

tyArgs2Type ::
  forall eff.
  [PC.TyArg] ->
  Eff eff (Type -> Type)
tyArgs2Type = \case
  [] -> pure id
  x : xs -> do
    f <- tyArgs2Type xs
    pure $ \c -> Abs (tyArg2Var x) (f c)

tyArg2Var :: PC.TyArg -> Variable
tyArg2Var = LocalRef . view (#argName . #name)

constructor2Type ::
  forall eff.
  Members '[Reader ModName, Err] eff =>
  PC.Constructor ->
  Eff eff Type
constructor2Type co = product2Type (co ^. #product)

product2Type ::
  forall eff.
  Members '[Reader ModName, Err] eff =>
  PC.Product ->
  Eff eff Type
product2Type = \case
  PC.RecordI r -> record2Type r
  PC.TupleI t -> tuple2Type t

record2Type ::
  forall eff.
  Members '[Reader ModName, Err] eff =>
  PC.Record ->
  Eff eff Type
record2Type r = foldWithProduct <$> traverse field2Type (toList $ r ^. #fields)

tuple2Type ::
  forall eff.
  Members '[Reader ModName, Err] eff =>
  PC.Tuple ->
  Eff eff Type
tuple2Type tu = do
  tup <- traverse ty2Type $ tu ^. #fields
  pure . foldWithProduct $ tup

field2Type ::
  forall eff.
  Members '[Reader ModName, Err] eff =>
  PC.Field ->
  Eff eff Type
field2Type f = ty2Type (f ^. #fieldTy)

ty2Type ::
  forall eff.
  Members '[Reader ModName, Err] eff =>
  PC.Ty ->
  Eff eff Type
ty2Type = \case
  PC.TyVarI tytv -> tyVar2Type tytv
  PC.TyAppI tyap -> tyApp2Type tyap
  PC.TyRefI tyre -> tyRef2Type tyre

tyVar2Type ::
  forall eff.
  PC.TyVar ->
  Eff eff Type
tyVar2Type = pure . Var . tyVar2Variable

tyVar2Variable :: PC.TyVar -> Variable
tyVar2Variable = LocalRef . view (#varName . #name)

tyApp2Type ::
  forall eff.
  Members '[Reader ModName, Err] eff =>
  PC.TyApp ->
  Eff eff Type
tyApp2Type ta = do
  fn <- ty2Type (ta ^. #tyFunc)
  args <- traverse ty2Type (toList $ ta ^. #tyArgs)
  pure $ foldWithApp fn args

tyRef2Type ::
  forall eff.
  Members '[Reader ModName, Err] eff =>
  PC.TyRef ->
  Eff eff Type
tyRef2Type = fmap Var . tyRef2Variable

tyRef2Variable ::
  forall eff.
  Members '[Reader ModName] eff =>
  PC.TyRef ->
  Eff eff Variable
tyRef2Variable = \case
  PC.LocalI lref -> localTyRef2Variable lref
  PC.ForeignI fref -> foreignTyRef2Variable fref

localTyRef2Variable ::
  forall eff.
  Members '[Reader ModName] eff =>
  PC.LocalRef ->
  Eff eff Variable
localTyRef2Variable ltr = do
  moduleName <- ask
  pure $ ForeignRef moduleName (ltr ^. #tyName . #name)

foreignTyRef2Variable ::
  forall eff.
  Members '[Reader ModName] eff =>
  PC.ForeignRef ->
  Eff eff Variable
foreignTyRef2Variable ftr = do
  let moduleName = moduleName2ModName (ftr ^. #moduleName)
  pure $ ForeignRef moduleName (ftr ^. #tyName . #name)

-- =============================================================================
-- X To Canonical type conversion functions.

{- | TyDef to Kind Canonical representation - sums not folded - therefore we get
 constructor granularity. Might use in a different implementation for more
 granular errors.
-}
tyDef2Types ::
  forall eff.
  Members '[Reader ModName, Err] eff =>
  PC.TyDef ->
  Eff eff [Type]
tyDef2Types tyde = do
  f <- tyAbsLHS2Type (tyde ^. #tyAbs) -- abstraction
  cs <- tyAbsRHS2Types (tyde ^. #tyAbs) --
  pure $ f <$> cs

tyAbsRHS2Types ::
  forall eff.
  Members '[Reader ModName, Err] eff =>
  PC.TyAbs ->
  Eff eff [Type]
tyAbsRHS2Types tyab = tyBody2Types (tyab ^. #tyBody)

tyBody2Types ::
  forall eff.
  Members '[Reader ModName, Err] eff =>
  PC.TyBody ->
  Eff eff [Type]
tyBody2Types = \case
  PC.OpaqueI _ -> pure [Var tyOpaque]
  PC.SumI s -> sum2Types s

sum2Types ::
  forall eff.
  Members '[Reader ModName, Err] eff =>
  PC.Sum ->
  Eff eff [Type]
sum2Types su = traverse constructor2Type $ M.elems (su ^. #constructors)

--------------------------------------------------------------------------------
-- Utilities

foldWithApp :: Type -> [Type] -> Type
foldWithApp = foldl' App

foldWithProduct :: [Type] -> Type
foldWithProduct = foldl' (App . App (Var tyProd)) (Var tyUnit)

foldWithSum :: [Type] -> Type
foldWithSum = foldl' (App . App (Var tySum)) (Var tyVoid)

module2ModuleName :: PC.Module -> ModName
module2ModuleName = moduleName2ModName . (^. #moduleName)
