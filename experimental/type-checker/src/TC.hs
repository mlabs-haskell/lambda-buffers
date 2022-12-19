{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module TC where

import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader (ReaderT, runReaderT)
import Control.Monad.Trans.State (StateT, evalStateT)
import Control.Monad.Trans.Writer (WriterT (runWriterT))

import Control.Monad.Except
import Control.Monad.Reader.Class
import Control.Monad.State.Class
import Control.Monad.Writer.Class

import Control.Monad.Identity (Identity (runIdentity))
import Data.Map qualified as Map

type Atom = String
infixr 8 :->:

data Kind
  = Kind
  | Kind :->: Kind
  | KVar String
  deriving (Eq)

instance Show Kind where
  show = \case
    Kind -> "*"
    x :->: y -> show x <> " -> " <> show y
    KVar a -> a

data Type
  = Var Atom
  | App Type Type
  | Abs String Type
  deriving (Eq)

instance Show Type where
  show = \case
    Var a -> a
    App t1 t2 -> show' t1 <> " " <> show' t2
    Abs a t1 -> "λ" <> a <> "." <> show t1
    where
      show' = \case
        Var a -> a
        App t1 t2 -> ll $ show' t1 <> " " <> show' t2
        Abs a t1 -> ll $ "λ" <> a <> "." <> show' t1
      ll t = "(" <> t <> ")"

type Context = [(Atom, Kind)]

type Judgement = (Context, Type, Kind)

data Derivation
  = Axiom Judgement
  | Abstraction Judgement Derivation
  | Application Judgement Derivation Derivation
  deriving (Show, Eq)

type DError = String

type Constraint = (Kind, Kind)

type Substitution = (Atom, Kind)

newtype DerivationContext = DC
  { freshVarStream :: [Atom]
  }

type DeriveM a = ReaderT Context (StateT DerivationContext (WriterT [Constraint] (Except DError))) a

type Derive a =
  forall m.
  ( MonadReader Context m
  , MonadState DerivationContext m
  , MonadWriter [Constraint] m
  , MonadError DError m
  ) =>
  m a

runDerive' :: Type -> (Derivation, [Constraint])
runDerive' t = either error id $ runExcept $ runWriterT $ evalStateT (runReaderT (derive t) defContext) (DC atoms)

-- | Creates the derivation
derive :: Type -> Derive Derivation
derive x = do
  ctx <- ask
  case x of
    Var at -> do
      v <- getBinding at
      pure $ Axiom (ctx, x, v)
    App t1 t2 -> do
      d1 <- derive t1
      d2 <- derive t2
      let ty1 = getKind d1
          ty2 = getKind d2
      v <- KVar <$> fresh
      tell [(ty1, ty2 :->: v)]
      pure $ Application (ctx, x, v) d1 d2
    Abs v t -> do
      newTy <- KVar <$> fresh
      d <- local ((v, newTy) :) (derive t)
      let ty = getKind d
      freshT <- KVar <$> fresh
      tell [(freshT, newTy :->: ty)]
      pure $ Abstraction (ctx, x, freshT) d

getBinding :: Atom -> Derive Kind
getBinding t = do
  ctx <- ask
  case t `lookup` ctx of
    Just x -> pure x
    Nothing -> throwError $ "Unbound term:" <> t

getKind :: Derivation -> Kind
getKind = \case
  Axiom (_, _, k) -> k
  Abstraction (_, _, k) _ -> k
  Application (_, _, k) _ _ -> k

type UErr = String
type Unifier a = Except UErr a

getVariables :: Kind -> [Atom]
getVariables = \case
  Kind -> mempty
  x :->: y -> getVariables x <> getVariables y
  KVar x -> [x]

unify :: Constraint -> Unifier [Substitution]
unify (l, r) = case l of
  Kind -> case r of
    Kind -> pure mempty
    (_ :->: _) -> nope l r
    KVar v -> pure [(v, Kind)]
  x :->: y -> case r of
    Kind -> nope l r
    KVar v ->
      if v `appearsIn` l
        then appearsErr l v
        else pure [(v, l)]
    m :->: n -> (<>) <$> unify (x, m) <*> unify (y, n)
  KVar a -> case r of
    KVar b -> pure [(a, r)]
    _ -> unify (r, l)
  where
    nope l r = throwError $ unlines ["Cannot unify: ", show l <> " with " <> show r]
    appearsErr v r =
      throwError $
        unlines
          [ "Cannot unify: "
          , show v <> " with " <> show r
          , "because: " <> show v <> " appears in " <> show r
          ]
    appearsIn a l = a `elem` getVariables l

unify2 :: [Substitution] -> Unifier [Substitution]
unify2 subs = case subs of
  [] -> pure []
  (at, ty) : xs -> case lookup at xs of
    Just ty2 -> (<>) <$> unify2 xs <*> unify (ty, ty2)
    Nothing -> ((at, ty) :) <$> unify2 xs

runUnify :: [Constraint] -> Either UErr [Substitution]
runUnify constraints = runExcept $ do
  subs <- mconcat <$> mapM unify constraints
  unify2 subs

substitute :: Substitution -> Derivation -> Derivation
substitute s d = case d of
  Axiom (ctx, t, k) -> Axiom (ctx, t, applySubstitution s k)
  Abstraction j dc -> Abstraction (applySubsToJudgement s j) (substitute s dc)
  Application j d1 d2 -> Application (applySubsToJudgement s j) (substitute s d1) (substitute s d2)
  where
    applySubsToJudgement s (ctx, t, k) = (ctx, t, applySubstitution s k)
    applySubstitution subs@(a, nt) = \case
      Kind -> Kind
      l :->: r -> applySubstitution subs l :->: applySubstitution subs r
      b@(KVar ab) -> if a == ab then nt else b

-- | Testing function
getType :: Type -> IO ()
getType t = do
  let (d, c) = runDerive' t
  print (d, c)
  let s = either error id $ runUnify c
  print s
  let res = go (go d s) s
  print res
  putStrLn $ show t <> ":" <> show (getKind res)
  where
    go = foldl (flip substitute)

atoms :: [Atom]
atoms = ['a' ..] >>= \x -> ['1' ..] >>= \y -> pure [x, y]

defContext :: Context
defContext =
  [ ("Either", Kind :->: Kind :->: Kind)
  , ("Maybe", Kind :->: Kind)
  , ("(,)", Kind :->: Kind :->: Kind)
  , ("Int", Kind)
  , ("Bool", Kind)
  , ("Map", Kind :->: Kind :->: Kind)
  ]

fresh :: Derive Atom
fresh = do
  (DC vs) <- get
  case vs of
    [] -> throwError "Impossible ~ end of infinite stream"
    x : xs -> put (DC xs) >> pure x
