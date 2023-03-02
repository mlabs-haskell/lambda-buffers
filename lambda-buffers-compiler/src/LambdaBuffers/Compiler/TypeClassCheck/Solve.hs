{-# LANGUAGE LambdaCase #-}

module LambdaBuffers.Compiler.TypeClassCheck.Solve (solve, inst, defTag, Overlap (..)) where

import Control.Monad.Except (throwError)
import Control.Monad.Reader (ReaderT, runReaderT)
import Control.Monad.Reader.Class (MonadReader (ask))
import Control.Monad.Writer.Class (MonadWriter (tell))
import Control.Monad.Writer.Strict (WriterT, execWriterT)
import Data.Foldable (traverse_)
import Data.Map qualified as M
import Data.Set qualified as S
import Data.Text (Text)
import LambdaBuffers.Compiler.ProtoCompat (defSourceInfo)
import LambdaBuffers.Compiler.TypeClassCheck.Pat (Exp (AppE, ConsE, DecE, LabelE, LitE, NilE, ProdE, RecE, RefE, SumE), ExpressionLike ((*:), (*=)), Literal (TyVar), Pat (AppP, ConsP, DecP, LabelP, LitP, NilP, ProdP, RecP, RefP, SumP, VarP), Tagged (Tag), matches)
import LambdaBuffers.Compiler.TypeClassCheck.Rules (Class (csupers), Constraint (C), Rule ((:<=)), ruleClass, ruleHead)

{- Pattern/Template/Unification variable  substitution.
   Given a string that represents a variable name,
   and a type to instantiate variables with that name to,
   performs the instantiation
-}
subV :: M.Map Text Exp -> Pat -> Exp
subV cxt = \case
  (VarP v) -> case M.lookup v cxt of
    Nothing -> LitE (TyVar v)
    Just t -> t
  ConsP x xs -> subV cxt x *: subV cxt xs
  LabelP l x -> subV cxt l *= subV cxt x
  ProdP xs -> ProdE (subV cxt xs)
  RecP xs -> RecE (subV cxt xs)
  SumP xs -> SumE (subV cxt xs)
  AppP t1 t2 -> AppE (subV cxt t1) (subV cxt t2)
  RefP n x -> RefE (subV cxt n) (subV cxt x)
  DecP a b c -> DecE (subV cxt a) (subV cxt b) (subV cxt c)
  LitP l -> LitE l
  NilP -> NilE

inst :: Pat -> Exp
inst = subV M.empty

{- Performs substitution on an entire instance (the first argument) given the
   concrete types from a Pat (the second argument).
   Note that ONLY PatVars which occur in the Instance *HEAD* are replaced, though they
   are replaced in the instance superclasses as well (if they occur there).
-}
subst :: Rule Pat -> Exp -> Rule Exp
subst cst@(C _ t :<= _) ty = fmap (subV (getSubs t ty)) cst

{- Given two patterns (which are hopefully structurally similar), gather a list of all substitutions
   from the PatVars in the first argument to the concrete types (hopefully!) in the second argument
-}
getSubs :: Pat -> Exp -> M.Map Text Exp -- should be a set, whatever
getSubs (VarP s) t = M.fromList [(s, t)]
getSubs (ConsP x xs) (ConsE x' xs') = getSubs x x' <> getSubs xs xs'
getSubs (LabelP l t) (LabelE l' t') = getSubs l l' <> getSubs t t'
getSubs (ProdP xs) (ProdE xs') = getSubs xs xs'
getSubs (RecP xs) (RecE xs') = getSubs xs xs'
getSubs (SumP xs) (SumE xs') = getSubs xs xs'
getSubs (AppP t1 t2) (AppE t1' t2') = getSubs t1 t1' <> getSubs t2 t2'
getSubs (RefP n t) (RefE n' t') = getSubs n n' <> getSubs t t'
getSubs (DecP a b c) (DecE a' b' c') = getSubs a a' <> getSubs b b' <> getSubs c c'
getSubs _ _ = M.empty

-- NoMatch isn't fatal but OverlappingMatches is (i.e. we need to stop when we encounter it)
data MatchResult
  = NoMatch
  | OverlappingMatches [Tagged (Rule Pat)]
  | MatchFound (Tagged (Rule Pat))

-- for SolveM, since we catch NoMatch
data Overlap = Overlap (Tagged (Constraint Exp)) [Tagged (Rule Pat)]
  deriving stock (Show, Eq)

selectMatchingInstance :: Exp -> Class -> [Tagged (Rule Pat)] -> MatchResult
selectMatchingInstance e c rs = case filter matchPatAndClass rs of
  [] -> NoMatch
  [r] -> MatchFound r
  overlaps -> OverlappingMatches overlaps
  where
    matchPatAndClass :: Tagged (Rule Pat) -> Bool
    matchPatAndClass (Tag _ r) =
      ruleClass r == c
        && ruleHead r
        `matches` e

type SolveM = ReaderT [Tagged (Rule Pat)] (WriterT (S.Set (Tagged (Constraint Exp))) (Either Overlap))

defTag :: a -> Tagged a
defTag = Tag defSourceInfo

{- Given a list of instances (the initial scope), determines whether we can derive
   an instance of the Class argument for the Pat argument. A result of [] indicates that there are
   no remaining subgoals and that the constraint has been solved.
   NOTE: At the moment this handles superclasses differently than you might expect -
         instead of assuming that the superclasses for all in-scope classes are defined,
         we check that those constraints can be solved before affirmatively judging that the
         target constraint has been solved. I *think* that makes sense in this context (whereas in Haskell
         it doesn't b/c it's *impossible* to have `instance Foo X` if the definition of Foo is
         `class Bar y => Foo y` without an `instance Bar X`)
-}
solveM :: Tagged (Constraint Exp) -> SolveM ()
-- TODO(@gnumonik): Explain why we have this TyVar rule here
solveM (Tag _ (C _ (LitE (TyVar _)))) = pure ()
solveM cst@(Tag _ (C c pat)) =
  ask >>= \inScope ->
    case selectMatchingInstance pat c inScope of
      NoMatch -> tell $ S.singleton cst
      OverlappingMatches olps -> throwError $ Overlap cst olps
      MatchFound rule -> case flip subst pat <$> rule of
        Tag _ (C _ p :<= []) -> solveClassesFor p (csupers c)
        Tag _ (C _ _ :<= is) -> do
          traverse_ (solveM . defTag) is
          solveClassesFor pat (csupers c)
  where
    -- Given a Pat and a list of Classes, attempt to solve the constraints
    -- constructed from the Pat and each Class
    solveClassesFor :: Exp -> [Class] -> SolveM ()
    solveClassesFor p = traverse_ (\cls -> solveM (defTag $ C cls p))

solve :: [Tagged (Rule Pat)] -> Tagged (Constraint Exp) -> Either Overlap [Tagged (Constraint Exp)]
solve rules c = fmap S.toList $ execWriterT $ runReaderT (solveM c) rules
