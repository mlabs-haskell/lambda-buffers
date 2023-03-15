-- | unification-fd based solver.
module LambdaBuffers.Compiler.MiniLog.UniFdSolver (solve) where

import Control.Monad (filterM, foldM, foldM_, void)
import Control.Monad.Error.Class (MonadError (catchError, throwError))
import Control.Monad.Except (ExceptT, runExceptT)
import Control.Monad.Reader (MonadReader (local), ReaderT (runReaderT), asks)
import Control.Monad.Trans (MonadTrans (lift))
import Control.Monad.Writer (MonadWriter (tell), Writer, runWriter)
import Control.Unification (Fallible, Unifiable (zipMatch))
import Control.Unification qualified as U
import Control.Unification qualified as Unif
import Control.Unification.IntVar (IntBindingT, IntVar, runIntBindingT)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Text qualified as Text
import LambdaBuffers.Compiler.MiniLog qualified as ML

-- | Only ground terms and unification-fd provides the UVar.
data Term' fun atom a
  = Struct' fun [a]
  | Atom' atom
  deriving stock (Eq, Ord, Show, Foldable, Functor, Traversable)

-- | What does it mean for two Term' to match.
instance (Eq fun, Eq atom) => Unifiable (Term' fun atom) where
  zipMatch :: Term' fun atom a -> Term' fun atom a -> Maybe (Term' fun atom (Either a (a, a)))
  zipMatch (Struct' fl argsl) (Struct' fr argsr)
    | fl /= fr || (length argsl /= length argsr) = Nothing
    | otherwise =
        Just $ Struct' fl $ zipWith (curry Right) argsl argsr
  zipMatch (Atom' l) (Atom' r) | l == r = Just $ Atom' l
  zipMatch _ _ = Nothing

data UError fun atom
  = OccursFailure IntVar (UTerm fun atom)
  | MismatchFailure (Term' fun atom (UTerm fun atom)) (Term' fun atom (UTerm fun atom))
  | MLError (ML.MiniLogError fun atom)
  | Internal String
  deriving stock (Show)

-- | `Unification.Fallible` wiring.
instance Fallible (Term' fun atom) IntVar (UError fun atom) where
  occursFailure = OccursFailure
  mismatchFailure = MismatchFailure

type UTerm fun atom = U.UTerm (Term' fun atom) IntVar
type Scope fun atom = Map ML.VarName IntVar

-- | A clause context consists of available clauses (knowledge base) and a trace of all the called goals that led up to it.
data ClauseContext fun atom = MkClauseCtx
  { cctxTrace :: [UTerm fun atom]
  , cctxClauses :: [ML.Clause fun atom]
  }
  deriving stock (Show)

type UniM fun atom a =
  ReaderT
    (ClauseContext fun atom)
    ( ExceptT
        (UError fun atom)
        ( IntBindingT
            (Term' fun atom)
            (Writer [ML.MiniLogTrace fun atom])
        )
    )
    a

runUniM ::
  [ML.Clause fun atom] ->
  UniM fun atom a ->
  (Either (UError fun atom) a, [ML.MiniLogTrace fun atom])
runUniM clauses p =
  let (errOrRes, logs) = runWriter . runIntBindingT . runExceptT . (`runReaderT` MkClauseCtx mempty clauses) $ p
   in (fst errOrRes, logs)

-- | Implements `ML.MiniLogSolver`.
solve :: (Show fun, Show atom) => ML.MiniLogSolver fun atom
solve clauses goals = case runUniM clauses (top goals) of
  (Left err, logs) -> case err of
    MLError mlErr -> (Left mlErr, logs)
    other -> (Left $ ML.InternalError . Text.pack . show $ other, logs)
  (Right res, logs) -> (Right res, logs)

{- | Top clause essentially analogous to `?-` in Prolog.
 Behaves as a special `callClause` that keeps the `Scope` that the `solve` can return to users for inspecting results.
-}
top :: (Eq fun, Eq atom, Show fun, Show atom) => [ML.Term fun atom] -> UniM fun atom (Map ML.VarName (ML.Term fun atom))
top goals = do
  (goals', scope) <- interpretTerms mempty goals
  _ <- solveGoal `traverse` goals'
  (fromUTerm . U.UVar) `traverse` scope

-- | Solving a goal means looking up a matching clause and `callClause` on it with the given goal as the argument.
solveGoal :: (Eq fun, Eq atom, Show fun, Show atom) => UTerm fun atom -> UniM fun atom (UTerm fun atom)
solveGoal goal' = do
  -- TODO(bladyjoker): Reach out to the author for this issue.
  -- WARN(bladyjoker): Needed to resolve from UVar otherwise we try to do a lookup with a variable that everything unifies with.
  goal <- force goal'
  mlGoal <- fromUTerm goal
  trace $ ML.SolveGoal mlGoal
  clause <- lookupClause goal
  checkCycle goal
  g <- duplicateTerm goal
  _ <- local (\r -> r {cctxTrace = g : cctxTrace r}) (callClause clause goal)
  trace $ ML.DoneGoal mlGoal
  return goal

{- | In functional speak, this is like a function call (application), where clause is a function and a goal is the argument.
 We simply `interpretClause` and unify the given argument with the head of the clause.
 After that we proceed to call all sub-goals in the body of the clause.
-}
callClause :: (Eq fun, Eq atom, Show fun, Show atom) => ML.Clause fun atom -> UTerm fun atom -> UniM fun atom (UTerm fun atom)
callClause clause arg = do
  mlArg <- fromUTerm arg
  trace $ ML.CallClause clause mlArg
  (clauseHead', clauseBody') <- interpretClause clause
  clauseHead' `unify` arg
  _ <- solveGoal `traverse` clauseBody'
  trace $ ML.DoneClause clause mlArg
  return clauseHead'

-- | Checks if the supplied goal was already visited.
checkCycle :: (Eq fun, Eq atom, Show fun, Show atom) => UTerm fun atom -> UniM fun atom ()
checkCycle goal = do
  visitedGoals <- asks cctxTrace
  foldM_
    ( \mayCycle visited -> do
        visited' <- duplicateTerm visited
        goal' <- duplicateTerm goal
        catchError
          ( do
              goal' `unify` visited'
              fromUTerm goal >>= throwError . MLError . ML.CycledGoalsError . (: mayCycle)
          )
          ( \case
              MismatchFailure _ _ -> do
                checked <- fromUTerm visited
                return (checked : mayCycle)
              err -> throwError err
          )
    )
    mempty
    visitedGoals

{- | Given a unifiable term and the knowledge base (clauses) find a next `MiniLog.Clause` to `callClause` on.
 This is a delicate operation, the search simply tries to unify the heads of `MiniLog.Clause`s with the given goal.
 However, before unifying with the goal, `duplicateTerm` is used to make sure the original goal variables are not affected (unified on) by the search.
-}
lookupClause :: (Eq fun, Eq atom, Show fun, Show atom) => UTerm fun atom -> UniM fun atom (ML.Clause fun atom)
lookupClause goal = do
  -- WARN(bladyjoker): Goal has to be `force`d.
  mlGoal <- fromUTerm goal
  trace $ ML.LookupClause mlGoal
  clauses <- asks cctxClauses
  matched <-
    filterM
      ( \cl -> do
          clauseHead' <- toUTerm $ ML.clauseHead cl
          goal' <- duplicateTerm goal
          catchError
            (goal' `unify` clauseHead' >> return True)
            ( \case
                MismatchFailure _ _ -> return False
                err -> throwError err
            )
      )
      clauses
  case matched of
    [] -> throwError . MLError . ML.MissingClauseError $ mlGoal
    [clause] -> trace (ML.FoundClause mlGoal clause) >> return clause
    overlaps -> throwError . MLError . ML.OverlappingClausesError overlaps $ mlGoal

{- | Duplicate a unifiable term (basically copies the structure and instantiates new variables).
 See https://www.swi-prolog.org/pldoc/doc_for?object=duplicate_term/2.
-}
duplicateTerm :: (Eq fun, Eq atom) => UTerm fun atom -> UniM fun atom (UTerm fun atom)
duplicateTerm (U.UVar _) = freeVar
duplicateTerm at@(U.UTerm (Atom' _)) = return at
duplicateTerm (U.UTerm (Struct' f args)) = U.UTerm . Struct' f <$> (duplicateTerm `traverse` args)

{- | Turn a unifiable term back into it's original MiniLog.Term.
 The term needs to be `force`d otherwise you'd get back a variable.
-}
fromUTerm' :: (Eq fun, Eq atom, Show fun, Show atom) => UTerm fun atom -> UniM fun atom (ML.Term fun atom)
fromUTerm' (U.UVar v) = return $ ML.Var $ Text.pack $ show (U.getVarID v)
fromUTerm' (U.UTerm (Atom' at)) = return $ ML.Atom at
fromUTerm' (U.UTerm (Struct' f args)) = ML.Struct f <$> (fromUTerm' `traverse` args)

fromUTerm :: (Eq fun, Eq atom, Show fun, Show atom) => UTerm fun atom -> UniM fun atom (ML.Term fun atom)
fromUTerm t = force t >>= fromUTerm'

-- | Turn a `MiniLog.Term` into a unifiable term.
toUTerm :: (Eq fun, Eq atom) => ML.Term fun atom -> UniM fun atom (UTerm fun atom)
toUTerm (ML.Var _) = freeVar
toUTerm (ML.Atom at) = return . U.UTerm . Atom' $ at
toUTerm (ML.Struct f args) = U.UTerm . Struct' f <$> (toUTerm `traverse` args)

{- | Interpretation of first order syntax encoding of `MiniLog.Clause` into an executable (HOAS) form using `unification-fd` machinery.
 This is where the magic happens as each `MiniLog.Var` gets associated with a `Unification.Uvar`, and thus all `Unification.unify`
 operations are propagated by the underlying machinery.
-}
interpretClause :: (Eq fun, Eq atom) => ML.Clause fun atom -> UniM fun atom (UTerm fun atom, [UTerm fun atom])
interpretClause (ML.MkClause headT body) = do
  (body', scope) <- interpretTerms mempty body
  (headT', _scope') <- interpretTerm scope headT
  return (headT', body')

-- | TODO(bladyjoker): This is ugly.
interpretTerms :: (Eq fun, Eq atom) => Scope fun atom -> [ML.Term fun atom] -> UniM fun atom ([UTerm fun atom], Scope fun atom)
interpretTerms scope terms = do
  (ts', vs') <-
    foldM
      ( \(terms', scope') t -> do
          (t', scope'') <- interpretTerm scope' t
          return (t' : terms', scope'')
      )
      ([], scope)
      terms
  return (reverse ts', vs')

{- | Interpret a `MiniLog.Term` such that for each `MiniLog.Var` a new `Unification.UVar` is created and added to the `Scope`
 or reused from the `Scope` if it was already instantiated.
 The collected `Scope` is used in `top` to provide
-}
interpretTerm :: (Eq fun, Eq atom) => Scope fun atom -> ML.Term fun atom -> UniM fun atom (UTerm fun atom, Scope fun atom)
interpretTerm scope (ML.Var vn) = case Map.lookup vn scope of
  Nothing -> do
    v <- freeVar'
    return (U.UVar v, Map.insert vn v scope)
  Just v -> return (U.UVar v, scope)
interpretTerm scope (ML.Struct f args) = do
  (args', scope') <- interpretTerms scope args
  return (U.UTerm $ Struct' f args', scope')
interpretTerm scope (ML.Atom at) = return (U.UTerm $ Atom' at, scope)

-- | Conventiently wrapped `UMonad` actions.
debug :: Show a => a -> UniM fun atom ()
debug = trace . ML.InternalTrace . show

trace :: ML.MiniLogTrace fun atom -> UniM fun atom ()
trace x = lift . lift . lift $ tell [x]

force :: (Eq fun, Eq atom) => UTerm fun atom -> UniM fun atom (UTerm fun atom)
force = lift . U.applyBindings

unify :: (Eq fun, Eq atom, Show atom, Show fun) => UTerm fun atom -> UTerm fun atom -> UniM fun atom ()
unify l r = do
  debug ("unify" :: String, l, r)
  void $ lift $ Unif.unify l r

freeVar :: (Eq fun, Eq atom) => UniM fun atom (UTerm fun atom)
freeVar = U.UVar <$> freeVar'

freeVar' :: (Eq fun, Eq atom) => UniM fun atom IntVar
freeVar' = do
  v <- lift . lift $ Unif.freeVar
  debug ("new var" :: String, v)
  return v
