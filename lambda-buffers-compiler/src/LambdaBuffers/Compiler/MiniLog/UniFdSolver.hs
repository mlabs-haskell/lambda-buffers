-- | unification-fd based solver.
module LambdaBuffers.Compiler.MiniLog.UniFdSolver (solve, runUMonad) where

import Control.Monad (filterM, foldM, void)
import Control.Monad.Except (ExceptT, MonadError (catchError, throwError), runExceptT)
import Control.Monad.Identity (Identity (runIdentity))
import Control.Monad.Reader (ReaderT (runReaderT), local)
import Control.Monad.Reader.Class (asks)
import Control.Monad.Trans (MonadTrans (lift))
import Control.Monad.Writer (MonadWriter (tell), WriterT (runWriterT))
import Control.Unification (Fallible, Unifiable (zipMatch))
import Control.Unification qualified as U
import Control.Unification qualified as Unif
import Control.Unification.IntVar (IntBindingT, IntVar, runIntBindingT)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Text qualified as Text
import Debug.Trace qualified as Debug
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
  | MissingGoal (ML.Term fun atom)
  | OverlappingClauses [ML.Clause fun atom]
  | Internal String
  deriving stock (Show)

-- | `Unification.Fallible` wiring.
instance Fallible (Term' fun atom) IntVar (UError fun atom) where
  occursFailure = OccursFailure
  mismatchFailure = MismatchFailure

type UTerm fun atom = U.UTerm (Term' fun atom) IntVar
type Scope fun atom = Map ML.VarName (UTerm fun atom)

-- | A clause context consists of available clauses (knowledge base) and variables in clause closure.
data ClauseContext fun atom = MkMiniLogRead
  { cctxScope :: Scope fun atom
  , cctxClauses :: [ML.Clause fun atom]
  }
  deriving stock (Show)

data MiniLogTrace fun atom
  = LookupClause (UTerm fun atom)
  | FoundClause (ML.Clause fun atom)
  | CallGoal (UTerm fun atom)
  | DoneGoal (UTerm fun atom)
  | Debug String
  deriving stock (Show)

type UMonad fun atom a =
  ReaderT
    (ClauseContext fun atom)
    ( WriterT
        [MiniLogTrace fun atom]
        ( ExceptT
            (UError fun atom)
            (IntBindingT (Term' fun atom) Identity)
        )
    )
    a

runUMonad :: [ML.Clause fun atom] -> UMonad fun atom a -> Either (UError fun atom) (a, [MiniLogTrace fun atom])
runUMonad clauses p =
  let (errOrRes, _) = runIdentity . runIntBindingT . runExceptT . runWriterT . (`runReaderT` MkMiniLogRead mempty clauses) $ p
   in errOrRes

solve :: (Show fun, Show atom) => ML.MiniLogSolver fun atom
solve clauses goals = case runUMonad clauses (top goals) of
  Left err -> case err of
    (OverlappingClauses overlaps) -> Left $ ML.OverlappingClausesError overlaps
    (MissingGoal missing) -> Left $ ML.MissingGoalError missing
    other -> Left $ ML.InternalError . Text.pack . show $ other
  Right res -> Right (fst res)

{- | Top clause essentially analogous to `?-` in Prolog.
 Keeps the scope that the `solve` can return to users for inspecting results.
-}
top :: (Eq fun, Eq atom, Show fun, Show atom) => [ML.Term fun atom] -> UMonad fun atom (Map ML.VarName (ML.Term fun atom))
top goals = do
  (goals', scope) <- interpretTerms mempty goals
  _ <- local (\r -> r {cctxScope = scope}) (callGoal `traverse` goals')
  debug ("top", scope)
  fromUTerm `traverse` scope

-- | Calling a goal means looking up a matching clause and `callClause` on it with the given goal as the argument.
callGoal :: (Eq fun, Eq atom, Show fun, Show atom) => UTerm fun atom -> UMonad fun atom (UTerm fun atom)
callGoal goal' = do
  debug ("call", goal')
  -- TODO(bladyjoker): Reach out to the author for this issue.
  goal <- force goal' -- WARN(bladyjoker): Needed to resolve from UVar.
  clause <- lookupClause goal
  _ <- callClause clause goal
  return goal

{- | In functional speak, this is like a function call (application), where clause is a function and a goal is the argument.
 We simply `interpretClause` and unify the given argument with the head of the clause.
 After that we proceed to call all sub-goals in the body of the clause.
-}
callClause :: (Eq fun, Eq atom, Show fun, Show atom) => ML.Clause fun atom -> UTerm fun atom -> UMonad fun atom (UTerm fun atom)
callClause clause arg = do
  debug ("call clause", arg, clause)
  (clauseHead', clauseBody', clauseScope) <- interpretClause clause
  arg `unify` clauseHead'
  debug ("call clause scope", clauseScope)
  _ <- local (\r -> r {cctxScope = clauseScope}) (callGoal `traverse` clauseBody')
  return clauseHead'

{- | Given a unifiable term and the knowledge base (clauses) find a next `MiniLog.Clause` to `callClause`.
 This is a delicate operation, the search simply tries to unify the heads of `MiniLog.Clause`s with the given goal.
 However, before unifying with the goal, `duplicateTerm` is used to make sure the original goal variables are not affected (unified on) by the search.
-}
lookupClause :: forall {fun} {atom}. (Eq fun, Eq atom, Show fun, Show atom) => UTerm fun atom -> UMonad fun atom (ML.Clause fun atom)
lookupClause goal = do
  debug ("lookup", goal)

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
    [] -> fromUTerm goal >>= throwError . MissingGoal
    [clause] -> tell [FoundClause clause] >> return clause
    overlaps -> throwError $ OverlappingClauses overlaps

{- | Duplicate a unifiable term (basically copies the structure and instantiates new variables).
 See https://www.swi-prolog.org/pldoc/doc_for?object=duplicate_term/2.
-}
duplicateTerm :: (Eq fun, Eq atom) => UTerm fun atom -> UMonad fun atom (UTerm fun atom)
duplicateTerm (U.UVar _) = freeVar
duplicateTerm at@(U.UTerm (Atom' _)) = return at
duplicateTerm (U.UTerm (Struct' f args)) = U.UTerm . Struct' f <$> (duplicateTerm `traverse` args)

{- | Turn a unifiable term back into it's original MiniLog.Term.
 Relies on the `Scope` to be properly built and maintained.
-}
fromUTerm :: (Eq fun, Eq atom) => UTerm fun atom -> UMonad fun atom (ML.Term fun atom)
fromUTerm (U.UVar v) = do
  mayV <- lift . lift . lift $ U.lookupVar v
  case mayV of
    Nothing -> do
      vars <- asks cctxScope
      case filter
        ( \case
            (_, U.UVar v') -> v == v'
            _ -> False
        )
        (Map.toList vars) of
        [] -> throwError . Internal $ "Couldn't find VarName" <> show v
        [(vn, _)] -> return $ ML.Var vn
        _ -> throwError $ Internal "Too m,any varname"
    Just ut -> fromUTerm ut
fromUTerm (U.UTerm (Atom' at)) = return $ ML.Atom at
fromUTerm (U.UTerm (Struct' f args)) = ML.Struct f <$> (fromUTerm `traverse` args)

-- | Turn a `MiniLog.Term` into a unifiable term.
toUTerm :: (Eq fun, Eq atom) => ML.Term fun atom -> UMonad fun atom (UTerm fun atom)
toUTerm (ML.Var _) = freeVar
toUTerm (ML.Atom at) = return . U.UTerm . Atom' $ at
toUTerm (ML.Struct f args) = U.UTerm . Struct' f <$> (toUTerm `traverse` args)

{- | Interpretation of first order syntax encoding of `MiniLog.Clause` into an executable (HOAS) form using `unification-fd` machinery.
 This is where the magic happens as each `MiniLog.Var` gets associated with a `Unification.Uvar`, and thus all `Unification.unify`
 operations are propagated by the underlying machinery.
 The returned `Scope` contains `Unification.UVar`s associated with `MiniLog.VarName` to facilitate reporting using `fromUTerm`.
-}
interpretClause :: (Eq fun, Eq atom, Show fun, Show atom) => ML.Clause fun atom -> UMonad fun atom (UTerm fun atom, [UTerm fun atom], Scope fun atom)
interpretClause (ML.MkClause headT body) = do
  (body', vars) <- interpretTerms mempty body
  debug ("1", vars)
  (headT', vars') <- interpretTerm vars headT
  debug ("1", vars')
  return (headT', body', vars')

-- | TODO(bladyjoker): Use ST this is ugly.
interpretTerms :: (Eq fun, Eq atom) => Scope fun atom -> [ML.Term fun atom] -> UMonad fun atom ([UTerm fun atom], Scope fun atom)
interpretTerms vars terms = do
  (ts', vs') <-
    foldM
      ( \(terms', vars') t -> do
          (t', vars'') <- interpretTerm vars' t
          return (t' : terms', vars'')
      )
      ([], vars)
      terms
  return (reverse ts', vs')

{- | Interpret a `MiniLog.Term` such that for each `MiniLog.Var` a new `Unification.UVar` is created and added to the `Scope`
 or reused from the `Scope` if it was already instantiated.
-}
interpretTerm :: (Eq fun, Eq atom) => Scope fun atom -> ML.Term fun atom -> UMonad fun atom (UTerm fun atom, Scope fun atom)
interpretTerm vars (ML.Var vn) = case Map.lookup vn vars of
  Nothing -> do
    debug (vn, "nope")
    v <- freeVar
    return (v, Map.insert vn v vars)
  Just v -> debug (vn, "yoop") >> return (v, vars)
interpretTerm vars (ML.Struct f args) = do
  (args', vars') <- interpretTerms vars args
  return (U.UTerm $ Struct' f args', vars')
interpretTerm vars (ML.Atom at) = return (U.UTerm $ Atom' at, vars)

-- | Conventiently wrapped `UMonad` actions.
debug :: (Show a, Monad m) => a -> m ()
debug x = Debug.trace (show x) (return ())

force :: (Eq fun, Eq atom) => UTerm fun atom -> UMonad fun atom (UTerm fun atom)
force = lift . lift . U.applyBindings

unify :: (Eq fun, Eq atom) => UTerm fun atom -> UTerm fun atom -> UMonad fun atom ()
unify l r = do
  -- WARN(bladyjoker): Forcing bindings here as well for extra safety.
  l' <- force l
  r' <- force r
  void $ lift . lift $ Unif.unify l' r'

freeVar :: (Eq fun, Eq atom) => UMonad fun atom (UTerm fun atom)
freeVar = lift . lift . lift $ U.UVar <$> Unif.freeVar
