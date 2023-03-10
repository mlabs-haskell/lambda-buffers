{- | MiniLog is a simple first order syntax encoding of a Prolog-like logic language without non-determinism and backtracking abilities.
 It is used to represent LambdaBuffers Type Class rules (InstanceClause and Derive) and to check for their logical consistency.
-}
module LambdaBuffers.Compiler.MiniLog (VarName, Term (..), Clause (..), MiniLogError (..), struct, (@), (@<=), MiniLogTrace (..), MiniLogSolver) where

import Data.Map (Map)
import Data.Text (Text)

-- | Variable name is just `Text`.
type VarName = Text

-- | A MiniLog `Term` can either be a variable, an atom or a compound term.
data Term f a
  = Var VarName
  | Struct f [Term f a]
  | Atom a
  deriving stock (Eq, Ord, Show, Foldable, Functor, Traversable)

-- | A MiniLog `Clause` is like a Prolog (Horn) clause and it has a head `Term` and a conjunction of `Term`s in the body.
data Clause f a = MkClause
  { clauseHead :: Term f a
  , clauseBody :: [Term f a]
  }
  deriving stock (Eq, Ord, Show, Foldable, Functor, Traversable)

struct :: forall {f} {a}. f -> [Term f a] -> Term f a
struct = Struct

(@) :: forall {f} {a}. Text -> Term f a
(@) = Var

(@<=) :: forall {f} {a}. Term f a -> [Term f a] -> Clause f a
(@<=) = MkClause

{- | An interface definition for MiniLog solvers.
 Given a knowledge base (clauses) and a list of goals to solve, returns either
 the solution (variable to term binding) or a `MiniLogError`. In any case, it
 returns a `MiniLogTrace` that can help in understanding the underlying
 reasoning path.
-}
type MiniLogSolver f a =
  (Eq f, Eq a) =>
  [Clause f a] ->
  [Term f a] ->
  (Either (MiniLogError f a) (Map VarName (Term f a)), [MiniLogTrace f a])

-- | Interface errors that `MiniLogSolver` implementations must provide.
data MiniLogError f a
  = MissingGoalError (Term f a)
  | OverlappingClausesError [Clause f a]
  | InternalError Text -- Can be used by implementations to signal their internal error conditions.
  deriving stock (Eq, Ord, Show)

-- | Interface traces that the `MiniLogSolver` must provide.
data MiniLogTrace fun atom
  = LookupClause (Term fun atom)
  | FoundClause (Term fun atom) (Clause fun atom)
  | SolveGoal (Term fun atom)
  | DoneGoal (Term fun atom)
  | CallClause (Clause fun atom) (Term fun atom)
  | DoneClause (Clause fun atom) (Term fun atom)
  | InternalTrace String -- Can be used by implementations to add their internal tracing.
  deriving stock (Eq, Ord, Show)
