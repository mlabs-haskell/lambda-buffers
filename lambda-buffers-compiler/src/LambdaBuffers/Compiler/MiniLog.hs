{- | MiniLog is a simple first order syntax encoding of a Prolog-like logic language without non-determinism and backtracking abilities.
 It is used to represent LambdaBuffers Type Class rules (`InstanceClause` and `Derive`) and to check for their logical consistency.
-}
module LambdaBuffers.Compiler.MiniLog (
  VarName,
  Term (..),
  Clause (..),
  MiniLogError (..),
  struct,
  (@),
  (@<=),
  MiniLogTrace (..),
  MiniLogSolver,
) where

import Data.Map (Map)
import Data.Text (Text)

{- | Variable name is just `Text`.
 Think of it as a unification variable (an unknown), much like you'd have in Prolog:
 ?- X = 1.
-}
type VarName = Text

{- | A MiniLog `Term` can either be a variable, an atom or a compound term.

  In Prolog a term like `foo(1,X)` is a compound term `Struct` with the 'functor'
 'foo' and arity 2, meaning it has 2 arguments, the first one bound to an `Atom`
 '1' and the second to a `Var` 'X'.
-}
data Term f a
  = Var VarName
  | Struct f [Term f a]
  | Atom a
  deriving stock (Eq, Ord, Show, Foldable, Functor, Traversable)

{- | A MiniLog `Clause` is like a Prolog (Horn) clause and it has a head `Term` and a conjunction of `Term`s in the body.
 In Prolog:

 ```prolog
 animal(X) :- human(X).
 human(socrates).
 ```
 Here we see 2 clauses, the first one has for the `clauseHead` a `Term`
 'animal(X)' and for the body a single `Term` 'human(X)'. Notice that they share
 a `Var` 'X'. The second clause has the `clauseHead` a `Term`
 'human(socrates)' and it's also called a 'fact', which simply means it's
 'true' as it has no `clauseBody` as a condition. 'socrates' is an `Atom` (a
 ground value).
-}
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
  = -- | No `Clause` was found for goal.
    MissingClauseError (Term f a)
  | -- | Multiple overlapping `Clauses` were found.
    OverlappingClausesError [Clause f a] (Term f a)
  | -- | A goal cycle was detected.
    CycledGoalsError [Term f a]
  | -- | Implementation/provider internal error conditions.
    InternalError Text
  deriving stock (Eq, Ord, Show)

-- | Interface traces that the `MiniLogSolver` must provide.
data MiniLogTrace fun atom
  = LookupClause (Term fun atom)
  | FoundClause (Term fun atom) (Clause fun atom)
  | SolveGoal (Term fun atom)
  | DoneGoal (Term fun atom)
  | CallClause (Clause fun atom) (Term fun atom)
  | DoneClause (Clause fun atom) (Term fun atom)
  | -- |Can be used by implementations to add their internal tracing.
    InternalTrace String
  deriving stock (Eq, Ord, Show)
