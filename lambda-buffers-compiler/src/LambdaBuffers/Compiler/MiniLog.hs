{- | MiniLog is a simple first order syntax encoding of a Prolog-like logic language without non-determinism and backtracking abilities.
 It is used to represent LambdaBuffers Type Class rules (InstanceClause and Derive) and to check for their logical consistency.
-}
module LambdaBuffers.Compiler.MiniLog (VarName, Term (..), Clause (..), MiniLogError (..), struct, (@), (@<=), MiniLogTrace (..), MiniLogSolver, showClauses) where

import Data.Map (Map)
import Data.Text (Text)
import Prettyprinter (Doc, Pretty (pretty), align, comma, dot, encloseSep, line, lparen, rparen, space, squote, vsep, (<+>))

-- | Variable name is just `Text`.
type VarName = Text

-- | A MiniLog `Term` can either be a variable, an atom or a compound term.
data Term f a
  = Var VarName
  | Struct f [Term f a]
  | Atom a
  deriving stock (Eq, Ord, Foldable, Functor, Traversable)

-- | A MiniLog `Clause` is like a Prolog (Horn) clause and it has a head `Term` and a conjunction of `Term`s in the body.
data Clause f a = MkClause
  { clauseHead :: Term f a
  , clauseBody :: [Term f a]
  }
  deriving stock (Eq, Ord, Foldable, Functor, Traversable)

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
    OverlappingClausesError [Clause f a]
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

-- | Printing to Prolog terms.
termToProlog :: (Show f, Show a) => Term f a -> Doc a
termToProlog (Atom at) = squote <> pretty (toPrologAtom at) <> squote
termToProlog (Struct f []) = squote <> pretty (toPrologAtom f) <> squote
termToProlog (Struct f args) = squote <> pretty (toPrologAtom f) <> squote <> align (lparen <> encloseSep mempty rparen comma (termToProlog <$> args))
termToProlog (Var vn) = "V" <> pretty vn

toPrologAtom :: Show a => a -> String
toPrologAtom = filter (/= '"') . show

clauseToProlog :: (Show f, Show a) => Clause f a -> Doc a
clauseToProlog (MkClause headt []) = termToProlog headt <> dot
clauseToProlog (MkClause headt bodyts) = termToProlog headt <+> ":-" <> line <> space <> space <> align (encloseSep mempty mempty comma (termToProlog <$> bodyts) <> dot)

instance (Show f, Show a) => Show (Clause f a) where
  show = show . clauseToProlog

instance (Show f, Show a) => Show (Term f a) where
  show = show . termToProlog

showClauses :: (Show f, Show a) => [Clause f a] -> String
showClauses = show . vsep . fmap clauseToProlog
