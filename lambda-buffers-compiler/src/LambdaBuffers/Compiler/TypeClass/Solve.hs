{-# LANGUAGE LambdaCase #-}

module LambdaBuffers.Compiler.TypeClass.Solve (solveM, solve, Overlap (..)) where

import LambdaBuffers.Compiler.TypeClass.Pat (
  Pat (AppP, DecP, ProdP, RecP, RefP, SumP, VarP, (:*), (:=)),
  matches,
 )
import LambdaBuffers.Compiler.TypeClass.Rules (
  Class (csupers),
  Constraint (C),
  Rule ((:<=)),
  mapPat,
  ruleHeadClass,
  ruleHeadPat,
 )

import Control.Monad.Except (throwError)
import Control.Monad.Reader (ReaderT, runReaderT)
import Control.Monad.Reader.Class (MonadReader (ask))
import Control.Monad.Writer.Class (MonadWriter (tell))
import Control.Monad.Writer.Strict (WriterT, execWriterT)
import Data.Foldable (traverse_)
import Data.List (foldl')
import Data.Set qualified as S
import Data.Text (Text)

{- Pattern/Template/Unification variable  substitution.
   Given a string that represents a variable name,
   and a type to instantiate variables with that name to,
   performs the instantiation
-}
subV :: Text -> Pat -> Pat -> Pat
subV varNm t = \case
  var@(VarP v) -> if v == varNm then t else var
  x :* xs -> subV varNm t x :* subV varNm t xs
  l := x -> subV varNm t l := subV varNm t x
  ProdP xs -> ProdP (subV varNm t xs)
  RecP xs -> RecP (subV varNm t xs)
  SumP xs -> SumP (subV varNm t xs)
  AppP t1 t2 -> AppP (subV varNm t t1) (subV varNm t t2)
  RefP n x -> RefP (subV varNm t n) (subV varNm t x)
  DecP a b c -> DecP (subV varNm t a) (subV varNm t b) (subV varNm t c)
  other -> other

{- Performs substitution on an entire instance (the first argument) given the
   concrete types from a Pat (the second argument).
   Note that ONLY PatVars which occur in the Instance *HEAD* are replaced, though they
   are replaced in the instance superclasses as well (if they occur there).
-}
subst :: Rule -> Pat -> Rule
subst cst@(C _ t :<= _) ty = mapPat (go (getSubs t ty)) cst
  where
    go :: [(Text, Pat)] -> Pat -> Pat
    go subs tty =
      let noflip p1 p2 = uncurry subV p2 p1
       in foldl' noflip tty subs

{- Given two patterns (which are hopefully structurally similar), gather a list of all substitutions
   from the PatVars in the first argument to the concrete types (hopefully!) in the second argument
-}
getSubs :: Pat -> Pat -> [(Text, Pat)] -- should be a set, whatever
getSubs (VarP s) t = [(s, t)]
getSubs (x :* xs) (x' :* xs') = getSubs x x' <> getSubs xs xs'
getSubs (l := t) (l' := t') = getSubs l l' <> getSubs t t'
getSubs (ProdP xs) (ProdP xs') = getSubs xs xs'
getSubs (RecP xs) (RecP xs') = getSubs xs xs'
getSubs (SumP xs) (SumP xs') = getSubs xs xs'
getSubs (AppP t1 t2) (AppP t1' t2') = getSubs t1 t1' <> getSubs t2 t2'
getSubs (RefP n t) (RefP n' t') = getSubs n n' <> getSubs t t'
getSubs (DecP a b c) (DecP a' b' c') = getSubs a a' <> getSubs b b' <> getSubs c c'
getSubs _ _ = []

-- NoMatch isn't fatal but OverlappingMatches is (i.e. we need to stop when we encounter it)
data MatchError
  = NoMatch
  | OverlappingMatches [Rule]

-- for SolveM, since we catch NoMatch
data Overlap = Overlap Constraint [Rule]
  deriving stock (Show, Eq)

selectMatchingInstance :: Pat -> Class -> [Rule] -> Either MatchError Rule
selectMatchingInstance p c rs = case filter matchPatAndClass rs of
  [] -> Left NoMatch
  [r] -> Right r
  overlaps -> Left $ OverlappingMatches overlaps
  where
    matchPatAndClass :: Rule -> Bool
    matchPatAndClass r =
      ruleHeadClass r == c
        && ruleHeadPat r
        `matches` p

type SolveM = ReaderT [Rule] (WriterT (S.Set Constraint) (Either Overlap))

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
solveM :: Constraint -> SolveM ()
solveM cst@(C c pat) =
  ask >>= \inScope ->
    -- First, we look for the most specific instance...
    case selectMatchingInstance pat c inScope of
      Left e -> case e of
        NoMatch -> tell $ S.singleton cst
        OverlappingMatches olps -> throwError $ Overlap cst olps
      -- If there is, we substitute the argument of the constraint to be solved into the matching rules
      Right rule -> case subst rule pat of
        -- If there are no additional constraints on the rule, we try to solve the superclasses
        C _ p :<= [] -> solveClassesFor p (csupers c)
        -- If there are additional constraints on the rule, we try to solve them
        C _ _ :<= is -> do
          traverse_ solveM is
          solveClassesFor pat (csupers c)
  where
    -- NOTE(@bladyjoker): The version w/ flip is more performant...
    -- Given a Pat and a list of Classes, attempt to solve the constraints
    -- constructed from the Pat and each Class
    solveClassesFor :: Pat -> [Class] -> SolveM ()
    solveClassesFor p = traverse_ (\cls -> solveM (C cls p))

solve :: [Rule] -> Constraint -> Either Overlap [Constraint]
solve rules c = fmap S.toList $ execWriterT $ runReaderT (solveM c) rules
