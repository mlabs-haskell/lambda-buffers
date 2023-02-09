{-# LANGUAGE LambdaCase #-}

module LambdaBuffers.Compiler.TypeClass.Solve (solve) where

import Data.List (foldl', sortBy)
import Data.Text (Text)
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

import Data.Set qualified as S

{- Variable substitution. Given a string that represents a variable name,
   and a type to instantiate variables with that name to, performs the
   instantiation
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

-- should be vastly more efficient than Data.List.Nub
deduplicate :: Ord a => [a] -> [a]
deduplicate = S.toList . S.fromList

-- is the first pattern a substitution instance of the second
isSubstitutionOf :: Pat -> Pat -> Bool
isSubstitutionOf p1 p2 = matches p2 p1

compareSpecificity :: Pat -> Pat -> Ordering
compareSpecificity p1 p2
  | p1
      `isSubstitutionOf` p2
      && p2
      `isSubstitutionOf` p1 =
      EQ
  | p1 `isSubstitutionOf` p2 = LT
  | otherwise = GT

sortOnSpecificity :: Pat -> Class -> [Rule] -> [Rule]
sortOnSpecificity p c ps =
  sortBy (\a1 a2 -> compareSpecificity (ruleHeadPat a1) (ruleHeadPat a2)) $
    filter matchPatAndClass ps
  where
    matchPatAndClass :: Rule -> Bool
    matchPatAndClass r =
      ruleHeadClass r == c
        && ruleHeadPat r
        `matches` p

mostSpecificInstance :: Pat -> Class -> [Rule] -> Maybe Rule
mostSpecificInstance p c ps = case sortOnSpecificity p c ps of
  [] -> Nothing
  (x : _) -> Just x

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
solve ::
  [Rule] -> -- all instance rules in scope. WE ASSUME THESE HAVE ALREADY BEEN GENERATED, SOMEWHERE
  Constraint -> -- constraint we're trying to solve
  [Constraint] -- subgoals that cannot be solved for w/ the current rule set
solve inScope cst@(C c pat) =
  -- First, we look for the most specific instance...
  case mostSpecificInstance pat c inScope of
    -- If there isn't one, we return only the constraint we were trying to solve
    Nothing -> [cst]
    -- If there is, we substitute the argument of the constraint to be solved into the matching rules
    Just rule -> case subst rule pat of
      -- If there are no additional constraints on the rule, we try to solve the superclasses
      C _ p :<= [] -> case csupers c of
        [] -> []
        xs -> solveClassesFor p xs
      -- If there are additional constraints on the rule, we try to solve them
      C _ _ :<= is -> case concatMap (solve inScope) is of
        -- If we succeed at solving the additional constraints on the rule, we try to solve the supers
        [] -> solveClassesFor pat (csupers c)
        -- We deduplicate the list of unsolvable subgoals
        xs -> deduplicate xs
  where
    -- NOTE(@bladyjoker): The version w/ flip is more performant...
    -- Given a Pat and a list of Classes, attempt to solve the constraints
    -- constructed from the Pat and each Class
    solveClassesFor :: Pat -> [Class] -> [Constraint]
    solveClassesFor p =
      deduplicate -- multiple constraints could emit the same subgoal which is bad
        . concatMap
          ( solve inScope
              . ( \cls ->
                    let classConstraint = C cls p
                     in classConstraint
                )
          )
