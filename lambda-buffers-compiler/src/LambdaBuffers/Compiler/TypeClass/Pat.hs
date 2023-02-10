{-# LANGUAGE LambdaCase #-}

module LambdaBuffers.Compiler.TypeClass.Pat (
  Pat (..),
  toProd,
  toRec,
  toSum,
  patList,
  matches,
) where

import Data.Text (Text)

{- A simple ADT to represent patterns.

Note that this ADT allows us to represent nonsensical types (i.e. we can "put the wrong pattern in a hole").
This could be ameliorated by using a GADT, which would give us correct-by-construction patterns at the
cost of significantly more complex type signatures.
-}

data Pat
  = {- Name / ModuleName / Opaque / TyVarP are literal patterns (or ground terms)
       because hey cannot contain any VarPs and therefore "have no holes".
       Every TyDef or subcomponent thereof will be translated into a composite
       pattern "without any holes". (Nil is also a literal/ground term, I guess) -}
    Name Text
  | ModuleName [Text]
  | Opaque
  | TyVarP Text
  | {- Lists (constructed from Nil and :*) with bare types are used to
       encode products (where a list of length n encodes an n-tuple)
       Lists with field labels (l := t) are used to encode records and sum types
       These representations let us "peer into the structure" of the TyBody, and are
       somewhat analogous to the Generics.SOP representation or, in the case of records (or sums
       interpreted as variants), to a row-types representation. We can imagine that each record and
       sum are backed by an implicit row.
       Unfortunately this encoding allows us to generate Pats which do not correspond to
       any possible types. For the purposes of instance resolution/code generation this shouldn't matter
       so long as the patterns are only generalizations of "real" types. We could ameliorate this problem by
       using a GADT for Pat, but this would greatly complicate the constraint solving/deriving
       algorithms and require copious use of type families (and possibly singletons).
    -}
    Nil -- Nil and :*  are hacks to write rules for ProdP and SumP. A bare Nil == Unit
  | Pat :* Pat -- cons
  | Pat := Pat {- field labels or constr names. The LHS should be (Name "Foo")
                  for schema types, but should be a PatVar for deriving rules and instances -}
  | RecP Pat {- where the Pat arg is expected to be (l := t :* rest) or Nil, where rest
                is also a pat-list  of labeled fields or Nil -}
  | ProdP Pat {- Pat arg should be a list of "Bare types" -}
  | SumP Pat {- where the Pat arg is expected to be (Constr l t :* rest) or Nil, where
                rest is either Nil or a tyList of Constrs -}
  | VarP Text {- This isn't a type variable. It is used more generally to refer to any "hole" in a pattern into
                 to which another pattern may be substituted. TyVarP is the literal pattern / ground term for TyVars  -}
  | RefP Pat Pat {- 1st arg should be a ModuleName  -}
  | AppP Pat Pat {- Pattern for Type applications -}
  | {- This last one is a bit special. This represents a complete type declaration.
       The first Pat should be instantiated to `Name l` where l is a concrete name.
       The second Pat should be instantiated to a Pat-List (using :*/Nil) which only contains TyVarPs.
       The final Pat should be instantiated to a Pat body.
       In some languages, parts of this may be ignored. E.g. in Rust the type name doesn't matter (we use the constr name of the
       outermost inner sum for constructing types). -}
    DecP Pat Pat Pat
  deriving stock (Show, Eq, Ord)

infixr 5 :*

{- Utility functions. Turn a list of types into a product/record/sum type.
-}
toProd :: [Pat] -> Pat
toProd = ProdP . foldr (:*) Nil

toRec :: [Pat] -> Pat
toRec = RecP . foldr (:*) Nil

toSum :: [Pat] -> Pat
toSum = SumP . foldr (:*) Nil

{- Converts a pattern that consists of a well formed pattern list
   (i.e. patterns formed from :* and Nil) into a list of patterns.
-}
patList :: Pat -> Maybe [Pat]
patList = \case
  Nil -> Just []
  p1 :* p2 -> (p1 :) <$> patList p2
  _ -> Nothing

{- This is used as a predicate to filter instances or Gens which are structurally compatible
   with the argument type.
   The first argument is the inner Pat from an instance head or Gen.
   The second argument is the Pat representation of a type that we want to derive an instance / generate code for.
   NOTE: Is not bidirectional! The first Pat has to be more general than the first
         (more specifically: The second Pat should be a substitution instance of the first)
-}
matches :: Pat -> Pat -> Bool
matches t1 t2 | t1 == t2 = True -- need the guard
matches (VarP _) _ = True
matches (x :* xs) (x' :* xs') = matches x x' && matches xs xs'
matches (l := t) (l' := t') = matches l l' && matches t t'
matches (ProdP xs) (ProdP xs') = matches xs xs'
matches (RecP xs) (RecP xs') = matches xs xs'
matches (SumP xs) (SumP xs') = matches xs xs'
matches (AppP t1 t2) (AppP t1' t2') = matches t1 t1' && matches t2 t2'
matches (RefP mn t1) (RefP mn' t2) = matches mn mn' && matches t1 t2
matches (DecP t1 t2 t3) (DecP t1' t2' t3') =
  matches t1 t1' && matches t2 t2' && matches t3 t3'
matches _ _ = False
