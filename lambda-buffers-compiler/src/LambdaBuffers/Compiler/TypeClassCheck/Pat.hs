{-# LANGUAGE LambdaCase #-}

module LambdaBuffers.Compiler.TypeClassCheck.Pat (
  Pat (..),
  Exp (..),
  Literal (..),
  ExpressionLike (..),
  toProdP,
  toSumP,
  toRecP,
  patList,
  toProdE,
  toSumE,
  toRecE,
  getLocalRefE,
  expList,
  matches,
) where

import Data.Kind (Type)
import Data.Text (Text)

{- A simple ADT to represent patterns.

Note that this ADT allows us to represent nonsensical types (i.e. we can "put the wrong pattern in a hole").
This could be ameliorated by using a GADT, which would give us correct-by-construction patterns at the
cost of significantly more complex type signatures.
-}

data Literal
  = Name Text
  | ModuleName [Text]
  | Opaque
  | TyVar Text
  deriving stock (Show, Eq, Ord)

data Pat
  = LitP Literal
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
    NilP -- Nil and :*  are hacks to write rules for ProdP and SumP. A bare Nil == Unit
  | ConsP Pat Pat -- cons
  | LabelP Pat Pat {- field labels or constr names. The LHS should be (Name "Foo")
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

{- Utility functions. Turn a list of types into a product/record/sum type.
-}
toProdP :: [Pat] -> Pat
toProdP = ProdP . foldr ConsP NilP

toRecP :: [Pat] -> Pat
toRecP = RecP . foldr ConsP NilP

toSumP :: [Pat] -> Pat
toSumP = SumP . foldr ConsP NilP

{- Converts a pattern that consists of a well formed pattern list
   (i.e. patterns formed from :* and Nil) into a list of patterns.
-}
patList :: Pat -> Maybe [Pat]
patList = \case
  NilP -> Just []
  p1 `ConsP` p2 -> (p1 :) <$> patList p2
  _ -> Nothing

data Exp
  = LitE Literal
  | NilE
  | ConsE Exp Exp
  | LabelE Exp Exp
  | RecE Exp
  | ProdE Exp
  | SumE Exp
  | -- NO EXPRESSION VARS! EXPRESSIONS DON'T HAVE HOLES!
    RefE Exp Exp {- 1st arg should be a ModuleName  -}
  | AppE Exp Exp {- Pattern for Type applications -}
  | DecE Exp Exp Exp
  deriving stock (Show, Eq, Ord)

class ExpressionLike (p :: Type) where
  (*:) :: p -> p -> p

  nil :: p

  (*=) :: p -> p -> p

infixr 5 *:

instance ExpressionLike Pat where
  p1 *: p2 = p1 `ConsP` p2

  nil = NilP

  p1 *= p2 = LabelP p1 p2

instance ExpressionLike Exp where
  p1 *: p2 = p1 `ConsE` p2

  nil = NilE

  p1 *= p2 = LabelE p1 p2

{- Utility functions. Turn a list of types into a product/record/sum type.
-}
toProdE :: [Exp] -> Exp
toProdE = ProdE . foldr ConsE NilE

toRecE :: [Exp] -> Exp
toRecE = RecE . foldr ConsE NilE

toSumE :: [Exp] -> Exp
toSumE = SumE . foldr ConsE NilE

-- | Extract the "type function" from an AppE Expr
tyFunE :: Exp -> Maybe Exp
tyFunE = \case
  AppE e1 _ -> case tyFunE e1 of
    Nothing -> Just e1
    Just e2 -> Just e2
  _ -> Nothing

getLocalRefE :: Exp -> Maybe Text
getLocalRefE = \case
  RefE NilE (LitE (Name t)) -> Just t
  app@(AppE _ _) -> case tyFunE app of
    Just (RefE NilE (LitE (Name t))) -> Just t
    _ -> Nothing
  _ -> Nothing

{- Converts a pattern that consists of a well formed pattern list
   (i.e. patterns formed from :* and Nil) into a list of patterns.
-}
expList :: Exp -> Maybe [Exp]
expList = \case
  NilE -> Just []
  p1 `ConsE` p2 -> (p1 :) <$> expList p2
  _ -> Nothing

matches :: Pat -> Exp -> Bool
matches (LitP l1) (LitE l2) = l1 == l2
matches (VarP _) _ = True
matches (x `ConsP` xs) (x' `ConsE` xs') = matches x x' && matches xs xs'
matches (LabelP l t) (LabelE l' t') = matches l l' && matches t t'
matches (ProdP xs) (ProdE xs') = matches xs xs'
matches (RecP xs) (RecE xs') = matches xs xs'
matches (SumP xs) (SumE xs') = matches xs xs'
matches (AppP t1 t2) (AppE t1' t2') = matches t1 t1' && matches t2 t2'
matches (RefP mn t1) (RefE mn' t2) = matches mn mn' && matches t1 t2
matches (DecP t1 t2 t3) (DecE t1' t2' t3') =
  matches t1 t1' && matches t2 t2' && matches t3 t3'
matches NilP NilE = True
matches _ _ = False
