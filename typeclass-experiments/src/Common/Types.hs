{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}

module Common.Types where

import qualified Common.SourceTy as Ty
import Data.Bifunctor
import Data.List
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty  as NE
import Data.Set (Set)
import Data.Text (Text)

{- A simple ADT to represent types (i.e. the terms of our schema language).

We can also use this to represent patterns by using PatVars in compound types.

Note that this ADT allows us to represent nonsensical types. I wanted to prove to Drazen that I could
do something without using a billion language extensions, and some nonsense is the price we pay for that :p
-}

data Pat
  = {- extremely stupid, unfortunately necessary -}
    Name Text
  | {- arity 0 primitives -}
    IntP
  | StringP
  | BoolP
  | {- arity 1 primitives -}
    ListP
  | MaybeP
  | {- arity 2 primitives -}
    MapP
  | EitherP
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
  | VarP Text {- This isn't a type variable. Although it is used to represent them in certain contexts,
                 it is also used more generally to refer to any "hole" in a pattern to which another pattern
                 may be substituted. We could have separate constr for type variables but it doesn't appear to be
                 necessary at this time. -}
  | RefP Pat {- still unclear on how to handle these here -}
  | AppP Pat Pat {- Pattern for Type applications -}
  | {- This last one is a bit special. This represents a complete type declaration.

       The first Pat should be instantiated to `Name l` where l is a concrete name.

       The second Pat should be instantiated to a Pat-List (using :*/Nil) which only contains Names.

       The final Pat should be instantiated to a Pat body.

       In some languages, parts of this may be ignored. E.g. in Rust the type name doesn't matter (we use the constr name of the
       outermost inner sum for constructing types). -}
    DecP Pat Pat Pat
  deriving (Show, Eq, Ord)

infixr 5 :*

-- Pattern synonyms. These VASTLY improve readability

pattern List :: Pat -> Pat
pattern List t = AppP ListP t

pattern Maybe :: Pat -> Pat
pattern Maybe t = AppP MaybeP t

pattern Map :: Pat -> Pat -> Pat
pattern Map k v = AppP (AppP MapP k) v

pattern Either :: Pat -> Pat -> Pat
pattern Either l r = AppP (AppP EitherP l) r

pattern RecFields :: Pat -> Pat -> Pat -> Pat
pattern RecFields l x xs = RecP ((l := x) :* xs)

pattern ProdArgs :: Pat -> Pat -> Pat
pattern ProdArgs x xs = ProdP (x :* xs)

pattern RecBody :: Pat -> Pat -> Pat
pattern RecBody constr fields = SumP (constr := RecP fields :* Nil)

pattern ProdBody :: Pat -> Pat -> Pat
pattern ProdBody constr fields = SumP ((constr := ProdP fields) :* Nil)

pattern Rec :: Pat -> Pat -> Pat -> Pat -> Pat
pattern Rec tyName tyVars constr fields =
  DecP tyName tyVars (SumP (constr := RecP fields :* Nil))

pattern Product :: Pat -> Pat -> Pat -> Pat -> Pat
pattern Product tyName tyVars constr args =
  DecP tyName tyVars (SumP (constr := ProdP args :* Nil))

pattern SumBody :: Pat -> Pat -> Pat -> Pat
pattern SumBody l x xs = SumP ((l := x) :* xs)

pattern Sum :: Pat -> Pat -> Pat -> Pat
pattern Sum tyName tyVars pat =
  DecP tyName tyVars pat

pattern Void :: Pat
pattern Void = SumP Nil

{- Utility functions. Turn a list of types into a product/record/sum type.
-}
toProd :: [Pat] -> Pat
toProd = ProdP . foldr (:*) Nil

toRec :: [Pat] -> Pat
toRec = RecP . foldr (:*) Nil

toSum :: [Pat] -> Pat
toSum = SumP . foldr (:*) Nil

{- Utility functions. Turn a list of types into a product/record/sum type.
-}
defToPat :: Ty.TyDef -> Pat
defToPat Ty.TyDef {..} = DecP (Name tyDefName) vars $ case tyDefBody of
  Ty.Sum constrs -> toSum . NE.toList . fmap goConstr $ constrs
  where
    collectFreeTyVars (Ty.TKind xs) = foldr (\x acc -> Name x :* acc) Nil xs

    vars = collectFreeTyVars tyDefKind

    goConstr :: (Ty.ConstrName, Ty.Product) -> Pat
    goConstr (n, p) = Name n := goProduct p

    goProduct :: Ty.Product -> Pat
    goProduct = \case
      Ty.Empty -> Nil
      Ty.Record rMap -> toRec . NE.toList . fmap (uncurry (:=) . bimap Name goPat) $ rMap
      Ty.Product pList -> toProd . NE.toList . fmap goPat $ pList

    goPat :: Ty.SourceTy -> Pat
    goPat = \case
      Ty.TyVar t -> VarP t
      Ty.TApp a b -> AppP (goPat a) (goPat b)
      Ty.PrimT prim -> goPrim prim
      Ty.TyRef ref -> case ref of
        Ty.Local t -> RefP (Name t)
        _ -> undefined -- dunno what to do here. Imports should be resolved by this stage?
    goPrim :: Ty.TyPrim -> Pat
    goPrim = \case
      Ty.TP0 p0 -> case p0 of
        Ty.TInt -> IntP
        Ty.TBool -> BoolP
        Ty.TString -> StringP
      Ty.TP1 p1 -> case p1 of
        Ty.TMaybe -> MaybeP
        Ty.TList -> ListP
      Ty.TP2 p2 -> case p2 of
        Ty.TMap -> MapP
        Ty.TEither -> EitherP
