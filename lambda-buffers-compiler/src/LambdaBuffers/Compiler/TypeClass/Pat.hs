{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

module LambdaBuffers.Compiler.TypeClass.Pat where

import Data.Bifunctor
import Data.List.NonEmpty qualified as NE
import Data.Text (Text)

{- A simple ADT to represent patterns

We can also use this to represent patterns by using PatVars in compound types.

Note that this ADT allows us to represent nonsensical types. I wanted to prove to Drazen that I could
do something without using a billion language extensions, and some nonsense is the price we pay for that :p
-}

data Pat
  = {- extremely stupid, unfortunately necessary -}
    Name Text
  | Opaque
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
pattern Int :: Pat
pattern Int = RefP (Name "Int")

pattern String :: Pat
pattern String = RefP (Name "String")

pattern Bool :: Pat
pattern Bool = RefP (Name "Bool")

pattern List :: Pat -> Pat
pattern List t = AppP (RefP (Name "List")) t

pattern Maybe :: Pat -> Pat
pattern Maybe t = AppP (RefP (Name "Maybe")) t

pattern Map :: Pat -> Pat -> Pat
pattern Map k v = AppP (AppP (RefP (Name "Map")) k) v

pattern Either :: Pat -> Pat -> Pat
pattern Either l r = AppP (AppP (RefP (Name "Either")) l) r

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

-- we probably don't need these _ functions
_list :: Pat
_list = RefP (Name "List")

_maybe :: Pat
_maybe = RefP (Name "Maybe")

_map :: Pat
_map = RefP (Name "Map")

_either :: Pat
_either = RefP (Name "Either")

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

-- misc utilities, break out into a Utils module if this grows too much

-- whyyyyyy isn't this in prelude
for :: [a] -> (a -> b) -> [b]
for = flip map

safeHead :: [a] -> Maybe a
safeHead = \case
  [] -> Nothing
  (x : _) -> Just x

_k, _v, _a, _l, _x, _xs, _name, _vars, _nil, _body :: Pat
_k = VarP "kdasfadsfsadf3323232421413413413"
_v = VarP "vdsfasdfa3e4fewafewufioeoifioaefiowe"
_a = VarP "a3242432aefiosdjfioasdf32jior3j2oirj32io"
_l = VarP "lasdfsdfj3ij319031j8381913j8138"
_x = VarP "x32fjio23io32jio3j2iof3ijo2fio32fio32"
_xs = VarP "xsadsfjdlsfji3i3298238893289j32j8923j89"
_name = VarP "namedsafdsfhuisdhfuidshu3893283h8df398hdf321"
_body = VarP "bodyadfjasidofjads08f8943j289234j89234j98fj38"
_vars = VarP "varsdsfdasfjklsdafjilsdafjiasdio43io2903"
_nil = VarP "nildsfjosdjfiosdajfoi89321893y8914981398"
