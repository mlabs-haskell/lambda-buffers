# Compiler Spec & Design

The Compiler component sits between the Frontend component and the Codegen
component. The purpose of this component is to typecheck (or, more accurately,
kindcheck) the Frontend's output (_Compiler Input_) and perform other additional
validation checks necessary to ensure that the Codegen component is capable of
processing the _Compiler Output_.

## Compiler interface

The Compiler operates on the Compiler Input proto, effectively enabling any
number of Frontends to interface with the Compiler in a language agnostic
manner.

Similarly, the Compiler Output is also a proto that is then consumed by Codegen
modules that can be written in any programming environment capable of
communicating using Google Protobufs.

## Checking type definitions

One primary purpose of the compiler is to typecheck the schemata that users
define via the Frontend. The lambda calculus representation of schemata is "one
level up" from the term level: ADT schemata declarations are the "terms" and
kinds are the "types".

Because the schema language only supports data declarations for algebraic data
types, and does not support function definitions, the only two admissible kinds
are `Type` and `TyCon`s of a specific arity, such that a `TyCon` can accept an
arbitrary number of arguments of kind `Type` but can **only** accept arguments
of kind `Type`. Put another way: An expression's kind is the same thing as its
arity, and only expressions of arity=0 (i.e. expressions of kind `Type`) can be
applied.

To elaborate on the previous point: A user can define a higher kinded data type,
such as `data Maybe a = Nothing | Just a` in the schema language. However users
cannot define a data type which has a higher kinded type as a _parameter_, and
users must "fully saturate" higher kinded types with well-scoped type variables
or zero arity `Type`s to pass them as arguments to a `TyCon`. Consequently, a
definition such as `data HKT f a = HKT (f a)` is not valid because `f :: Type ->
Type`. Restricting the schema language in this way does reduce its
expressiveness, but allowing higher kinded type parameters would greatly limit
(or, at the very least, greatly complicate) our ability to codegen for a variety
of target languages with type systems less expressive than Haskell.

The schema language supports recursive types, and so the compiler must be
capable of checking recursive types.

In addition to typechecking, the compiler must perform a special check for
recursive types: It must validate that a recursive type is inhabited (or
inhabitable). The purpose of this check is to ensure that any schema which
passes validation is (in principle) a schema for which type definitions and
typeclass "instances" (which may be simple functions in languages without
typeclass support) can be generated. As an example, the compiler should reject
types such as `data F a = F (F a)`, which is uninhabited (and uninhabitable).

(Provisional:) Finally, the compiler should _normalize_ expressions as far as it
is able to. For example, it may be possible to define a data type in the schema
language in a form similar to: `data G a = G ((Either) ((Maybe) a) Int)`, where
the parentheses indicate application. Ideally, this would be normalized to `data
G a = G (Either (Maybe a) Int)` to result in cleaner (and more performant) code
generation.

## Checking type class definitions and instance clauses

The Compiler should, if possible, ensure that all instance declarations for
schemata are derivable using hard-coded derivation axioms. Because the checks
relevant to validating type class instances ough to be entirely separate from
the checks enumerated above, they can be worked on separately at a later date
when the design of the typeclass system has been fleshed out more.

## Unsolved Problems

- How do we represent recursive types in our lambda calculus AST?
