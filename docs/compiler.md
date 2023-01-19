# LambdaBuffers _Compiler_ Spec & Design

The _Compiler_ component sits between the _Frontend_ component and the code
generation component named _Codegen_. The purpose of the _Compiler_ is to
perform various checks on the [Compiler
input](../lambda-buffers-proto/compiler-proto.md#lambdabuffers-compiler-CompilerInput)
provided by the _Frontend_ ensuring that supplied type, class and instance
clause definitions are valid or otherwise communicate any error conditions
determined during processing.

The end goal of the _Compiler_
is to ensure that the _Codegen_ component is capable of processing the _Compiler
Output_ by providing correct and complete information.

## _Compiler_ interface

The _Compiler_ operates on the _Compiler Input_ message specified in the
[compiler.proto](../lambda-buffers-proto/compiler.proto) [Google Protocol
Buffers](https://protobuf.dev/) schema - enabling, any _Frontend_ to interface
with the _Compiler_ in a language agnostic manner.

Similarly, the _Compiler Output_ message is also specified in the
[compiler.proto](../lambda-buffers-proto/compiler.proto) [Google Protocol
Buffers](https://protobuf.dev/) schema that is then consumed by _Codegen_
modules, able to be written in any programming environment capable of
communicating via [Google Protocol Buffers](https://protobuf.dev/).

Refer to the [Compiler Proto
documentation](../lambda-buffers-proto/compiler-proto.md) for more information.

## Checking type definitions (kind checking)

The first step the Compiler performs is _kind checking and inference_ on [type
definitions](../lambda-buffers-proto/compiler-proto.md#lambdabuffers-compiler-TyDef)
provided by the _Frontend_ and otherwise raises [kind checking errors](missing-link).

When successful, the Compiler outputs a [CompilerOutput](missing-link) that
annotates each [type
term](../lambda-buffers-proto/compiler-proto.md#lambdabuffers-compiler-Ty) with
[kind](../lambda-buffers-proto/compiler-proto.md#lambdabuffers-compiler-Kind)
information.

In standard _type checking_ terminology LambdaBuffers terms are [abstract data
type](https://en.wikipedia.org/wiki/Abstract_data_type) declarations and their
types are kinds.

Currently, the _Compiler_ accepts:

 1. type terms of kind `Type` (such as `Int` or `Bool`),

 2. type function terms of kind `Type → Type` (such as `Maybe` or `Either` - though note that type functions are not "first class" in the sense that they cannot be passed as arguments to other type functions).

There are future plans to expand this to Higher Kinded Types (such as `MaybeT`,
`StateT` etc - i.e. types parameterized on type function terms) - subject to research into _Codegen_ of such types in the target languages.

_Compiler_ does support recursive types.

All LambdaBuffers [type
variables](../lambda-buffers-proto/compiler-proto.md#lambdabuffers-compiler-TyArg)
terms must be monomorphically kinded, with polymorphic kinds defaulting to
monomorphic ones. For example `Phantom a = Phantom` would resolve to the
monomorphic kind `Type → Type` rather than the polymorphic kind `∀a. a → Type`.

## Checking type cardinality

In addition to _kind checking_, the Compiler could perform a special check for
types to determine their cardinality. This is especially useful to catch and
report on _non inhabited_ types that users might define.

For example, `data F a = F (F a)` declares a _non-inhabited recurisve type_ that
can't be constructed. LambdaBuffers Compiler SHOULD reject such types as they
can't possibly be constructed and generated typeclass instances would in turn be ill-defined.

This problem is equivalent to a problem of [calculating graph
components](https://en.wikipedia.org/wiki/Component_(graph_theory)).

## Normalising type definitions

Finally, the compiler should be able to _normalise_ expressions. For example, it
may be possible to define a data type in the schema language in a form similar
to: `data G a = G ((Either) ((Maybe) a) Int)`, where the bracketing indicates
the order of application within the term. The example term would normalise to
`data G a = G (Either (Maybe a) Int)` - resulting in a cleaner (and more
performant) code generation.

## Checking typeclass definitions and instance clauses

The _Compiler_ should, if possible, ensure that all instance declarations for
schemata are derivable using hard-coded derivation axioms.

Other schema languages support generating type definitions in many languages
from a single definition in the schema language. One key feature that sets
LambdaBuffers apart from these alternatives is support for
[_typeclasses_](https://en.wikipedia.org/wiki/Type_class), which enable the
generation of [ad-hoc polymorphic
functions](https://en.wikipedia.org/wiki/Ad_hoc_polymorphism) that operate on
types generated from LambdaBuffers schemata.

The LambdaBuffers schema language doesn't allow users to specify typeclass instance
implementations themselves. Users, instead, will write _instance clauses_ as
part of the schema definition, and the LambdaBuffers code generator will derive
these declared instances when generating code.

Two important consequences of this design decision are:

1) _All instances must be derived structurally_. As an example, consider the
arbitrary product type `data P a b = P a b`. The semantics of the generated
instance (i.e. the behavior of the generated code) must be determinable from the
_structure of the type_ - that it is a product - and the instances for its
arguments `a` and `b`, and by those features alone. (Since `a` and `b` are type
variables here, writing a direct instance for any interesting class is likely
impossible, so LambdaBuffers supports constrained instances such as `instance (C
a, C b) => C (P a b)`)

2) _All instances must be uniform across supported languages_. Because the
LambdaBuffers codegen component (and _not_ the user) is responsible for
generating instances, we must ensure that the codegen component is suitably
equipped to generate instances in each language that exhibit behavior which is,
to the greatest extent possible, equivalent to the behavior of generated
instances in any other language. We _must_ have an extensive test quite to
verify uniform behavior of generated instances.

In languages with a typeclass system (Haskell, PureScript) or equivalent (Rust's
Traits), we will utilize the existing system and _should_ (to the extent that
doing so is feasible) utilize existing typeclasses and instances from commonly
used or standard libraries. In languages lacking a type system that is
sufficiently rich to express typeclass relations, we will generate instances
using idiomatic language features. (The details will depend on the particular
language.)

## Unsolved Problems

- [ ] How do we represent recursive types in our lambda calculus AST?

- [ ] How would cardinality checking be integrated within our current checking
      strategy?
