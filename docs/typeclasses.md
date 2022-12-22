# Typeclass Support Spec & Design

## Introduction

Other schema languages support generating type definitions in many languages from a single definition in the schema language. One key feature that sets LambdaBuffers apart from these alternatives is support for _typeclasses_, which enable the generation of ad-hoc polymorphic functions that operate on types generated from LambdaBuffers schemata.

Due to inherent limitations in the design of the LambdaBuffers schema language, users will not be able to define typeclass instances themselves. (Allowing users to write their own methods would greatly complicate the schema language to the point where it is doubtful it could rightly be called a schema language.) Users, instead, will declare typeclass instances as part of the schema definition, and the LambdaBuffers code generator will derive these declared instances when generating code.

Two important consequences of this design decision are:

1) _All instances must be derived structurally_. As an example, consider the arbitrary product type `data P a b = P a b`. The semantics of the generated instance (i.e. the behavior of the generated code) must be determinable from the _structure of the type_ - that it is a product - and the instances for its arguments `a` and `b`, and by those features alone. (Since `a` and `b` are type variables here, writing a direct instance for any interesting class is likely impossible, so LambdaBuffers supports constrained instances such as `instance (C a, C b) => C (P a b)`)

2) _All instances must be uniform across supported languages_. Because the LambdaBuffers codegen component (and _not_ the user) is responsible for generating instances, we must ensure that the codegen component is suitably equipped to generate instances in each language that exhibit behavior which is, to the greatest extent possible, equivalent to the behavior of generated instances in any other language. We _must_ have an extensive test quite to verify uniform behavior of generated instances.

Finally: In languages with a typeclass system (Haskell, PureScript) or equivalent (Rust's Traits), we will utilize the existing system and _should_ (to the extent that doing so is feasible) utilize existing typeclasses and instances from commonly used or standard libraries. In languages lacking a type system that is sufficiently rich to express typeclass relations, we will generate instances using idiomatic language features. (The details will depend on the particular language.)

## Architecture: Constraints and CodeGen

Two components are necessary for integrating typeclass support into LambdaBuffers:

- 1) A system that allows LambdaBuffers developers to declare the _logical relations_ between classes and instances to facilitate solving constraints. On the basis of these logical relations, the constraint solver should be able to determine whether or not an instance declared in the source can be constructed by the CodeGen, and report any errors to the user. This component should be part of the LamdaBuffers _Compiler_.
- 2) A system that generates instance declarations in the target languages. This component should be part of the LambdaBuffers CodeGen toolkit.

## Compiler Component: Constraint Solving

In order to determine whether a typeclass instance can be generated for a type defined in the schema language, the LambdaBuffers compiler must support a _constraint solver_ which uses a set of predefined rules.

The constraint solver must support constrained instances, and, consequently, must support type variables in instance declarations. For type constructors paramterized by type variables, e.g. `data T a = T a`, it should be possible to write constrained instances of the sort `instance Eq a => Eq (T a)`, and the polymorphism of that instance should be reflected in the generated code.

The constraint solver ought to support _SuperClass_ constraints, i.e. constraints which appear in the context of a class definition in Haskell, e.g. the `S` in `class S x => C x`.

The constraint solver should be of roughly equivalent power to Haskell's constraint solver. That is, if a `instance C Int` instance exists for some class `C`, and a `instance C a => C [a]` instance exists, the solver should be able to determine that a `C [Int]` constraint can be satisfied.

Users will not be able to define new typeclasses in the schema language itself. Ideally, the LambdaBuffers developers will provide a framework or module system for defining new typeclasses so that contributors can add support for new classes in a modular way, however this should not be considered necessary for the initial release of LambdaBuffers.

Users are responsible for ensuring that all of the relevant instance declarations in a SuperClass hierarchy have been provided. For example, if we define the `Ord` class in the same way that Haskell does: `class Eq a => Ord a`, and a user declares an `instance Ord T` for some type `T` but does not declare an `instance Eq T` for that type, the constraint solver should emit an error that warns the user about the missing superclass.

The constraint solver should also emit a helpful error message if a user attempts to declare an instance for an unsupported typeclass, or attempts to declare an instance that cannot be derived. Note that, due to the aforementioned restrictions on supported typeclasses - i.e. that they must be structurally derivable - it is conceptually possible to determine whether an instance _could_ be derived given other suitable instance declarations. We ought to be able to tell the user which intermediate instances they are missing in our error message. (NOTE: Introduce an example here to make this clearer)

The constraint solver component of the compiler is conceptually separate from the rest of the compiler, and can be developed as a separate module.

## CodeGen Component

The previous section is ambiguous as to where the initial set of in-scope typeclass instances come from. There are two possible approaches:

- 1) Every instance is hardcoded in the compiler component itself, and the codegen component must either support those hardcoded instances for each language or error out if it does not support them.

- 2) Both the initial set of instances (which function as deriving rules) and the code generation tools for those instances are bundled together outside of the compiler or codegen components.
