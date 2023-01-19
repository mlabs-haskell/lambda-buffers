# LambdaBuffers design

The goal of the LambdaBuffers project is to enable software developers to
specify their application types in a common format that can be conveniently
shared and their values effectively communicated across language barriers.

## Problem statement

Software projects that span multiple language environments often interact in a
sub-optimal manner. Significant effort is spent in making application messages
declared in one language environment available to other language environments.

This toil is usually manifested in manually written and managed
serialization/encoding code that is then used to communicate application values
in the context of networking, configuration and databases.

Ensuring compatibility, consistency and correctness of application messages is a
difficult, tedious and error prone process that often results in unwarranted
costs to the business and unsustainable technical debt.

## Requirements

1. Expressive types,
2. Expressive semantics annotation,
3. Extensibility to new types,
4. Extensibility to new semantics,
5. Universally consistent semantics,
6. Modular architecture with APIs for each component.

### Expressive types

Application types that users can define should be expressive enough to
facilitate type driven domain modeling and to succinctly express application
programming interfaces.

Taking inspiration from existing type systems, LambdaBuffers supports [algebraic
data types](https://en.wikipedia.org/wiki/Algebraic_data_type) that facilitates
elegant type composition. Such types are well studied and widely used in
functional programming languages such as Haskell.

LambdaBuffers supports first class sum, product and record types. Types can be
parametrized, effectively enabling generic types, and defined in recursive
manner making them sufficiently expressive.

### Expressive semantics annotation

Enabling users to manage the *semantics* associated with their types is
essential to adapting LambdaBuffers in many different domains and use cases.

For a given corpus of application types, users are enabled to declare their
*semantics* by stating per type what can be done with them.

For example, users would like some types to have certain byte encodings, like
JSON or CBOR, and have functions performing these operations made available in
the target languages. Some types could be declared as numeric meaning they can
be added, subtracted or multiplied in a given target language. Most types could
be declared to support equality relation, making function checking for equality
on values of said types available in the target language.

LambdaBuffers supports [type classes](https://en.wikipedia.org/wiki/Type_class),
also known as *type constraints* for that purpose. Another concept that's well
studied and widely used in functional programming languages such as Haskell.

Users declare *instance clauses* for their types signifying the kind of
*semantics* they wish generated in the target language. User don't have the
ability to provide actual implementations, as all implementation are generated
uniformly as elaborated in the specification document needed for any type class.

For each new type class declared, code generation tooling must be updated to
handle the new type class.

### Extensibility to new types

Enabling users to introduce new *built-in* types facilitates LambdaBuffers to be
adapted in many different domains and use cases.

These types have special treament *under the hood* and are generally mapped onto
existing types and their value representations in the targeted language
environments.

LambdaBuffers supports [opaque
types](https://en.wikipedia.org/wiki/Opaque_data_type) for that exact purpose
that can be used to define a whole new domain.

Example opaque types include various integer types, sequence types, text types,
sets, maps and other semantically richer data types. Generally, such types are
already well-defined and widely used in various language environments and come
equipped with rich libraries that work with them. Redefining them *ab ovo* would
be counterproductive as users would have to reimplement and reinvent the rich
support for such types.

### Extensibility to new semantics

Enabling users to introduce new *type semantics* facilitates LambdaBuffers to be
adapted in many different domains and use cases.

In LambdaBuffers, introducing new *type semantics* works by first declaring a
*type class*, then by providing a specification document that elaborates how
LambdaBuffers types are uniformly handled and then providing code generation
modules that implement compliant code generation modules for different target
languages.

Concretely, serialization has special treatment in most techologies in this
space, however in LambdaBuffers, this is just a new type class.

There's not much that users get for free here, for each new type class,
deliberate effort must be invested to support that 'semantics' in different
target language environments in a compliant manner.

LambdaBuffers team will officially support a certain set of type classes and
provide code generation modules for a set of officially supported language
environments. However, modular design will hopefully facilitate community
contributions in a sustainable fashion.

### Universally consistent semantics

The types specified by users must be handled in a compatible manner across
language environments. This is a critical requirement that underpins this
project.

If two values of a LambdaBuffers type are considered as 'equal' in one language
environment, they should be considered equal in all others. In a similar
fashion, if LambdaBuffers type has a declared JSON serialization semantics,
values encoded as JSON in one language environment have to have the same
encoding performed in all other language environments.

LambdaBuffers doesn't provide a way to formally ensure such consistency,
however, a comprehensive test suite could be developed to make sure new code
generation modules are indeed implemented correctly.

### Modular architecture with APIs for each component
