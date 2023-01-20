# LambdaBuffers design

The goal of the LambdaBuffers project is to enable software developers to
specify their application types in a common format that can be conveniently
shared and their values effectively communicated across language barriers.

## Problem statement

Software projects that span multiple language environments often interact in a
sub-optimal manner. Significant effort is spent in making application messages
declared in one language environment available to other language environments.

This burden is particularly onerous in manually written and managed
serialization/encoding code which is used to communicate application values
in the context of networking, configuration and databases.

Ensuring compatibility, consistency and correctness of application messages is a
difficult, tedious and error prone process that often results in unwarranted
costs to the business and unsustainable technical debt.

## Requirements

1. Expressive types,
2. Expressive semantics annotation,
3. Extensible to new types,
4. Extensible to new semantics,
5. Universally consistent semantics,
6. Modular API architecture.

### Expressive types

Application types that users can define should be expressive enough to
facilitate type driven domain modeling and to express application
programming interfaces.

Taking inspiration from existing type systems, LambdaBuffers supports [algebraic
data types](https://en.wikipedia.org/wiki/Algebraic_data_type) that facilitate
elegant type composition. Such types are well studied and widely used in
functional programming languages such as Haskell.

LambdaBuffers supports first class sum, product and record types. Types can be
parameterized, effectively enabling generic types. LambdaBuffers also supports
recursive type definitions, which allow users to succinctly define elegant and
expressive data structures.

### Expressive semantics annotation

Enabling users to manage the *semantics* associated with their types is
essential for adapting LambdaBuffers to a variety of different domains and use
cases.

While most existing schema systems only facilitate type declarations in a
variety of languages, LambdaBuffers takes a further step and provides users with
the capability to manage the *semantics* of the types defined in schemata by
indicating which basic operations and functions a type ought to support.

For example, suppose a user would like some types to support certain encoding
(e.g. JSON or CBOR). In order to support a particular encoding, functions that
serialize and deserialize the types are needed in the target language(s).
Another example: Users may wish to declare that certain types are numeric - i.e.
that values of these types can be added, subtracted or multiplied in a given
target language. Most types could be declared to support an equality relation,
which requires a function that can check values for equality in the target
language(s).

In order to provide users the capability to manage the *semantics* of the types
they define, LambdaBuffers supports [type
classes](https://en.wikipedia.org/wiki/Type_class), also known as *type
constraints*. Type classes are a well-established mechanism for supporting ad
hoc polymorphism, backed by a large amount of academic research and widely used
in functional programming languages such as Haskell, PureScript, and (albeit
under a different name) Rust.

One essential difference between LambdaBuffers type classes and type classes as
implemented in Haskell/PureScript/Rust is that LambdaBuffers does not allow
users to declare the implementation of type class instances. Instead, users
declare *instance clauses* for their types which signify the *semantics* (i.e.
functions, methods) they wish to be generated in the target language. All
implementation are generated uniformly as elaborated in the specification
document for a given type class.

For each new type class declared, code generation tooling must be updated to
handle the new type class.

### Extensible to new types

Enabling users to introduce new *built-in* types allows LambdaBuffers to be
adapted in many different domains and use cases.

These types have special treatment *under the hood* and are generally mapped onto
existing types and their value representations in the targeted language
environments. For example, a primitive `Int` type in the LambdaBuffers schema
language may be mapped to `Int` in Haskell and `i32` in Rust. Primitive
parameterized types are also possible: A primitive `Maybe` type might be mapped
to `Maybe` in Haskell and to `Option<_>` in Rust.

LambdaBuffers supports [opaque
types](https://en.wikipedia.org/wiki/Opaque_data_type) to provide users with the
power to define their own primitive or builtin types.

Example opaque types include various integer types, sequence types, text types,
sets, maps and other semantically richer data types. Generally, such types are
already well-defined and widely used in various language environments and come
equipped with rich libraries that work with them. Redefining them *ab ovo* would
be counterproductive as users would have to re-implement and reinvent the rich
support for such types.

### Extensible to new semantics

Enabling users to introduce new *type semantics* facilitates LambdaBuffers to be
adapted in many different domains and use cases.

In LambdaBuffers, introducing new *type semantics* works by first declaring a
*type class*, which is simply the name of the class bundled with any
super-classes (should they exist). Next, specification document that elaborates
how instances are to be generated for members of the class must be created,
taking care to ensure that instances are always generated uniformly. Finally, a
code generation module for the class must be written that implements compliant
code generation for different target languages.

Concretely, serialization has special treatment in most technologies in this
space, however in LambdaBuffers, this is just a new type class.

For each new type class, deliberate effort must be invested to support
that 'semantics' in different target language environments in a compliant manner.
(Because type class instances are generated uniformly relative to the
structure of the original type, in accordance with a set of deriving rules
provided in the code generation implementation, the amount of boilerplate
required to implement a new class is substantially reduced if not
entirely eliminated.)

The LambdaBuffers team will officially support a certain set of type classes and
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

LambdaBuffers does not provide a way to formally verify consistency across languages,
however, a comprehensive test suite will be developed to ensure that new code
generation modules are indeed implemented correctly.

### Modular API architecture

LambdaBuffers establishes three separate components of the architecture, namely
*Frontend*, *Compiler* and *Codegen**.

*Frontend* is a user facing component that features a way to input, specify or
otherwise construct application types. *Frontend* also orchestrates the overall
work that includes the *Compiler* and *Codegen*, invoking each of these
components as required by different workflows the *Frontend* supports.
LambdaBuffers officially supports a *Frontend* component implementation that
features a text based language for specifying LambdaBuffers types. However,
because of the modular API architecture approach, a *Frontend* can be
implemented in any language and in any fashion as long as they are able to
interface with the *Compiler* and *Codegen* components.

The *Compiler* is a key component that is made available via the *Compiler
Input* specified via [Google Protocol Buffers](https://protobuf.dev/). It
performs all the necessary checks to ensure that the naming, type definitions
and their declared semantics are indeed valid.

The *Codegen* component consumes the *Compiler Output* that contains all the
information necessary to perform valid and robust code generation. *Frontend*
communicates with the *Codegen* to understand its *Capabilities*, enabling each
*Codegen* module to gradually build up support for desired opaque types and
declared semantics.
