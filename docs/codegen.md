# LambdaBuffers Codegen

NOTE: The implementation of the code generation framework is still in early stages and will likely undergo substantial changes as development continues. This document serves to outline general principles that any implementation of the LambdaBuffers code generation framework ought to adhere to.

## Requirements

1. Modular & reusable components
2. Ergonomic interface
3. Extensible to new opaque types
4. Extensible to new type classes

### Modular & Reusable Components

Because the code generation modules for each target language will almost certainly constitute the bulk of the final LambdaBuffers codebase, it is essential that components of the code generation framework be as modular and reusable as is practicable.

Although each target language has its own distinct syntax and semantics, many syntactic forms exist in multiple target languages. For example, Haskell and PureScript both use a comma-separated set enclosed in square brackets (e.g. `[1,2,3]`) to represent a `List _/[_]` value, while Rust uses a similar form (in conjunction with a macro, e.g. `vec![1,2,3]`) to represent a `Vector<_>` value. To reduce redundant and superfluous code, common syntactic patterns such at this should be abstracted out into a set of functions that can be reused across languages.

### Ergonomic Interface

While the LambdaBuffers team will support a finite set of specific target languages, adding support for an additional language should be as painless as possible (ceteris paribus) to encourage and facilitate open source contributions by third parties. A basic set of tools which can be used to write code generation modules for any target language should be developed, and all code generation modules written by the LambdaBuffers team should employ those tools (in order to provide a robust set of examples for future contributors, among other benefits).

### Extensible to New Opaque Types

Users and contributors should be able to easily extend the default set of supported opaque types to support additional opaque types. In the context of code generation, this means: Users should have the ability to specify the target type for a given opaque type in the target language (including the package or module that contains the target type if the target type is not part of the language's standard library).

Because type class instances must be derived structurally, and because an opaque type is by definition a type with no visible internal structure, users should be provided with an ergonomic interface for noting the presence of a type class instance for an opaque type's target type in a particular language (if the instance exists in the standard library), or for referring to the module where such an instance is located (if the instance is defined in a library or by the user in one of their own modules).

### Extensible to New Type Classes

Users and contributors should be able to easily extend the default set of supported type classes to support additional type classes and facilitate the derivation of instances for newly defined classes.

In practice, the type class code generation machinery must consist of two distinct parts: First, a set of deriving rules which correspond to instances that already exist (in some module or in a language's standard library/prelude). Second, code generation functions that operate on user-defined types which satisfy those rules.

Each of these parts should be ergonomic, well-documented, and the implementation of the default set of supported type classes ought to be thoroughly commented in order that users have a diverse set of real examples to work from.
