<!-- markdownlint-disable-file -->

# Comparison Matrix

Legend:

- 🟢 Available (grading added in some cases spanning: Bad, Average, Good, Excellent)
- 🟡 In development
- 🔵 Potential future feature
- 🔴 Not currently available
- ❔ Not clear

| **Feature**                                            | **Proto Buffers** | **ADL**      | **JSON Schema** | **Lambda Buffers** | **CDDL** | **ASN.1**    |
|--------------------------------------------------------|-------------------|--------------|-----------------|--------------------|----------|--------------|
| Sum types                                              | 🟢                | 🟢           | 🔴              | 🟢                 | 🟢       | 🟢           |
| Record types                                           | 🟢                | 🟢           | 🟢              | 🟢                 | 🟢       | 🟢           |
| Product types                                          | 🔴                | 🔴           | 🔴              | 🟢                 | ❔       | 🔴           |
| Recursive types                                        | 🟢                | 🟢           | 🔴              | 🟢                 | 🟢       | ❔           |
| Parameterized types (generic types)                    | 🔴                | 🟢           | 🔴              | 🟢                 | 🟢       | 🔴           |
| Type annotations/constraints                           | 🟢                | 🟢           | 🟢              | 🔵                 | 🟢       | 🟢           |
| Add new builtin types                                  | 🔴                | 🟢           | 🔴              | 🟢                 | 🔴       | 🔴           |
| Add new type semantics (e.g. different encodings)      | 🟢                | 🟢           | 🔴              | 🟢                 | 🔴       | 🟢           |
| Manage type semantics (at language level)              | 🔴                | 🔴           | 🔴              | 🟢                 | 🔴       | 🔴           |
| Codegen support                                        | 🟢 (Excellent)    | 🟢 (Average) | 🟢 (Excellent)  | 🟡                 | 🟢 (Bad) | 🟢 (Average) |
| DevOps tooling - build system integration              | 🟢                | 🔴           | ❔              | 🟡                 | 🔴       | 🔴           |
| Documentation tooling                                  | 🟢                | 🔴           | 🟢              | 🔵                 | 🔴       | ❔           |
| Formatting, linting, and development environment tools | 🟢                | 🔴           | 🟢              | 🟢                 | 🔴       | 🔴           |
| Language checker API                                   | 🟢                | 🔴           | 🟢              | 🟢                 | 🔴       | 🔴           |
| Codegen API                                            | 🟢                | 🟢           | 🔴              | 🟢                 | 🔴       | 🔴           |
| Language specification                                 | 🟢                | 🟢           | 🟢              | 🟢                 | 🟢       | 🟢           |
| Backwards compatibility strategy                       | 🟢                | 🔴           | 🔴              | 🔴                 | 🔴       | 🔴           |

## Descriptions

### Sum Types

Types of the form `Time = Present | Past | Future`, which allow a type do be
constructed by one of many variants. Think Rust's `enums`.

### Product Types

Types of the form `Person = MkPerson Age Name`, where `MkPerson` is of Kind
`Type->Type->Type`. Product types combine multiple elements into one data type
without tagging the elements.

### Record Types

Types of the form `Person = MkPerson { age :: Age, name :: Name }`. Record types
are similar to `structs` in most programming languages.

### Recursive Types

Recursive types are defined by the presence of the LHS type in its RHS
definition. A classic example is:

```text
List a = Nil | Cons a (List a)
^^^^^^                 ^^^^^^
```

### Type Functions (Generics)

Type functions allow for the introduction of type variables in the LHS definition
of the term - creating a parametrised type definition. The classic example is
`Maybe a` which is the equivalento of `Option <A>` in rust:

```text
Maybe a = Nothing | Just a
```

Using the above type definition we can now define another type that uses `Maybe`
and instantiates it to use `Integer`

```text
Time_Saved_via_LambdaBuffers = Maybe Integer
```

### Type Annotations / Constraints

There exists a system of constraining or further annotating types - enriching
the type's specification.

### Add New Built-in Types

Refer to [./design](./design.md) - section `### Extensibility to new types`.

### Add New Type Semantics

Refer to [./design](./design.md) - section `### Extensibility to new semantics`.

### Manage Type Semantics (at Language Level)

Refer to [./design](./design.md) - section `### Extensibility to new semantics`.

### Codegen Support

Codegen support relates to the language being able to generate types for other
programming languages.

### DevOps Tooling - Build System Integration

The framework/language provides a seamless way of integrating with normal build
tools and systems (eg. Bazel, Nix, etc.).

### Documentation Tooling

The language can generate human readable documentation in an easy to share and
view format. For example HTML, or Markdown.

### Formatting, Linting, and Development Environment Tools

Tools that allow formatting, linting, and automating standardisation of the
language specific files.

### Language Checker API

The language checker component exposes an API to interface with itself in a
language agnostic manner.

### Codegen API 

The language codegen component exposes an API to interface with itself in a
language agnostic manner.

### Language Specification

There exists a well defined language specification document. 

### Backwards Compatibility Strategy

The language makes certain backwards compatibility guarantees between versions of
the same type definition.

## References


- https://json-schema.org/implementations.html
- https://www.rfc-editor.org/rfc/rfc8610
- https://github.com/timbod7/adl
- https://www.itu.int/en/ITU-T/asn1/Pages/introduction.aspx
- https://protobuf.dev/
- https://github.com/dcSpark/cddl-codegen
