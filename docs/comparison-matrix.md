<!-- markdownlint-disable-file -->

# Comparison Matrix 

Legend:

- 🟢 Available (grading added in some cases spanning: Bad, Average, Good, Excellent)
- 🟡 In development
- 🔵 Potential future feature
- 🔴 Not currently available
- ❔ Not clear

| **Feature**                                            | **Proto Buffers** | **ADL**      | **JSON Schema** | **Lambda Buffers** | **CDDL** | **ASN.1**    |
|--------------------------------------------------------+-------------------+--------------+-----------------+--------------------+----------+--------------|
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
| Documentation tooling                                  | 🟢                | ❔           | 🟢              | 🔵                 | 🔴       | ❔           |
| Formatting, linting, and development environment tools | 🟢                | 🔴           | 🟢              | 🟢                 | 🔴       | 🔴           |
| Documentation tooling                                  | 🟢                | 🔴           | 🟢              | 🔵                 | 🔴       | ❔           |
| Language checker API                                   | 🟢                | 🔴           | 🟢              | 🟢                 | 🔴       | 🔴           |
| Codegen API                                            | 🟢                | 🟢           | 🔴              | 🟢                 | 🔴       | 🔴           |
| Language specification                                 | 🟢                | 🟢           | 🟢              | 🟢                 | 🟢       | 🟢           |
| Backwards compatibility strategy                       | 🟢                | 🔴           | 🔴              | 🔴                 | 🔴       | 🔴           |

## Features

### Sum types

A type that can take on several different forms, also referred to as a *tagged
union* or a *variant* (see https://en.wikipedia.org/wiki/Tagged_union).

An example sum type definition in Haskell

```haskell

data Either a b = Left a | Right b
```
### Record types

A record type is essentially a product type where each field is accompanied by a
field name (https://en.wikipedia.org/wiki/Product_type)

### Product types

A product type is a tuple of types.

### Recursive types

Recursive types are types that are defined in terms of themselves.

```haskell

data List a = Nil | Cons a (List a)
data Tree a = Leaf a | Branch (Tree a) (Tree a)
```

## References 

- https://json-schema.org/implementations.html
- https://www.rfc-editor.org/rfc/rfc8610
- https://github.com/timbod7/adl
- https://www.itu.int/en/ITU-T/asn1/Pages/introduction.aspx
- https://protobuf.dev/
- https://github.com/dcSpark/cddl-codegen
