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
| Documentation tooling                                  | 🟢                | ❔           | 🟢              | 🔵                 | 🔴       | ❔           |
| Formatting, linting, and development environment tools | 🟢                | 🔴           | 🟢              | 🟢                 | 🔴       | 🔴           |
| Documentation tooling                                  | 🟢                | 🔴           | 🟢              | 🔵                 | 🔴       | ❔           |
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

:FIXME: @Drazen

### Add New Built-in Types

## References


- https://json-schema.org/implementations.html
- https://www.rfc-editor.org/rfc/rfc8610
- https://github.com/timbod7/adl
- https://www.itu.int/en/ITU-T/asn1/Pages/introduction.aspx
- https://protobuf.dev/
- https://github.com/dcSpark/cddl-codegen
