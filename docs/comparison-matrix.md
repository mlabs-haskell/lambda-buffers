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
| Sum Types                                              | 🟢                | 🟢           | 🔴              | 🟢                 | 🟢       | 🟢           |
| Record Types                                           | 🟢                | 🟢           | 🟢              | 🟢                 | 🟢       | 🟢           |
| Product Types                                          | 🔴                | 🔴           | 🔴              | 🟢                 | ❔       | 🔴           |
| Recursive Types                                        | 🟢                | 🟢           | 🔴              | 🟢                 | 🟢       | ❔           |
| Type functions (Generics)                              | 🔴                | 🟢           | 🔴              | 🟢                 | 🟢       | 🔴           |
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

:todo: add chapter elaborating on each feature
## References 

- https://json-schema.org/implementations.html
- https://www.rfc-editor.org/rfc/rfc8610
- https://github.com/timbod7/adl
- https://www.itu.int/en/ITU-T/asn1/Pages/introduction.aspx
- https://protobuf.dev/
- https://github.com/dcSpark/cddl-codegen
