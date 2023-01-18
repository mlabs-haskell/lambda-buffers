<!-- markdownlint-disable-file -->

# Comparison Matrix 

Legend:

- 🟢 Available
- 🟡 In development
- 🔵 Potential future feature
- 🔴 Not currently available
- ❔ Not clear

| **Feature**                                      | **Proto Buffers** | **ADL** | **JSON Schema** | **Lambda Buffers** | **CDDL** | **ASN.1** |
|--------------------------------------------------|-------------------|---------|-----------------|--------------------|----------|-----------|
| Sum Types                                        | 🟢                | 🟢      | 🔴              | 🟢                 |          |           |
| Record Types                                     | 🟢                |         |                 | 🟢                 |          |           |
| Product Types                                    | 🟢                | 🟢      | 🟢              | 🟢                 |          |           |
| Recursive Types                                  |                   |         |                 | 🟢                 |          |           |
| Type functions                                   | 🔴                | 🔴      | 🔴              | 🟢                 |          |           |
| One type schema for many languages.              |                   |         |                 | 🟢                 |          |           |
| Generate markdown or html using type definitions |                   |         |                 | 🔵                 |          |           |
| Formatting tool                                  |                   |         |                 | 🔵                 |          |           |
| Opaque types support                             |                   |         |                 | 🟢                 |          |           |
| Modular design and decoupled internal components | 🟢                | 🔴      | 🔴              | 🟢                 |          |           |
| Backwards compatibility                          | 🟢                |         |                 | 🔵                 |          |           |
| Unification based Kind checking.                 |                   |         |                 | 🟢                 |          |           |
| Type annotations                                 |                   |         |                 | 🔵                 |          |           |
|                                                  |                   |         |                 |                    |          |           |

## Google Protocol Buffers

### Similar features

- Offer a single source of truth that is used as a generation component for type
-definitions in many other languages.

- Recursive types.

### Additional Features

- Can specify type functions.

- Can specify and generate typeclass instances.

- Type class instances are checked by the Compiler.

- 

### Additional Features Considered

- Type annotations

- Leveraging `Naturals` and `String Literals` in the type definition.

- Leveraging polykinded types, and type schemes in type definitions.

- Cardinality check of types. 

TODO: 
- JSON Schema
- CDDL 
- feature matrix 
