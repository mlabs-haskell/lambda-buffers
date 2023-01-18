<!-- markdownlint-disable-file -->

# Comparison Matrix 

Legend:

- 🟢 Available
- 🟡 In development
- 🔵 Potential future feature
- 🔴 Not currently available

|----------------------------------|----------------------|---------|-----------------|--------------------|
| **Feature**                      | **Protocol Buffers** | **ADL** | **JSON Schema** | **Lambda Buffers** |
|----------------------------------|----------------------|---------|-----------------|--------------------|
| Sum Types                        | 🟢                   | 🟢      | 🟢              | 🟢                 |
|----------------------------------|----------------------|---------|-----------------|--------------------|
| Record Types                     |                      |         |                 | 🟢                 |
|----------------------------------|----------------------|---------|-----------------|--------------------|
| Product Types                    |                      |         |                 | 🟢                 |
|----------------------------------|----------------------|---------|-----------------|--------------------|
| Recursive Types                  |                      |         |                 | 🟢                 |
|----------------------------------|----------------------|---------|-----------------|--------------------|
| Types as single source of truth  |                      |         |                 | 🟢                 |
| by many backeds to generate type |                      |         |                 |                    |
| definitions in target languages. |                      |         |                 |                    |
|----------------------------------|----------------------|---------|-----------------|--------------------|
| Generate markdown or html using  |                      |         |                 | 🔵                 |
| type definitions.                |                      |         |                 |                    |
|----------------------------------|----------------------|---------|-----------------|--------------------|
| Formatting tool.                 |                      |         |                 | 🔵                 |
|----------------------------------|----------------------|---------|-----------------|--------------------|
| Opaque types support.            |                      |         |                 | 🟢                 |
|----------------------------------|----------------------|---------|-----------------|--------------------|
| Modular design and decoupled .   | 🟢                   |         |                 | 🟢                 |
| components allow for easy        |                      |         |                 |                    |
| interfacing and  building on top |                      |         |                 |                    |
| of the components                |                      |         |                 |                    |
|----------------------------------|----------------------|---------|-----------------|--------------------|
| Backwards compatibility          |                      |         |                 | 🔵                 |
|----------------------------------|----------------------|---------|-----------------|--------------------|
|                                  |                      |         |                 |                    |





## Google Protocol Buffers

### Similar features

- Offer a single source of truth that is used as a generation component for type
-definitions in many other languages.

- Recursive types.

### Currently Missing Feature

- Documentation generation - generate markdown or html documentation from the
-type definitions.

- Formatting tool.

### Additional Features

- Can specify type functions.

- Can specify and generate typeclass instances.

- Type class instances are checked by the Compiler.

- Unification based Kind checking.

### Additional Features Considered

- Type annotations

- Leveraging `Naturals` and `String Literals` in the type definition.

- Leveraging polykinded types, and type schemes in type definitions.

- Cardinality check of types. 

TODO: 
- JSON Schema
- CDDL 
- feature matrix 
