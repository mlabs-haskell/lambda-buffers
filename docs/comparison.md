# Comparison

## Google Protocol Buffers

### Similar features

+ Can specify Sum Types.

+ Can specify Product and Record Types.

+ Modular design and decoupled components allow for easy interfacing and
  building on top of the components.

+ Offer a single source of truth that is used as a generation component for type
  definitions in many other languages.

### Currently Missing Feature

+ Documentation generation - generate markdown or html documentation from the
  type definitions.

+ Formatting tool.

### Additional Features

+ Can specify type functions.

+ Can specify and generate typeclass instances.

+ Type class instances are checked by the Compiler.

+ Unification based Kind checking.

### Additional Features Considered

+

+ Leveraging `Naturals` and `String Literals` in the type definition.

+ Leveraging polykinded types, and type schemes in type definitions.
