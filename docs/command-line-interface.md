# LambdaBuffers Command Line Interface

LambdaBuffers consists of three runtime command line interface components:

- [lambda-buffers-frontend-cli](lambda-buffers-frontend-cli)
- [lambda-buffers-compiler-cli](lambda-buffers-compiler-cli)
- [lambda-buffers-codegen-cli](todo-link)

The *Frontend* CLI orchestrates work between the user and the *Compiler* and
*Codegen* components.

It's desirable to have both the *Compiler* CLI and the *Codegen* CLI subject to
a strict API with a specified set of flags to enable CLI implementation from
various sources. This would be especially helpful with *Codegen* modules that
bring about support for new targets, opaque types and typeclasses.

<!-- TODO(bladyjoker): Complete this chapter -->
