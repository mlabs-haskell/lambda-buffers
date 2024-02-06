# Catalyst milestone 1: Research

## Outputs

- [x] A report summarizing user interviews and containing a qualitative analysis of the discovered use cases.
  - **STATUS**: Done (#17)
  - An interview with 3 MLabs engineers was performed (1.5h) who's work span multiple Cardano dApp projects. Their [feedback](https://github.com/mlabs-haskell/lambda-buffers/blob/main/docs/feedback/interview-notes.md) is made available in the repo.
  - Additionally, a survey was sent out to MLabs engineers and their [feedback](https://github.com/mlabs-haskell/lambda-buffers/blob/main/docs/feedback/questionnaire-results.pdf) is made available in the repo.
- [x] An architecture design document.
  - **STATUS**: Done (#17)
  - Extensive documentation on [design](https://github.com/mlabs-haskell/lambda-buffers/blob/main/docs/design.md), [compiler](https://github.com/mlabs-haskell/lambda-buffers/blob/main/docs/compiler.md) and [codegen](https://github.com/mlabs-haskell/lambda-buffers/blob/main/docs/codegen.md) is made available in the repo.
- [x] A language specification document elaborating on the data type model features.
  - **STATUS**: Done (#17)
  - Extensive documentation on [design](https://github.com/mlabs-haskell/lambda-buffers/blob/main/docs/design.md), [compiler](https://github.com/mlabs-haskell/lambda-buffers/blob/main/docs/compiler.md) and [codegen](https://github.com/mlabs-haskell/lambda-buffers/blob/main/docs/codegen.md) is made available in the repo.
- [x] A related work document comparing the proposed technology via a feature matrix with others in the same space.
  - **STATUS**: Done (#17, #18)
  - [Document](https://github.com/mlabs-haskell/lambda-buffers/blob/main/docs/comparison-matrix.md) comparing different schema techologies to LambdaBuffers is made available in the repo
- [x] An initial compiler implementation that performs some basic checks in accordance with the language specification.
  - **STATUS**: Done (#10)
  - The [initial compiler implementation perform](https://github.com/mlabs-haskell/lambda-buffers/blob/main/lambda-buffers-compiler) `kind checking` is made available in the repo.

## Acceptance Criteria

- [x] At least 3 users/projects have been interviewed about their desired use case for this technology.
  - An interview with 3 MLabs engineers was performed (1.5h) who's work span multiple Cardano dApp projects. Their [feedback](https://github.com/mlabs-haskell/lambda-buffers/blob/main/docs/feedback/interview-notes.md) is made available in the repo.
  - Additionally, a survey was sent out to MLabs engineers and their [feedback](https://github.com/mlabs-haskell/lambda-buffers/blob/main/docs/feedback/questionnaire-results.pdf) is made available in the repo.
- [x] The architecture design document is completed and available in the project repository.
  - Extensive documentation on [design](https://github.com/mlabs-haskell/lambda-buffers/blob/main/docs/design.md), [compiler](https://github.com/mlabs-haskell/lambda-buffers/blob/main/docs/compiler.md) and [codegen](https://github.com/mlabs-haskell/lambda-buffers/blob/main/docs/codegen.md) is made available in the repo.
- [x] The initial compiler implementation is completed, capturing SOME of the intended language semantics as described in the Language Specification
  - The [initial compiler implementation perform](https://github.com/mlabs-haskell/lambda-buffers/blob/main/lambda-buffers-compiler) `kind checking` is made available in the repo.

## Evidence of Milestone Completion

- [x] Completed and reviewed design document is available in the project repository.
  - Extensive documentation on [design](https://github.com/mlabs-haskell/lambda-buffers/blob/main/docs/design.md), [compiler](https://github.com/mlabs-haskell/lambda-buffers/blob/main/docs/compiler.md) and [codegen](https://github.com/mlabs-haskell/lambda-buffers/blob/main/docs/codegen.md) is made available in the repo.
- [x] Completed and reviewed initial version of the compiler command line tool made available in the project repository.
  - The [Frontend CLI](https://github.com/mlabs-haskell/lambda-buffers/blob/main/lambda-buffers-frontend) called `lambda-buffers-frontend-cli` is made available in the repo and is currently able to parse, validate and format `.lbf` documents that contain the LambdaBuffers type modules:

```shell
lambda-buffers/lambda-buffers-frontend$ cabal run 
Usage: lambda-buffers-frontend-cli COMMAND

  LambdaBuffers Frontend command-line interface tool

Available options:
  -h,--help                Show this help text

Available commands:
  compile                  Compile a LambdaBuffers Module (.lbf)
  format                   Format a LambdaBuffers Module (.lbf)
```

There's ongoing work to integrate the [Compiler CLI](lambda-buffers-compiler) in the Frontend CLI.

- [x] Test case: Compiler is able to validate a schema that uses a subset of types and capabilities from the spec.
  - Both the [Frontend](https://github.com/mlabs-haskell/lambda-buffers/blob/main/lambda-buffers-frontend) and the [Compiler](https://github.com/mlabs-haskell/lambda-buffers/blob/main/lambda-buffers-compiler) components are accompanied by a test suite that is routinely run by the projects' CI system.
  - A [corpus of `lbf`](https://github.com/mlabs-haskell/lambda-buffers/blob/main/lambda-buffers-frontend/resources) files is made available in the repo and used in the test suite to ensure correct document handling.
  - The [compiler tests on the kind checking machinery](https://github.com/mlabs-haskell/lambda-buffers/blob/main/lambda-buffers-compiler/test/Test/KindCheck.hs) is also made available in the repo.

## References

- [Catalyst project sheet](https://docs.google.com/spreadsheets/d/16dTxgGsxHvcMe5aCgFPDYEJKgX_VQiNAcwhp2RyA48o/edit#gid=1672366179)
