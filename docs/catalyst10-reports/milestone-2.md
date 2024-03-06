# Catalyst milestone 2: Javascript support

While the milestone technically requires Javascript support, we implemented [Typescript](https://www.typescriptlang.org/) support which is a [typed superset of Javascript](https://www.typescriptlang.org/docs/handbook/typescript-from-scratch.html#a-typed-superset-of-javascript). This was done to better interpolate with existing Javascript libraries on Cardano (which were actually implemented in Typescript) such as [lucid](https://github.com/spacebudz/lucid), [cardano-js-sdk](https://github.com/input-output-hk/cardano-js-sdk), [cardano-serialization-lib](https://www.npmjs.com/package/@emurgo/cardano-serialization-lib-nodejs), etc.

## Outputs

- [x] A LambdaBuffers code generation module that outputs type constructors and derived implementations in the Javascript programming language given a LambdaBuffers schema.
  - The module implementation is in [lambda-buffers-codegen/src/LambdaBuffers/Codegen/Typescript](https://github.com/mlabs-haskell/lambda-buffers/tree/2e2ed98f4df7f207ec3cf131adf5b47b202e248f/lambda-buffers-codegen/src/LambdaBuffers/Codegen/Typescript).

- [x] A Javascript library that implements the LambdaBuffers Prelude runtime.
      This module would include standardised JSON encoding and equality
      implementations for all declared type class instances in the schema.
      - A standalone Prelude library for Typescript was implemented together with
        its runtime for LambdaBuffers. These can be found here:
        - [prelude-typescript](https://github.com/mlabs-haskell/prelude-typescript)
        - [runtimes/typescript/lbr-prelude](https://github.com/mlabs-haskell/lambda-buffers/tree/343a1a900f42dcf5b1c1a7e330eafb07c280908b/runtimes/typescript/lbr-prelude)

- [x] A Javascript test suite that assures the manually implemented and automatically generated implementations are consistent with the predefined LambdaBuffers Prelude golden data set of JSON files and perform correct implementation derivation.
        - A test suite ensuring that the manually implemented and automatically generated implementations can be found here: [testsuites/lbt-prelude/lbt-prelude-typescript](https://github.com/mlabs-haskell/lambda-buffers/tree/50bfbc4a182275d42be978b5a251530bab84f4aa/testsuites/lbt-prelude/lbt-prelude-typescript).
- [x] A Javascript library that implements the LambdaBuffers Plutus runtime. This module would include standardised PlutusData encoding implementations for all declared type class instances in the Plutus schema.
        - A standalone Plutus library for Typescript was implemented together with
          its runtime for LambdaBuffers. These can be found here:

          - [plutus-ledger-api-typescript](https://github.com/mlabs-haskell/plutus-ledger-api-typescript)

          - [runtimes/typescript/lbr-plutus](https://github.com/mlabs-haskell/lambda-buffers/tree/dc5ee6797d1230661d6bb3dfa658eddeadd7cb60/runtimes/typescript/lbr-plutus)

- [x] A Javascript test suite that assures the manually implemented and automatically generated implementations are consistent with the predefined LambdaBuffers Plutus golden data set of PlutusData encoded files and perform correct implementation derivation.
      - A test suite ensuring that the manually implemented and automatically generated implementations can be found here: [testsuites/lbf-plutus/lbf-plutus-typescript](https://github.com/mlabs-haskell/lambda-buffers/tree/50bfbc4a182275d42be978b5a251530bab84f4aa/testsuites/lbt-plutus/lbt-plutus-typescript)
- [x] Nix devops modules (Nix API) for streamlining the LambdaBuffers code generation pipeline to Javascript.
      - New flake modules were implemented to easily generate NPM packages from LambdaBuffers
        - [lbf-typescript](https://github.com/mlabs-haskell/lambda-buffers/blob/f59bdb78d06fa677567d053eddb3d1fe46250fd8/extras/lbf-nix/lbf-typescript.nix)
        - [lbf-prelude-typescript](https://github.com/mlabs-haskell/lambda-buffers/blob/f59bdb78d06fa677567d053eddb3d1fe46250fd8/extras/lbf-nix/lbf-prelude-typescript.nix)
        - [lbf-plutus-typescript](https://github.com/mlabs-haskell/lambda-buffers/blob/f59bdb78d06fa677567d053eddb3d1fe46250fd8/extras/lbf-nix/lbf-plutus-typescript.nix)
- [x] Documentation on LambdaBuffers usage patterns for Javascript.
      - [A new page was created for the TypeScript use case](https://mlabs-haskell.github.io/lambda-buffers/typescript.html)

## Acceptance Criteria

- [x] LambdaBuffers schemas that are based on the LambdaBuffers Prelude module can be used in Javascript projects to specify application types.
      - Test libraries for Prelude demonstrate how TypeScript code for the LambdaBuffers Prelude is generated to TypeScript and used in a library: [testsuites/lbt-prelude/lbt-prelude-typescript](https://github.com/mlabs-haskell/lambda-buffers/tree/50bfbc4a182275d42be978b5a251530bab84f4aa/testsuites/lbt-prelude/lbt-prelude-typescript)
      - Moreover, there is a [docs/typescript-prelude](https://github.com/mlabs-haskell/lambda-buffers/tree/1d806a1710aab625ea520c596a72338c5bde578d/docs/typescript-prelude) sample project which also demonstrates this.
- [x] LambdaBuffers schemas that are based on the LambdaBuffers Plutus module can be used in Javascript projects to specify application types.
      - Test libraries for Plutus demonstrate how TypeScript code for the LambdaBuffers Prelude is generated to TypeScript and used in a library: [testsuites/lbt-plutus/lbt-plutus-typescript](https://github.com/mlabs-haskell/lambda-buffers/tree/50bfbc4a182275d42be978b5a251530bab84f4aa/testsuites/lbt-plutus/lbt-plutus-typescript)
      - Moreover, there is a [docs/typescript-plutus](https://github.com/mlabs-haskell/lambda-buffers/tree/1d806a1710aab625ea520c596a72338c5bde578d/docs/typescript-plutus) sample project which also demonstrates this.
- [x] The documentation and devops tooling is available to facilitate easy adoption.
      - Similarly to other languages supported by LambdaBuffers, a [TypeScript flake](https://github.com/mlabs-haskell/flake-lang.nix/tree/5bb4fdf556a2f2f23717c654c186f13f28b9c277/flake-lang/typescript) is implemented along with its documentation.
        The testing libraries also serve as an example, to understand how to use these Nix utilities:
        [testsuites/lbt-plutus/lbt-plutus-typescript/build.nix](https://github.com/mlabs-haskell/lambda-buffers/blob/4c6304cf3a3a0c08bbb46e94532a293fdea513e9/testsuites/lbt-plutus/lbt-plutus-typescript/build.nix).

## Evidence of Milestone Completion

- [x] The completed and reviewed LambdaBuffers Prelude runtime library is available for the Javascript programming language.
      - [runtimes/typescript/lbr-prelude](https://github.com/mlabs-haskell/lambda-buffers/tree/343a1a900f42dcf5b1c1a7e330eafb07c280908b/runtimes/typescript/lbr-prelude)
      - [prelude-typescript](https://github.com/mlabs-haskell/prelude-typescript)
- [x] The completed and reviewed LambdaBuffers Plutus runtime library is available for the Javascript programming language.
      - [runtimes/typescript/lbr-plutus](https://github.com/mlabs-haskell/lambda-buffers/tree/dc5ee6797d1230661d6bb3dfa658eddeadd7cb60/runtimes/typescript/lbr-plutus)
      - [plutus-ledger-api-typescript](https://github.com/mlabs-haskell/plutus-ledger-api-typescript)
- [x] The completed and reviewed LambdaBuffers Prelude test suite is available and is passing in CI for the Javascript programming language.
      - [testsuites/lbt-prelude/lbt-prelude-typescript](https://github.com/mlabs-haskell/lambda-buffers/tree/50bfbc4a182275d42be978b5a251530bab84f4aa/testsuites/lbt-prelude/lbt-prelude-typescript)
- [x] The completed and reviewed LambdaBuffers Plutus test suite is available and is passing in CI for the Javascript programming language.
      - [testsuites/lbf-plutus/lbt-plutus-typescript](https://github.com/mlabs-haskell/lambda-buffers/tree/50bfbc4a182275d42be978b5a251530bab84f4aa/testsuites/lbt-plutus/lbt-plutus-typescript)
- [x] The completed and reviewed Nix API for LambdaBuffers Javascript support is available.
      -  [lbf-typescript](https://github.com/mlabs-haskell/lambda-buffers/blob/f59bdb78d06fa677567d053eddb3d1fe46250fd8/extras/lbf-nix/lbf-typescript.nix)
      -  [lbf-prelude-typescript](https://github.com/mlabs-haskell/lambda-buffers/blob/f59bdb78d06fa677567d053eddb3d1fe46250fd8/extras/lbf-nix/lbf-prelude-typescript.nix)
      -  [lbf-plutus-typescript](https://github.com/mlabs-haskell/lambda-buffers/blob/f59bdb78d06fa677567d053eddb3d1fe46250fd8/extras/lbf-nix/lbf-plutus-typescript.nix)
- [x] The  completed and reviewed LambdaBuffers for Javascript documentation is available.
      - [TypeScript documentation](google.com) TODO(jaredponn) put the commit here just before this when this is reviewed

## Demo files

- Demo project: [lambda-buffers-for-cardano](https://github.com/mlabs-haskell/lambda-buffers-for-cardano/tree/main/transactions/demo-typescript)
