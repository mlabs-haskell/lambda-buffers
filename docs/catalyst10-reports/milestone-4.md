# Catalyst milestone 4: Separate PlutusTx backend and improvements to existing LambdaBuffers facilities

## Outputs

- [ ] A separate code generator for Haskell and PlutusTx

- [ ] A separate LambdaBuffers code generation module that outputs type definitions and derived implementations for Haskell's Prelude and PlutusTx's Prelude (or equivalent LambdaBuffers' Preludes) given a LambdaBuffers schema.

- [ ] A Haskell test suite that assures the manually implemented and automatically generated implementations are consistent with the predefined LambdaBuffers Prelude golden data set of JSON files and perform correct implementation derivation.

- [ ] Nix devops modules (Nix API) for streamlining the LambdaBuffers code generation pipeline to either Haskell's Prelude or PlutusTx's Prelude.

- [ ] Documentation on LambdaBuffers usage patterns for Haskell's Prelude and PlutusTx's Prelude

- [ ] A complete Plutus .lbf schema file to include all Plutus Ledger API types with backend support for Rust, TypeScript, and PureScript.

- [ ] An extended integration test suite to verify that the manually implemented and automatically implemented instances of the updated LambdaBuffers' Plutus .lbf schema file are consistent across all backends.

- [ ] A versioning scheme for the LambdaBuffers git repository using git tags that follow semantic versioning.

- [ ] Optimized Nix build times.

- [ ] Error messages that follow the GNU error message format.

## Acceptance Criteria

- [ ] An executable and Nix tooling to translate LambdaBuffers modules to Haskell projects to specify application types.

- [ ] An executable and Nix tooling to translate LambdaBuffers modules to PlutusTx projects to specify application types.

- [ ] An updated LambdaBuffers Plutus schema for the complete Plutus Ledger API types.

- [ ] The documentation and devops tooling is available to facilitate easy adoption.

- [ ] Git tags for official releases of LambdaBuffers.

## Evidence of Milestone Completion

- [ ] The completed and reviewed LambdaBuffers code generator for Haskell's Prelude.

- [ ] The completed and reviewed LambdaBuffers code generator for PlutusTx's Prelude.

- [ ] Benchmarks of the before and after optimized Nix build times.

- [ ] Demonstrations (in the form of screenshots or simply text) of error messages following the GNU error message format.

## References

- [Project Catalyst Milestone 4](https://milestones.projectcatalyst.io/projects/1000122/milestones/4)
