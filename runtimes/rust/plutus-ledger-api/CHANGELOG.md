<!-- markdownlint-disable MD024 -->

# Changelog

This changelog is based on [Keep A
Changelog](https://keepachangelog.com/en/1.1.0).

## 3.1.0

### Added

- Add `insert_ada_mut` to `Value`
- Add `insert_token_mut` to `Value`
- Add `Copy` derive macro to `Extended<T>` and `Vote`
- Update all dependencies (including cardano-serialization-lib 15.0.1)

## v3.0.4

### Changed

- Updating all dependencies to latest minor versions

## v3.0.3

### Changed

- Cardano serialisation lib upgraded to 14.1.2

## v3.0.2

### Changed

- Add missing derive macros
- Added a few goldens data for V3 types

## v3.0.1

### Changed

- Added V3 TransactionHash with proper IsPlutusData implementation
- Fix some bugs in V3 types
- Updated dependencies

## v3.0.0

### Changed

- Fixed typos in V3 types
- Fixed Display alternative implementations

## v2.1.0

### Added

- Re-exported cardano-serialization-lib as `plutus-ledger-api::csl::lib`

### Changed

- Updated to cardano-serialization-lib 13.1.0

## v2.0.0

### Added

- Added cardano-serialization-lib conversion traits (`ToCSL` and `FromCSL`) ([#55](https://github.com/mlabs-haskell/plutus-ledger-api-rust/pull/55))
- Added v3 plutus ledger types ([#57](https://github.com/mlabs-haskell/plutus-ledger-api-rust/pull/57))
- Added the ability to derive `IsPlutusData` instances ([#56](https://github.com/mlabs-haskell/plutus-ledger-api-rust/pull/56))
- Added a few utility functions for Values ([#55](https://github.com/mlabs-haskell/plutus-ledger-api-rust/pull/55))
- Added Display and Debug implementations for
  CurrencySymbol, TokenName, Value, AddressWithExtraInfo and some other types ([#55](https://github.com/mlabs-haskell/plutus-ledger-api-rust/pull/55))
- Added FromStr implementations for CurrencySymbol, TokenName, Value, Address
  and some other types ([#55](https://github.com/mlabs-haskell/plutus-ledger-api-rust/pull/55))

### Changed

- Fixed `serde` serialization of Plutus `Value`s
- Updated cardano-serialization-lib to Conway compatible 12.1.1

## v1.0.0

### Added

- Added golden tests ([#45](https://github.com/mlabs-haskell/plutus-ledger-api-rust/pull/45))

## v0.2.1

### Changed

- Use published lbr-prelude and lbr-prelude-derive ([#42](https://github.com/mlabs-haskell/plutus-ledger-api-rust/pull/42))

## v0.2.0

Start of this Changelog

## v0.1.0

MVP version including all Plutus Ledger types
