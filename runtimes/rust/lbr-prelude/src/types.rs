pub type Either<A, B> = Result<B, A>;

// NOTE: This is used because we need *something* for every "prelude" type.
//       Because LB uses a single configuration (under the "prelude" heading) for
//       all non-ledger opaque types, we need to shove the BLS types for the Haskell-ish
//       backends into that "prelude" config, but we cannot directly support it in
//       non-Haskell backends. (In theory we could, but we'd have to maintain low-level
//       compatibility w/ the Haskell implementation's FFI bindings, which would be a nightmare)
//
//       No one should ever need this functionality in the Rust backend anyway, and using a type which
//       is morally equivalent to `Void` assures us that no one can ever try to interact with an unsupported
//       value.
pub enum Unsupported {}
