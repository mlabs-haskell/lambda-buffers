# Developer guidelines

## Versioning and changelogging

Packages in this repository must be versioned using [PVP][pvp] for Haskell and
PureScript packages, and [Semantic Versioning 2.0.0][semver] for other languages.

Most importantly, minor and patch changes must not include any breaking changes:
no entity is removed, and there's no change in type definitions and functionality
of preexisting exported entities. If any of this occurs, a major version must be
bumped. Disregarding this rule can end up in breaking client package updates.

Any changes must be logged in `CHANGELOG.md`, which must comply with [Keep A
Changelog](https://keepachangelog.com/en/1.1.0/) requirements. Each entry
should also provide a link to the GitHub issue and/or Pull Request that
corresponds to the entry.

An example entry is below:

```lang-none
* Something is fixed
  [#123](https://github.com/mlabs/lambda-buffers/issues/123)
```

## Release flow

In this repository we adopted a flow with multiple release branches, one for
each major version. This means that users of these packages can point to these
branches and use `nix flake update` without having to deal with breaking changes.
These branches must follow the pattern of `v-MAJOR(.MAJOR)` (`v1`, `v2` etc. for
Semantic Versioning, or v1.0 for PVP).

Furthermore, release versions are pushed as `git tags`.

Stable versions should always be pushed to their respective release branches
in a reasonable schedule (weekly or monthly depending on the project).
In most cases this would also mean a release, which requires some additional
manual tasks:

1. bump versions in package manifest files (*.cabal, Cargo.toml, etc.)
2. update lock file if necessary
3. push git tag

## Monorepo versioning policies

If a repository has multiple packages, these can evolve independently and their
release cycle could be different. All changes shall be contained in the
aggregated `CHANGELOG.md` of all packages under the appropriate header.

If a Semantic Versioning and PVP are used simultaneously, the first major
number of PVP must always be 1 (or 0 if in a beta state).

Versioning policies when updating dependencies are well explained in the
[PVP][pvp] and [Semver][semver] documents, the same rules apply to dependencies
inside the same repository. In short, if a dependency update does not have any
effect on the public interface of the upstream package, it can be considered a
minor or patch release.

> As an example if we have three packages `HaskellApp-v1.3.4.1`,
> `HaskellLib-v1.3.0.0` and `RustApp-v0.1.0`, we could bump `HaskellApp` to
> `1.4.0.0` without touching the other two.

## Acknowledgements

This document was adapted from [tx-village
`CONTRIBUTING.md`](https://github.com/mlabs-haskell/tx-village/blob/main/CONTRIBUTING.md)'s.
This is a living document that may change and grow to meet the project's needs

[pvp]: https://pvp.haskell.org/
[semver]: https://semver.org/
