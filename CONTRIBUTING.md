# Developer guidelines

## Versioning and changelogging

Packages in this repository must be versioned using [PVP][pvp] for Haskell and
PureScript packages, and [Semantic Versioning 2.0.0][semver] for other languages.

Most importantly, minor and patch changes must not include any breaking changes:
no entity is removed, and there's no change in type definitions and functionality
of preexisting exported entities. If any of this occurs, a major version must be
bumped. Disregarding this rule can end up in breaking client package updates.

Any changes must be logged in `CHANGELOG.md`, which must comply with [Keep A
Changelog](https://keepachangelog.com/en/1.1.0/) requirements. Each entry should
also provide a link to the GitHub issue and/or Pull Request that corresponds to
the entry.

An example entry is below:

```lang-none
* Something is fixed
  [#123](https://github.com/mlabs/lambda-buffers/issues/123)
```

## Acknowledgements

This document was adapted from [tx-village
`CONTRIBUTING.md`](https://github.com/mlabs-haskell/tx-village/blob/main/CONTRIBUTING.md)'s.
This is a living document that may change and grow to meet the project's needs

[pvp]: https://pvp.haskell.org/
[semver]: https://semver.org/
