# Lambda Buffers

## Introduction

## Documentation

## Getting started

### Installing Nix

This repository relies heavily on the [Nix Package
Manager](https://nixos.org/download.html) for both development and package
distribution.

To install run the following command:

```sh
sh <(curl -L https://nixos.org/nix/install) --daemon
```

and follow the instructions.

```sh
$ nix --version
nix (Nix) 2.8.0
```

> NOTE: The repository should work with Nix version greater or equal to 2.8.0.

Make sure to enable [Nix Flakes](https://nixos.wiki/wiki/Flakes#Enable_flakes)
and IFD by editing either `~/.config/nix/nix.conf` or `/etc/nix/nix.conf` on
your machine and add the following configuration entries:

```yaml
experimental-features = nix-command flakes
allow-import-from-derivation = true
```

Optionally, to improve build speed, it is possible to set up a binary caches
maintained by IOHK and Plutonomicon by setting additional configuration entries:

```yaml
substituters = https://cache.nixos.org https://iohk.cachix.org https://cache.iog.io https://public-plutonomicon.cachix.org
trusted-public-keys = cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY= hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ= iohk.cachix.org-1:DpRUyj7h7V830dp/i6Nti+NEO2/nhblbov/8MW7Rqoo= public-plutonomicon.cachix.org-1:3AKJMhCLn32gri1drGuaZmFrmnue+KkKrhhubQk/CWc=
```
