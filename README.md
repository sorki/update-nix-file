# update-nix-file

**PoC**


Based on [update-nix-fetchgit](https://github.com/expipiplus1/update-nix-fetchgit)

## Build

Use [hnix-overlay](https://github.com/sorki/hnix-overlay) and

```bash
nix-build -A update-nix-file
```

## Package breakage

`setBreakage` function allows to manipulate `meta.broken`
attributes of `.nix` files programmatically. Currently it doesn't `eval`
`nixpkgs` directly but uses `AST` to traverse the package set following
`callPackage` calls.

This will be further generalized to arbitrary `.nix` file
manipulation operations like adding, removing or changing attributes.

### Command line usage

```bash
update-nix-file break zzuf --dry
update-nix-file unbreak zzuf
```

See `update-nix-file --help` for more.

### `cabal repl` usage

```haskell
import Update.Nix.File

> setBreakage True "zzuf"
-- will try to perform file operation

> break "zzuf"
> unbreak "zzuf"
-- are shorthands for setBreakage

> dbg = True
> dryRun = True
> setBreakage' dbg dryRun True "zzuf"
-- like setBreakage with debugging output
-- and dry-run enabled which makes it not to touch
-- files and use stdout instead
```


### Other bits

Until they have better home.

```bash
update-nix-file find-file nixpkgs
# evals <nixpkgs> location according to NIX_PATH
update-nix-file find-file nixpkgs/lib
# e.g. /etc/nixpkgs/lib/default.nix

update-nix-file find zzuf
# /etc/nixpkgs/pkgs/tools/security/zzuf/default.nix:19

update-nix-file find haskellPackages.zre
/etc/nixpkgs/pkgs/development/haskell-modules/hackage-packages.nix:271703
```

