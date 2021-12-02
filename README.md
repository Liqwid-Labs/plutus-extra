# `plutus-extra`

## What is this?

A collection of Plutus-related helper libraries. Currently, we have:

* `plutus-extra`, a collection of various extra bits;
* `plutus-numeric`, a range of extensions to Plutus' numerical hierarchy;
* `plutus-pretty`, a collection of helpers for pretty-printing Plutus types; and
* `tasty-plutus`, a testing framework for Plutus scripts.

See the relevant sub-directories for more specific information.

## How do I use this?

This repo consists of several libraries, with similar build instructions. To
build any of them, you will need Nix 2.4. The easiest way to build any of the
sub-libraries is to use `nix develop`, followed by `cabal new-build`. You will
need to enable `nix-command` and `flakes` to do this; either pass
`--extra-experimental-features nix-command --extra-experimental-features flakes`
to the `nix` call, or put the following in your `nix.conf`:

```
experimental-features = nix-command flakes
```

If you want to use any component as a dependency, you will need to ensure that
your commit of Plutus matches ours (or you _will_ have trouble). Check
`cabal.project` to be sure. We follow the [PVP](https://pvp.haskell.org), and
will bump major versions whenever we update our Plutus dependencies.

## What can I do with this?

The code is licensed under Apache 2.0; check the LICENSE.md file for details.
