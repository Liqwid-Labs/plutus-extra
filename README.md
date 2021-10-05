# `plutus-extra`

## What is this?

A collection of Plutus-related helper libraries. Currently, we have:

* `plutus-extra`, a collection of various extra bits; and
* `tasty-plutus`, a testing framework for Plutus scripts.

See the relevant sub-directories for more specific information.

## How do I use this?

This repo consists of several libraries, with similar build instructions. The
easiest way to build any of them is to use `nix-shell`, then `cabal new-build`.
You can run tests in the same way.

If you want to use any component as a dependency, you will need to ensure that
your commit of Plutus matches ours (or you _will_ have trouble). Check
`cabal.project` to be sure. We follow the [PVP](https://pvp.haskell.org), and
will bump major versions whenever we update our Plutus dependencies.

## What can I do with this?

The code is licensed under Apache 2.0; check the LICENSE.md file for details.
