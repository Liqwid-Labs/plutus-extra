# `plutus-extra`

## What is this?

A collection of Plutus-related helper libraries. Currently, we have:

* `plutus-collection`, for collections-related functionality;
* `plutus-context-builder`, which allows construction of `ScriptContext`s in a
  programmatic way;
* `plutus-deriving`, allowing TH derivations of certain Plutus type classes;
* `plutus-extra`, a collection of various extra bits;
* `plutus-golden`, providing golden test support for various Plutus
  serialization type classes;
* `plutus-laws`, containing property-based test helpers for various type class
  laws, both for Plutus and for some of our extensions;
* `plutus-numeric`, a range of extensions to Plutus' numerical hierarchy;
* `plutus-pretty`, a collection of helpers for pretty-printing Plutus types;
* `plutus-size-check`, allowing for simple checks of on-chain size;
* `quickcheck-plutus-instances`, providing a range of QuickCheck-related
  instances for Plutus types; and
* `tasty-plutus`, a testing framework for Plutus scripts.

See the relevant sub-directories for more specific information.

## How do I use this?

This repo consists of several libraries, with similar build instructions. To
build any of them, you will need Nix, version 2.4 or later. The easiest way to 
build any of the sub-libraries is to use `nix develop`, followed by 
`cabal new-build` in the relevant sub-directory. You will need to enable 
`nix-command` and `flakes` to do this; either pass 
`--extra-experimental-features nix-command --extra-experimental-features flakes`
to the `nix` call, or put the following in your `nix.conf`:

```
experimental-features = nix-command flakes
```

If you want to use any component as a dependency, you will need to ensure that
your commit of Plutus matches ours (or you _will_ have trouble). Check
`cabal.project` to be sure. We follow the [PVP](https://pvp.haskell.org), and
will bump major versions whenever we update our Plutus dependencies.

## Can I use this with Plutarch?

You can try, but we don't promise it will work. Some of the sublibraries may
work, but others are unlikely to: for example, `plutus-numeric` will basically
not help you at all when it comes to Plutarch.

## What can I do with this?

The code is licensed under Apache 2.0; check the LICENSE.md file for details.
