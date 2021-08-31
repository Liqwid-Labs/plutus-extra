# plutus-extra

Useful functions and data structures for Plutus projects.

## Project setup

### Pre-requisites

#### Nix

An up to date (2.3 or greater) installation of nix is required run the project's nix shell, which is the reccommended way to acqurie all other dependencies. 

Set up your machine to build things with `Nix`, following the [Plutus README](https://github.com/input-output-hk/plutus/blob/master/README.adoc) (make sure to set up the binary cache!).

#### Non-Library & Non-Haskell Dependencies

There is a number of non-library and non-haskell dependencies required to build the project. These should all be provided by the nix-shell, and this is the reccommended way to install them. 

### How to Build

A [Makefile](./Makefile) is provided with convenient targets for building the project.

Run `make build` to build the project and `make test` to build the project and run the test suite.

## Introduction

### Data structures

The package introduces `PlutusTx.Set` of unique values (running in linear time), `PlutusTx.Map` of key-value pairs, `PlutusTx.Bimap` of pairs, and `PlutusTx.NonEmpty` list which always has at least one element.

### Type classes

`PlutusTx.Bifunctor` behaves like a Functor over both of it's type arguments, e.g.:

```haskell
bimap f g (a, b) == (f a, g b)

bimap f g (Left l)  == Left (f l)
bimap f g (Right r) == Right (g r)
```

### `Extra` functions

The package extends `Plutus.V1.Ledger` and `PlutusTx` with useful library functions placed inside modules ending with `Extra` suffix.

E.g., `PlutusTx.Either.Extra` introduces function `maybeToEither`:

```haskell
import PlutusTx.Either.Extra (maybeToEither)

maybeToEither () Nothing  == Left ()
maybeToEither () (Just x) == Right x
```

### PAB utility functions

Modules in `Plutus.PAB` directory introduces utility functions to be used with the PAB (Plutus Application Backend):

-  `Plutus.PAB.CurrencyForge` exports `initCurrency`, for minting new currency and giving it to a specific wallet,
-  `Plutus.PAB.OutputBus` is a wrapper around the channel transferring information between `Contract` and real world,
-  `Plutus.PAB.PrettyLogger` introduces useful functions for prettying the logs.

