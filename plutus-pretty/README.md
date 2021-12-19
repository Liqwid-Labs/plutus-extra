# `plutus-pretty`

## What is this?

A collection of functionality related to pretty-printing of Plutus values, both
on and off-chain. For off-chain, we provide support for use of 
[`pretty-show`](https://hackage.haskell.org/package/pretty-show-1.10) with
Plutus types. This is a workaround for some types not having stock-derived
`Show` instances, and also contains some helpers for working with `pretty-show`.

For on-chain, we provide a separate pretty-printing solution, based around a
structural representation and a type class to convert values into it. To ensure
safety and correctness, we only allow a Template Haskell-based derivation for
instances of this type class. The output of the pretty-printer is JSON, as it's
designed for debugging.

## What can this do?

We currently provide functions to convert various Plutus types to `Value`; see
the `Plutus.V1.Pretty` module for details. We also provide two helper type
classes (in a similar spirit to `Eq1` and such) in `Text.Show.Pretty.Extra`. We
also provide a type `Skeleton`, which is an on-chain structural representation
of a type's values, along with a type class `Skeletal` for converting values
into a `Skeleton`. This is used to support several extended `trace` functions to
work with pretty-printed values on-chain.

## How on earth do I use the `Skeleton` and `Skeletal` stuff?

First, define `Eq` for your type, and use `makeSkeletal`:

```haskell
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}

import PlutusTx.Prelude
import PlutusTx.Skeleton (makeSkeletal)

data Foo = Bar {
    baz :: Value,
    quux :: Integer,
    frob :: [BuiltinString]
 }

instance Eq Foo where ...

$(makeSkeletal ''Foo)
```

Then, use the functions in `PlutusTx.Skeleton` to either produce `BuiltinString`
representations of `Foo` values, or trace them if you wish.

## What are the goals of this project?

### Convenience

Pretty-printing has a range of uses: debugging and user interfaces are the
primary concerns. We want to make this as easy as possible for Plutus types,
both on and off-chain.

### Readability

Pretty-printing is useless if the result is not human-readable, or not _easily_
so. Our goal is to make sure that anything that can be rendered from Plutus is,
and to make it as easy as possible.

### Good ecosystem citizenship

For the off-chain solution, we want to be as integrated into `pretty-show` as we
can: if you know how to use `pretty-show`, everything should Just Work. For
on-chain, we want to provide a usable and familiar interface, which doesn't run
into the limits of Plutus, and tries to be minimally intrusive.

## What can I do with this?

The code is licensed under Apache 2.0; check the LICENSE.md file for details.
