# `plutus-pretty`

## What is this?

A collection of functionality to support use of
[`pretty-show`](https://hackage.haskell.org/package/pretty-show-1.10) with
Plutus types. This is a workaround for some types not having stock-derived
`Show` instances, and also contains some helpers for working with `pretty-show`.

## What can this do?

We currently provide functions to convert various Plutus types to `Value`; see
the `Plutus.V1.Pretty` module for details. We also provide two helper type
classes (in a similar spirit to `Eq1` and such) in `Text.Show.Pretty.Extra`.

## What are the goals of this project?

We aim to provide manual `Value` conversions for those types which we need at
the moment, as well as helpers which make this easier, again, on an as-needed
basis. We may expand this in future to include more data types from Plutus if we
find we need them, as well as more instances of our helper type classes, again,
if we need them.

## What can I do with this?

The code is licensed under Apache 2.0; check the LICENSE.md file for details.
