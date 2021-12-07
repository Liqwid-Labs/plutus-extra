# `plutus-deriving`

## What is this?

A helper for deriving lawful instances of Plutus type classes.

## What can this do?

Currently, we can derive `Eq` only. The instances we derive obey the following
laws:

* Reflexivity (for any `x`, `x == x = True`)
* Symmetry (for any `x, y`, `x == y = y == x`)
* Transitivity (for any `x, y, z`, `x == y` and `y == z` implies `x == z`)
* Substitutability (for any `x, y` and pure `f`, `x == y` implies `f x == f y`)

## What are the goals of this project?

### Convenience

For regular Prelude `Eq`, we can get lawful instances very quickly: just `derive
stock Eq` and move on. This is not possible with Plutus `Eq`, as there is no
support for stock derivations of it. Furthermore, methods relying on `Generic`
(or anything similar) won't work on-chain. This forces writing tedious,
error-prone and brittle code for what is arguably extremely basic functionality.
This extends to other type classes as well, such as Plutus `Ord`.

We aim to make this as easy as possible, using the only tool we have at our
disposal (Template Haskell). While we can't be quite as straightforward as a
`deriving stock`, we want to be as close as possible.

### Law abidance

Type classes must have laws, and if we are to generate instances automatically,
our generated instances must obey said laws. This is even more critical in
generated code, as it is harder to debug or inspect. Our instances will obey
the relevant laws, and when there are several possible choices in how they are
obeyed, will make them distinctive.

### Compact code size

Plutus places strict limits on how large any given object can be on-chain: type
class method implementations are no exception. Furthermore, due to the extensive
inlining done by Plutus' compiler, any size excesses are magnified. For type
class methods, especially for 'fundamental' type classes like `Eq`, this matters
significantly, as these methods are used in many places, possibly in a way that
isn't directly visible. Thus, we aim for the smallest code possible being
generated.

## How can I use this?

```haskell
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Foo where
 
import PlutusTx.Prelude
import PlutusTx.Deriving (deriveEq)
import Data.Kind (Type)

data Foo (a :: Type) = 
  Mono BuiltinByteString |
  Poly a |
  Recursive BuiltinByteString (Foo a)

$(deriveEq ''Foo)
```

If you encounter issues with the generated code, you can inspect the output by
adding `{-# OPTIONS_GHC -ddump-splices #-}` to the top of the module where
`deriveEq` is used. This will dump the output of the TH splice during
compilation, so that you can inspect it.

## What can I do with this?

The code is licensed under Apache 2.0; check the LICENSE.md file for details.
