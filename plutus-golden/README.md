# `plutus-golden`

## What is this?

A framework for testing serialized representation stability using [golden
testing](https://ro-che.info/articles/2017-12-04-golden-tests). This focuses on
type classes used for serialization for Plutus projects.

## What can this do?

Currently, we can perform golden testing of the following serializing type
classes:

* `ToJSON` from `aeson`
* `ToData` from Plutus
* `ToSchema` from `openapi3`

For the first two, we support either using an `Arbitrary` instance for
generating samples, or, if you prefer, you can supply an explicin `Gen` instead.

## What are the goals of this project?

### Convenience

Golden tests can be a bit awkward to set up, as most frameworks for it are
general and unopinionated to the point of uselessness. This forces you to do a
lot of manual plumbing and setup, even if most of this generality is not needed.
`plutus-golden`, due to its narrower focus, aims to avoid _all_ of this - just
specify what types you want tested, and what type classes to test, and you're
all set.

This is partly what drives the decision to piggy-back off of `Arbitrary`; due to
Plutus' support for QuickCheck, it's likely you'll already have such instances
lying around, especially if you also want to do property testing.

### Good failure reporting

Knowing that a test failed is often far less useful than knowing _why_ it
failed. This is true of golden testing especially, as you need to figure out
what exactly changed in the representation. To this end, we aim to give focused
and useful information about what we expected to see, versus what we actually
saw, and present it as readably as the type class being checked allows.

This also extends to the format used for sample files: we use JSON throughout,
as this aids human-readability and manual inspection.

### Integration with `tasty`

The `tasty` test framework supports a range of test types, is extensible,
scriptable, and provides a lot of solutions to 'boilerplate' around testing.
`plutus-golden` aims to be a 'good citizen' of the `tasty` universe as much as
possible, allowing integration between itself and other uses of `tasty` without
issue.

## How can I use this?

```haskell
> {-# LANGUAGE TypeApplications #-}
>
> import Test.Tasty (testGroup)
> import Test.Tasty.Plutus.Golden (goldenJSON, goldenDataWith, goldenToSchema)
> 
> myGoldenTests :: TestTree
> myGoldenTests = testGroup "Golden tests" [
>   goldenJSON @MyType,
>   goldenDataWith myGenerator @MyOtherType,
>   goldenToSchema @MyType,
>   ...

## What can I do with this?

The code is licensed under Apache 2.0; check the LICENSE.md file for details.
