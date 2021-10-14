# `tasty-plutus`

## What is this?

A framework for testing Plutus scripts, integrating with 
[`tasty`](https://hackage.haskell.org/package/tasty).

## So what can this do?

We currently have the ability to test either a `Validator` or a `MintingPolicy`,
by use of `evaluateScript` from Plutus with custom testing hooks. Since
`evaluateScript` uses Plutus' own CEK evaluator, it is more precise, and will
also catch issues of Plutus compilation (as the tests will refuse to compile if
there are any such issues).

Specifically, we provide two kinds of tests:

* A 'unit-test-like' interface, based on [the techniques described here](https://github.com/input-output-hk/plutus/issues/3360#issuecomment-891643931).
* A property testing interface, using
  [QuickCheck](https://hackage.haskell.org/package/QuickCheck) as a basis.

These are not mutually exclusive - you can use one, the other, or both, in the
same set of tests. We also provide a range of options (using `tasty`'s option
interface) to control various aspects of the tests.

## What are the goals of this project?

### Convenience

Testing Plutus-based code can be a significant challenge. `tasty-plutus`, by a
mixture of good integration with the `tasty` ecosystem and well-chosen testing
APIs, aims to make it more straightforward. In particular, we want to codify
folklore practices or discoveries and make them accessible to everyone.

### Good failure reporting

Knowing that a test failed is often far less useful than knowing _why_ it
failed. To this end, `tasty-plutus` aims to give the most useful information it
can about the context of the failures, and present it in a readable and concise
way.

### Integration with `tasty`

The `tasty` test framework is well-supported for a range of testing types, is
extensible, scriptable, and provides a lot of solutions to 'boilerplate' around
testing. `tasty-plutus` will use this whenever possible, and in general, try to
be a 'good citizen' of the `tasty` universe.

## Where do I begin?

Start with `Test.Tasty.Plutus.Script.Unit` (for the unit-test-like interface)
and `Test.Tasty.Plutus.Script.Property` (for the property-test-like interface).
You will also need `Test.Tasty.Plutus.WithScript` (to create runnable tests),
`Test.Tasty.Plutus.TestData` and `Test.Tasty.Plutus.Context` (to construct
inputs), and possibly `Test.Tasty.Plutus.Options` if you need fine-grained
control over the `tasty`-compatible options we provide.

## What can I do with this?

The code is licensed under Apache 2.0; check the LICENSE.md file for details.
