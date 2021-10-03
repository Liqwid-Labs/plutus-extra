# `tasty-plutus`

## What is this?

A collection of utilities for testing Plutus-based code, integrating with the
[`tasty`](https://hackage.haskell.org/package/tasty) testing framework.

## So what can this do?

We currently have the ability to test a `Validator` or `MintingPolicy`, based on
[the techniques described here](https://github.com/input-output-hk/plutus/issues/3360#issuecomment-891643931). 
However, in addition to being more convenient, we use `evaluateScript` along
with custom testing hooks. Since `evaluateScript` uses Plutus's own CEK
evaluator, it is more precise, and will also catch issues of Plutus compilation
(as the tests will refuse to compile).

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

## How do I build this?

We recommend building using Nix, using our configuration. The easiest way to do
this is to use `nix-shell`, then `cabal new-build`. You can run tests in the
same way.

If you want to use `tasty-plutus` as a dependency, you will need to ensure that
the commit of Plutus you are using matches ours (or you _will_ have trouble).
Check `cabal.project` to be sure. We follow the [PVP](https://pvp.haskell.org),
and will bump versions whenever we update our Plutus dependencies.

## What can I do with this?

The code is licensed under Apache 2.0; check the LICENSE.md file for details.
