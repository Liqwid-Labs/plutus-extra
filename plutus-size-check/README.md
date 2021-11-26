# `plutus-size-check`

## What is this?

A `tasty`-based utility for checking the on-chain sizes of anything the Plutus
compiler can compile. 

## So what can this do?

We provide a convenient interface for testing whether a given entity (whether a
data type value, function or script) can fit on-chain at all (given current
limits), or into a specific size (in bytes or kibibytes). This is wrapped for
use with the `tasty` test framework.

## What are the goals of this project?

### Convenience

The limits on sizing imposed by the Plutus framework are a significant
limitation, and often need to be worked around. This can be somewhat difficult
to check without jumping through some hoops, and is especially difficult if the
goal is optimization and benchmarking. 

We aim to make testing sizes as easy as possible, by providing a tidy and
straightforward interface.

### Good reporting

We try to report sizes using human-understandable units, and make it easy to see
which things have failed and why. Even on a successful test, we still output the
size so that you can have some knowledge of how big your on-chain entities are.

### Integration with `tasty`

We aim to be good citizens of the `tasty` universe. If you can use `tasty`, you
should be able to use `plutus-size-check` too.

## How do I use this?

See the `Test.Tasty.Plutus.Size` module Haddocks for examples (and caveats) of
use.

## What can I do with this?

The code is licensed under Apache 2.0; check the LICENSE.md file for details.
