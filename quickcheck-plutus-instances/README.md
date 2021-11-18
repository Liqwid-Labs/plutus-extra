# `quickcheck-plutus-instances`

## What is this?

A collection of instances for Plutus types to support general use of QuickCheck.

## What can this do?

Currently, where possible, we define instances of the following type classes:

* `Arbitrary`, `Arbitrary1` and `Arbitrary2`
* `CoArbitrary`
* `Function`

We generally avoid using `Generics`-based derivations, as these instances
compile slowly, run slowly, and frequently end up violating (often undocumented)
invariants; instead, our instances are hand-written.

## What are the goals of this project?

### Low dependencies

We aim not to depend on too many packages, other than what is minimally needed
for Plutus, as well as QuickCheck itself. If your project doesn't need it,
neither do we.

### High-quality instances

Writing correct instances of many QuickCheck type classes is difficult:
shrinkers in particular are hard to get right. `CoArbitrary` and `Function` both
enable some very capable tests, but writing instances for them is even more
difficult, has less support and documentation, and thus is even less commonly
done. To make matters even worse, Plutus types often have undocumented
invariants, which can often make instances you write yourself subtly wrong in
hard-to-detect ways.

We aim to solve this problem once and for all. You should be able to use any of
our types with confidence, knowing that we have solved all of these issues; the
only way they can creep in would be in code that _you_ wrote, rather than in
stuff you assume is correct.

### Wide range of capabilities

QuickCheck provides some significantly powerful testing capabilities, especially
when combined with certain helper libraries. Many of these require instances of
multiple type classes to use at all, or to get useful reporting out of. We aim
to provide as many such instances as is reasonable, both to help use the
capabilities of QuickCheck, and also to help you write the instances you need
for your types more easily.

## How can I use this?

You can get all of the instances we provide with one import:

```haskell
import Test.QuickCheck.Plutus.Instances ()
```

## What can I do with this?

The code is licensed under Apache 2.0; check the LICENSE.md file for details.
