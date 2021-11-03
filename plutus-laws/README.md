# `plutus-laws`

## What is this?

A helper library for checking type class laws, similar in spirit to
[`quickcheck-classes`](https://hackage.haskell.org/package/quickcheck-classes).
Essentially, you specify what type class laws you want checked, and
`plutus-laws` does the rest, while avoiding many pitfalls in doing the same work
manually. We also aim to support several Plutus variants of common type classes
as part of the framework.

## What can this do?

Currently, we can check type class laws for the following:

* `ToJSON` and `FromJSON` (in combination)
* Plutus `ToData`, `FromData` and `UnsafeFromData` (in combination)
* Plutus `Eq`
* Plutus `Ord`
* Plutus `Semigroup`
* Plutus `Monoid`

We also ensure that some of these (specifically `Eq` and `Ord`) are checked in a
way that either ensures coverage issues don't arise, or at least warns when they
do. This requires no manual effort on the part of the user - only a choice
between functions. We also provide both automatic (via `Arbitrary`) and manual
specification of generators and shrinkers to use while testing the laws.

As this wraps `tasty-quickcheck`, we also support any options that
`tasty-quickcheck` supports, such as for controlling how many tests get run.
Lasty, we supply some helpers for writing your own laws and tests.

## What are the goals of this project?

### Convenience

Property testing and assuring that type class laws are followed are both
non-trivial tasks, and often require skills and experience that not everyone
has. Furthermore, acquiring such experience is often not easy. The _combination_
of these two tasks is particularly difficult, as it requires being aware of
multiple concerns, many of which are not well-documented.

`plutus-laws` is designed to not require you to think about any of these, and
ideally be as automatic as possible. Specify the type class laws you want
checked, provide a generator and shrinker, and we do the rest.

### Good coverage

Many type class laws are stated as 'if-then's. This is easy to follow, but
difficult to test, as you must ensure that the precondition both being satisfied
and /not/ being satisfied receives equal treatment: this issue is termed
_coverage_. The 'direct' approach to formalizing such 'if-then' statements as
laws is unlikely to have good coverage, especially for types which are infinite
or have large cardinality: if using QuickCheck yourself, it's up to you to make
sure that you check and control for this.

We want to make this a non-consideration as far as reasonable. While we can't
completely automate this (without having some kind of type-level cardinality
measuring scheme, which is far outside of our scope), we can at least remove the
fiddly low-level details of doing this. Where possible, `plutus-laws` makes this
automatic, and will warn you if you're potentially running into trouble. This at
least means that even _if_ you get the few choices we offer wrong, you won't
need to spend a long time fixing such issues.

### Integration with `tasty`

`tasty` is a meta-framework, allowing combining multiple types of testing inside
of the same suite. We aim to be a 'good citizen' of the `tasty` ecosystem, and
integrate well with the rest.

## How can I use this?

```haskell
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Plutus.Laws (jsonLaws, dataLawsWith)

myLawsTests :: TestTree
myLawsTests = testGroup "Laws" [
  jsonLaws @MyType,
  dataLawsWith myGen myShrinker,
  ...
```

## What can I do with this?

The code is licensed under Apache 2.0; check the LICENSE.md file for details.
