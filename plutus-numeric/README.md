# `plutus-numeric`

## What is this?

An extension to the Plutus numerical hierarchy, supporting additional
operations, as well as 'correct by construction' numerical types beyond
`Integer` and `Rational`.

## What can this do?

Currently, we add the following additional numerical types:

* `Natural`, representing non-negative integers; and
* `NatRatio`, representing non-negative ratios.

We also provide a helper type `Parity`, designed to indicate whether a number is
odd or even in a non-Boolean-blind way. We also provide a range of type classes,
describing a range of operations:

* `AdditiveHemigroup`, a generalization of an `AdditiveMonoid` with a
  [monus](https://en.wikipedia.org/wiki/Monus) operation;
* `EuclideanClosed`, a generalization of `Semiring` with a total version of
  [Euclidean division](https://en.wikipedia.org/wiki/Euclidean_division);
* `MultiplicativeGroup`, corresponding to [the same notion
  mathematically](https://en.wikipedia.org/wiki/Multiplicative_group); and
* `IntegralDomain`, corresponding to a constructive generalization of the [same
  mathematical notion](https://en.wikipedia.org/wiki/Integral_domain).

We provide a collection of laws for each of these type classes, as well as
operations based on these laws. Lastly, for convenience, we define a range of
useful type class instances for these new types.

## What are the goals of this project?

### Lawfulness

Type classes must have laws, and those laws should inform what instances are
valid and possible. Given the core nature of any numerical hierarchy, in Plutus
or elsewhere, any laws governing such must be especially well-documented and
thorough. We aim to put the laws first in all our type classes and
implementations: you know _exactly_ what you are getting. Furthemore, we explain
not only what the laws are, but what they mean relative the types we provide, as
well as Plutus types generally.

### Generality

Many numerical operations are general across many kinds of 'number';
mathematically, these notions are used fairly freely. Due to the type system
being more demanding than such 'fast and loose' reasoning allows, we need to
have an exact definition of the 'limits' of any such capability. We aim to work
as close to these limits as possible - if it makes sense for a numerical
operation to be supported, we should have that be possible.

### Correctness by construction

Making invalid states unrepresentable is an important Haskell practice; with it,
you can avoid a whole range of problems before they even arise. We aim to use
correct-by-construction approaches in everything we do, thus avoiding problems
of invalid state at a fundamental level.

## What can I do with this?

The code is licensed under Apache 2.0; check the LICENSE.md file for details.
