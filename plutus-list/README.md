# `plutus-list`

## What is this?

List utility functions that are not present in the Plutus standard library,
including fucntions involving lengths that work with the `Natural` number
type in `plutus-numeric`, as well as functions related to sorting.

## What can this do?

We currently have two modules:

* `PlutusTx.List.Natural` has list functions that work with `Natural`s instead
  of `Integers` like their standard library counterparts. Specifically, this
  means `length`, `replicate`, `take`, `drop`, and `splitAt` that works with
  `Natural`s;
* `PlutusTx.List.Ord` has functions that sort a list, checks list sortedness,
  or deduplicates a list.
  * `sort`, `sortOn` and `sortBy`, but uses a less efficient mergesort algorithm
  to ensure small code size;
  * `isSorted` and `isSortedAscending` for checking a list's sortedness, as well
    as `-On` and `-By` versions for both of the functions;
  * `ordNub` and `ordNubBy` that is like `nub` but has `O(n log n)` time and
    sorts the list along the way.

## What are the goals of this project?

### Compactness

Many list algorithms have fast implementations, but they are often lengthy. This
is undesirable in Plutus; therefore we must find a balance between efficiency
and size of the code. Basically, we do not try to optimize for edge-case and/or
constant time improvements if it introduces much extra code.

### Avoid bad practices

We avoid partial functions and inefficient algorithms; their usage can almost
always be replaced by better solutions. This is why functions like `head`,
`tail`, as well as `nub` (use `ordNub` instead) are not put in.

### Interaction with `plutus-numeric`

We use `plutus-numeric` classes and types whenever possible; they are often more
precise than standard library number types, which provides more correctness by
construction.

## What can I do with this?

The code is licensed under Apache 2.0; check the LICENSE.md file for details.
