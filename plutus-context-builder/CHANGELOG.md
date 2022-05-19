# Revision history for `plutus-context-builder`

This format is based on [Keep A Changelog](https://keepachangelog.com/en/1.0.0).

## Unreleased

## 2.0.1 -- 2022-05-17

### Added

* module `Test.Plutus.ContextBuilder.Internal`, not recommended for end users,
* the `foldBuilt` utility function, and
* `transactionSpending` and `transactionMinting` for testing of whole
  transactions.

## 2.0 -- 2022-03-09

### Added

* `ContextFragment` type, representing the old-style `ContextBuilder`, but
  without names. Names are still handled by `ContextBuilder`.
* `Naming` type, describing whether we are using named sub-contexts or not.
* `named`, `deleteNamed`, `unionNamed`, `alterNamed` for working with named sub-contexts.
* `liftContextFragment` for directly converting a hand-rolled `ContextFragment`
  into an anonymous `ContextBuilder`.
* `liftNamedContextFragment` for directly converting a hand-rolled
  `ContextFragment` into a named `ContextBuilder`.

### Changed

* `ValidatorUTXO` and `TestUTXO` no longer have data type contexts.
* `ContextBuilder` can now be anonymous or named; this is tagged using a
  `Naming`.
* `ContextBuilder` constructor no longer publically exported.
* `cbDatums` renamed `cfDatums`, now a field of `ContextFragment`, returns a 
  `Seq`.
* `cbInputs` renamed `cfInputs`, now a field of `ContextFragment`, returns a
  `Seq`.
* `cbMinting` renamed `cfMinting`, now a field of `ContextFragment`, returns a
  `Seq`.
* `cbOutputs` renamed `cfOutputs`, now a field of `ContextFragment`, returns a
  `Seq`.
* `cbSignatures` renamed `cfSignatures`, now a field of `ContextFragment`,
  returns a `Seq.
* `cbValidatorInputs` renamed `cfValidatorInputs`, now a field of
  `ContextFragment`.
* `cbValidatorOutputs` renamed `cfValidatorOutputs`, now a field of
  `ContextFragment`.
* All 'construction helpers' for `ContextBuilder` now return anonymous
  `ContextFragments`.

## 1.0 -- 2022-02-04

* Initial release.
