# Revision history for `tasty-plutus`

This format is based on [Keep A Changelog](https://keepachangelog.com/en/1.0.0)

## Unreleased

### Added

* `paysLovelaceToPubKey`, `paysLovelaceToWallet`.
* Minting validator support and functions.
* `TestData` type for testing API.
* Multiple `tasty` options.

### Changed

* `tagged` is now `datum` to reflect its more general purpose.
* 'Construction' functions in `Context` are now polymorphic over
  `Purpose`s.
* `InputType` and `OutputType` unified into `ExternalType`.
* Testing API now supports minting, and has been reworked with that in mind.
* Significant modifications to the entire testing API to use Plutus' own CEK
  interpreter.

### Removed

* `ForCertifying` and `ForRewarding` constructors of `Purpose`. We have no plans
  to support these.
* `spendsFromSelf`, as self-spending is now handled differently.
* `compile`, as this is now handled purely internally.
* `DecodeFailure`, as this condition no longer exists.
* Dependencies on `validation` and `witherable`.

## 2.0 -- 2021-09-14

### Changed

* Better rendering of record-like things.
* Depend on `pretty-show`.

### Removed

* `Pretty` instances, as we now use `pretty` instead of `prettyprinter`.

### Removed

* All `Pretty` instances.

## 1.0 -- 2021-09-08

* Initial release.
