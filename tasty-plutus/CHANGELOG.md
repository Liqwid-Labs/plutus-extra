# Revision history for `tasty-plutus`

This format is based on [Keep A Changelog](https://keepachangelog.com/en/1.0.0).

## 5.0

### Changed

* Rename `paysSelf` and `paysOther` into `paysToSelf` and `paysToOther` for
  consistency.
* Rename `ValidatorTest` to `ScriptTest`.
* Rename `Example` to `Outcome` and its two constructors `Good` and `Bad` to
  `Pass` and `Fail` respectively, unifying with the internal type used in unit
  tests.

### Removed

* Various re-exports from `Test.Tasty.Plutus.Script.Unit`.

## 4.0 -- 2021-11-11

### Changed

* Plutus upgrade: `plutus` pinned to `3f089ccf0ca746b399c99afe51e063b0640af547`, 
  `plutus-apps` pinned to `404af7ac3e27ebcb218c05f79d9a70ca966407c9`

## 3.4 -- 2021-11-03

* Add `ScriptInputPosition` option to control where in `txInfoInputs` the script
  input goes.
* Make `ContextBuilder` a `Monoid`.
* Add `Test.Tasty.Plutus.Script.Size`, containing a script size testing
  interface.

## 3.3 -- 2021-10-26

### Added

* `shouldn'tValidateTracing` and `shouldValidateTracing`, giving an explicit
  handle and predicate on traces.

## 3.2 -- 2021-10-19

### Changed

* Minting-related `ContextBuilder` functionality is now polymorphic in its
  `Purpose`.
* Fix a bug where _expected_ script failures reported as test failures in all
  cases, instead of only when unexpected.

## 3.1 -- 2021-10-14

### Added

* Support for QuickCheck-based property testing.
* Multiple new modules.
* Additional `tasty` options to control property-based execution.
* More examples in documentation.
* Dependency on `plutus-pretty`.
* New option for trace logs even on success for the unit-test-like interface.

### Changed

* Multiple definitions moved to own modules from `Unit`. `Unit` still re-exports
  these temporarily for backwards compatibility.
* Only one `Internal` module containing everything that needs hiding.
* `EvaluationError` no longer causes a test abort.

### Deleted

* `Test.Tasty.Plutus.Context.Internal` module (now merged into
  `Test.Tasty.Plutus.Internal`).

## 3.0 -- 2021-10-04

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
* `WithValidator` is now `WithScript`.
* `Test.Tasty.Plutus.Validator.Unit` is now `Test.Tasty.Plutus.Script.Unit`.
* Plutus bumped to `58c093a49eb7a369865e361179d649264fc817a4`.

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
