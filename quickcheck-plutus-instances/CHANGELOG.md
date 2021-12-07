# Revision history for `quickcheck-plutus-instances`

This format is based on [Keep A Changelog](https://keepachangelog.com/en/1.0.0).

## Unreleased

## 1.2 -- 2021-11-23

### Added

* Instances of `Arbitrary`, `CoArbitrary` and `Function` for `AssetClass`,
  `BuiltinString`.

## 1.1 -- 2021-11-16

### Added

* Instances of `Arbitrary`, `CoArbitrary` and `Function` for
  `Value`, `Address`, `Credential`, `TxOut`, `StakingCredential`. 
* Instances of `Arbitrary`, `Arbitrary1`, `Arbitrary2`, `CoArbitrary` and
  `Function` for `PlutusTx.AssocMap.Map`.
* `UniqueMap` wrapper around `AssocMap.Map` which ensures unique keys.
* `UniqueList` wrapper around lists to ensure unique values.
* `NonNegative` wrapper for generating non-negative (Plutus) numerical values.

### Changed

* Fix instances for `TxOutRef` so that they respect invariants.
* Fix instances for `Value` to use `UniqueMap` to respect invariants.

## 1.0 -- 2021-11-15

* Initial release.
