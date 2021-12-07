# Revision history for `plutus-numeric`

This format is based on [Keep A Changelog](https://keepachangelog.com/en/1.0.0).

## Unreleased

## 2.2 -- 2021-11-17

### Added

* `CoArbitrary` and `Function` instances for `Natural` and `NatRatio`.

## 2.1 -- 2021-11-15

### Added

- `natToInteger` and `natRatioToRational` as dedicated (and clearer) aliases for
  `addExtend`.

## 2.0 -- 2021-11-11

### Changed

* Plutus upgrade: `plutus` pinned to `3f089ccf0ca746b399c99afe51e063b0640af547`,
  `plutus-apps` pinned to `404af7ac3e27ebcb218c05f79d9a70ca966407c9`

## 1.1 -- 2021-10-28

### Added

* Hedgehog generators for `Natural` and `NatRatio` in new module
  `PlutusTx.Numeric.Gen`.
* `ToSchema` (from OpenAPI) instance for `NatRatio`.
* Golden tests, based on `plutus-golden`.
* `Prelude.Ord` instances for `Natural` and `NatRatio`.

## 1.0 -- 2021-10-19

* Initial release.
