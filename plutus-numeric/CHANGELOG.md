# Revision history for `plutus-numeric`

This format is based on [Keep A Changelog](https://keepachangelog.com/en/1.0.0).

## Unreleased

## 1.1 -- 2021-10-28

### Added

* Hedgehog generators for `Natural` and `NatRatio` in new module
  `PlutusTx.Numeric.Gen`.
* `ToSchema` (from OpenAPI) instance for `NatRatio`.
* Golden tests, based on `plutus-golden`.

## 1.0 -- 2021-10-19

* Initial release.
