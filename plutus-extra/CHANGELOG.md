# Revision history for plutus-extra

This format is based on [Keep A Changelog](https://keepachangelog.com/en/1.0.0).

## Unreleased

## 4.1 -- 2021-12-14

### Added

- PlutusTx.Data.Extra: `toDatumHash` to get `DatumHash` of a value
- PlutusTx.Set: `Data.OpenApi.Internal.Schema.ToSchema` instance for `Set`

### Changed

- PlutusTx.Data.Extra:
  - `toDatum`, `toRedeemer`, `fromDatum` and `fromRedeemer`
    use _Datum_ and _Redeemer_ types instead of _ValidatorTypes_

## 3.1 -- 2021-12-13

### Added

- Instances of `Eq`, `Ord`, `Arbitrary`, `CoArbitrary`
  and `Function` for `NonEmpty`

### Changed

- `NonEmpty` `Show` instance is derived

## 3.0 -- 2021-11-11

- Plutus upgraded
  - `plutus` pinned to `3f089ccf0ca746b399c99afe51e063b0640af547`
  - `plutus-apps` pinned to `404af7ac3e27ebcb218c05f79d9a70ca966407c9`

## 2.1 -- 2021-10-22

### Added

- `dataAtComputedAddressWithState` function at `Plutus.Contract.Test.Extra`
- `utxoAtComputedAddressWithState` function at `Plutus.Contract.Test.Extra`

## 2.0 -- 2021-10-04

### Changed

* Plutus bumped to `58c093a49eb7a369865e361179d649264fc817a4`.

# Legacy history

## 0.1.0.0 -- YYYY-mm-dd

- First version. Released on an unsuspecting world.

## 0.2.0.0 -- 2021-07-29

- Bump Plutus version to `4551bba`.

## 0.3.0.0 -- 2021-08-20

- Bump Plutus version to `00e7cc3`.
- Addition of new test assertions

## 0.3.0.1 -- 2021-08-27

- Derive `UnsafeFromData` in `UniqueMap`

## 1.0 -- 2021-09-06

### Changed

* Changelog now follows style guide and [Keep A
  Changelog](https://keepachangelog.com/en/1.0.0) conventions.
* Plutus is at commit ``289ac956a33c14fe1d75d6c0a6d6100f96a7d22c``.
* Version pins in Cabal file, general clean-up of unnecessary dependencies and
  clutter.
* Version number(s) fit the style guide now.

## 1.1 -- 2021-09-13
#### Added
* New test assertions in `Plutus.Contract.Test.Extra` for checking UTxOs at
  address computed from the state of `EmulatorTrace`.
