# Revision history for `plutus-laws`

This format is based on [Keep A Changelog](https://keepachangelog.com/en/1.0.0).

## Unreleased

## 3.0 -- 2022-01-21

### Changed
  
* Plutus upgrade: `plutus` pinned to `65bad0fd53e432974c3c203b1b1999161b6c2dce`, 
  `plutus-apps` pinned to `34fe6eeff441166fee0cd0ceba68c1439f0e93d2`

## 2.4 -- 2022-01-11

### Added

* Laws for `Module s v` 

## 2.3 -- 2021-12-08

### Added

* `Laws` newtype for extending the existing laws-based testing.
* `laws` and `lawsWith` for running custom `Laws`-based tests.

## 2.2 -- 2021-12-01

### Added

* Laws for ``AdditiveSemigroup``, ``AdditiveMonoid``,
  ``MultiplicativeSemigroup``, ``MultiplicativeMonoid`` and ``AdditiveGroup``.
* Laws for checking consistency across the component constraints of `Semiring`.

## 2.1 -- 2021-11-16

### Added

* Substitution testing for Plutus `Eq`.
* `PA` type as a Plutus version of QuickCheck's `A`.

## 2.0 -- 2021-11-11

### Changed

* Plutus upgrade: `plutus` pinned to `3f089ccf0ca746b399c99afe51e063b0640af547`,
  `plutus-apps` pinned to `404af7ac3e27ebcb218c05f79d9a70ca966407c9`

## 1.0 -- 2021-11-03

* Initial release.
