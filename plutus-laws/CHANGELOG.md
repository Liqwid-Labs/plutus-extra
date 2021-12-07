# Revision history for `plutus-golden`

This format is based on [Keep A Changelog](https://keepachangelog.com/en/1.0.0).

## Unreleased

## 2.3 -- 2021-12-07

### Added

* `Laws` newtype for extending the existing laws-based testing.

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
