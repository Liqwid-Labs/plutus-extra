# Revision history for `plutus-context-builder`

This format is based on [Keep A Changelog](https://keepachangelog.com/en/1.0.0).

## Unreleased

## 2.0 -- 2022-03-09

### Changed

* `ValidatorUTXO` and `TestUTXO` no longer have data type contexts.
* `ContextBuilder`'s `cbSignatures` now allows duplicate entries for the same
  key.
* `ContextBuilder`'s `<>` now keeps all signatures from both 'sides' of the
  operator.

## 1.0 -- 2022-02-04

* Initial release.
