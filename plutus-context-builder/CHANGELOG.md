# Revision history for `plutus-context-builder`

This format is based on [Keep A Changelog](https://keepachangelog.com/en/1.0.0).

## Unreleased

## 2.0 -- 2022-03-09

### Added

* `ContextFragment` type, representing the old `ContextBuilder`, but without
  names; those are still handled by `ContextBuilder` as before.
* `named` and `deleteNamed` for working with `ContextBuilder` and naming.

### Changed

* `ValidatorUTXO` and `TestUTXO` no longer have data type contexts.
* `ContextBuilder` now wraps a `Map Text ContextFragment`.
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
* `input`, `output`, `signedWith`, `datum`, `addDatum`, `minting`, 
  `outToPubKey`, `outToPubKeyWithDatum`, `outTokensToPubKey`,
  `outTokensToPubKeyWithDatum`, `outLovelaceToPubKey`,
  `outLovelaceToPubKeyWithDatum`, `outToOtherScript`, 
  `outTokensToOtherScript`, `inFromPubKeyWithDatum`, `inTokensFromPubKey`, 
  `inTokensFromPubKeyWithDatum`, `inLovelacyFromPubKey`,
  `inLovelaceFromPubKeyWithDatum`, `inFromOtherScript`,
  `inTokensFromOtherScript`, `mintedValue` all no longer 
  take a name argument, all return `ContextFragment`.
* `validatorInput`, `validatorOutput`, `outToValidator`, `inFromValidator` now 
  return a `ContextFragment`.

## 1.0 -- 2022-02-04

* Initial release.
