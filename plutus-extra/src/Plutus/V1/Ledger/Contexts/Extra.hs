{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -fno-omit-interface-pragmas
  -fno-specialize #-}

module Plutus.V1.Ledger.Contexts.Extra (
  ownInputValue,
  valueFromScript,
  getOutputDatum,
  findDatumAtOutput,
  findDatumAtInput,
  parseDatum,
  getAllOutputsWithDatum,
  getScriptOutputsWithDatum,
  allDatumsAtInput,
  allDatumsAtOutput,
  runsValidator,
  scriptInputsAt,
  scriptOutputsAt,
  firstInputOfValidator,
) where

--------------------------------------------------------------------------------

import Control.Arrow ((>>>))
import Data.Kind (Type)

--------------------------------------------------------------------------------

import Plutus.V1.Ledger.Address qualified as Address (toValidatorHash)
import Plutus.V1.Ledger.Contexts qualified as Contexts
import Plutus.V1.Ledger.Scripts qualified as Scripts (
  Datum (getDatum),
  DatumHash,
  ValidatorHash,
 )
import Plutus.V1.Ledger.Tx qualified as Tx (TxOut (..))
import Plutus.V1.Ledger.Value qualified as Ledger (Value)
import PlutusTx.Foldable qualified as Foldable
import PlutusTx.Foldable.Extra qualified as Foldable.Extra
import PlutusTx.IsData.Class qualified as PlutusTx (FromData (..))
import PlutusTx.Prelude

--------------------------------------------------------------------------------

{-# INLINEABLE ownInputValue #-}

{- | Gets the Value locked by the input currently being validated
 returns zero in the case of ill-typed ScriptContext
-}
ownInputValue :: Contexts.ScriptContext -> Ledger.Value
ownInputValue ctx =
  maybe zero (Contexts.txOutValue . Contexts.txInInfoResolved) $ Contexts.findOwnInput ctx

{-# INLINEABLE valueFromScript #-}

-- | Get the total value locked by the script with the specified ValidatorHash
valueFromScript :: Contexts.TxInfo -> Scripts.ValidatorHash -> Ledger.Value
valueFromScript Contexts.TxInfo {txInfoInputs = txInfoInputs} script =
  foldMap (valueAddr . Contexts.txInInfoResolved) txInfoInputs
  where
    valueAddr :: Tx.TxOut -> Ledger.Value
    valueAddr
      Tx.TxOut
        { txOutAddress = Address.toValidatorHash -> Just addr
        , txOutValue = txOutValue
        } | addr == script = txOutValue
    valueAddr _ = mempty

{-# INLINEABLE getOutputDatum #-}

{- | Get the datum attached to the transaction output with the specified datumHash

  This is useful to extract full Datums from the 'txOutDatumHash' provided by the TxOut record
-}
getOutputDatum ::
  forall (a :: Type).
  PlutusTx.FromData a =>
  Contexts.TxInfo ->
  Scripts.DatumHash ->
  Maybe a
getOutputDatum txInfo datumHash =
  Contexts.findDatum datumHash txInfo
    >>= (Scripts.getDatum >>> PlutusTx.fromBuiltinData @a)

--------------------------------------------------------------------------------

{- | Find the given datum type from the inputs or outputs at the address
     Use findDatumAtOutputs or findDatumAtInputs instead of this function!:
     False = Inputs, True = Outputs.
-}
{-# INLINEABLE findDatumAt #-}
findDatumAt ::
  forall (datum :: Type).
  (PlutusTx.FromData datum) =>
  Bool ->
  Contexts.TxInfo ->
  Scripts.ValidatorHash ->
  Maybe datum
findDatumAt
  output
  -- Fix "Could not deduce (GHC.Records.HasField)" when using RDS
  txInfo@Contexts.TxInfo {txInfoInputs = txInfoInputs, txInfoOutputs = txInfoOutputs}
  script =
    Foldable.Extra.firstJust findDatum outputs
    where
      outputs :: [Tx.TxOut]
      outputs =
        if output
          then txInfoOutputs
          else Contexts.txInInfoResolved <$> txInfoInputs

      findDatum :: Tx.TxOut -> Maybe datum
      findDatum
        Tx.TxOut
          { txOutAddress = Address.toValidatorHash -> Just scriptHash
          , txOutDatumHash = Just dhash
          }
          | scriptHash == script =
            Contexts.findDatum dhash txInfo >>= (PlutusTx.fromBuiltinData @datum . Scripts.getDatum)
      findDatum _ = Nothing

-- | Find the given datum type from the outputs at the address
{-# INLINEABLE findDatumAtOutput #-}
findDatumAtOutput ::
  forall (datum :: Type).
  (PlutusTx.FromData datum) =>
  Contexts.TxInfo ->
  Scripts.ValidatorHash ->
  Maybe datum
findDatumAtOutput = findDatumAt True

-- | Find the given datum type from the inputs at the address
{-# INLINEABLE findDatumAtInput #-}
findDatumAtInput ::
  forall (datum :: Type).
  (PlutusTx.FromData datum) =>
  Contexts.TxInfo ->
  Scripts.ValidatorHash ->
  Maybe datum
findDatumAtInput = findDatumAt False

--------------------------------------------------------------------------------

{-# INLINEABLE allDatumsAt #-}

{- | Find all the Datums of a given type at the input/output of a script
     Use allDatumsAtOutput or allDatumsAtInput instead of this function!:
     False = Inputs, True = Outputs.
-}
allDatumsAt ::
  forall (datum :: Type).
  (PlutusTx.FromData datum) =>
  Bool ->
  Contexts.TxInfo ->
  Scripts.ValidatorHash ->
  [datum]
allDatumsAt
  output
  txInfo@Contexts.TxInfo {txInfoInputs, txInfoOutputs}
  script =
    mapMaybe findDatum outputs
    where
      outputs :: [Tx.TxOut]
      outputs =
        if output
          then txInfoOutputs
          else Contexts.txInInfoResolved <$> txInfoInputs

      findDatum :: Tx.TxOut -> Maybe datum
      findDatum
        Tx.TxOut
          { txOutAddress = Address.toValidatorHash -> Just scriptHash
          , txOutDatumHash = Just dhash
          }
          | scriptHash == script =
            Contexts.findDatum dhash txInfo >>= (PlutusTx.fromBuiltinData @datum . Scripts.getDatum)
      findDatum _ = Nothing

-- | Find all the Datums of a given type at the output of a script
{-# INLINEABLE allDatumsAtOutput #-}
allDatumsAtOutput ::
  forall (datum :: Type).
  (PlutusTx.FromData datum) =>
  Contexts.TxInfo ->
  Scripts.ValidatorHash ->
  [datum]
allDatumsAtOutput = allDatumsAt True

-- | Find all the Datums of a given type at the input of a script
{-# INLINEABLE allDatumsAtInput #-}
allDatumsAtInput ::
  forall (datum :: Type).
  (PlutusTx.FromData datum) =>
  Contexts.TxInfo ->
  Scripts.ValidatorHash ->
  [datum]
allDatumsAtInput = allDatumsAt False

{-# INLINEABLE parseDatum #-}

{- | Helper function to parse a UTXO's datum and keep the UTXO

  @since 1.2
-}
parseDatum ::
  forall (datum :: Type).
  (PlutusTx.FromData datum) =>
  Contexts.TxInfo ->
  Contexts.TxOut ->
  Maybe (Contexts.TxOut, datum)
parseDatum txInfo out = do
  dh <- Contexts.txOutDatumHash out
  datum <- Scripts.getDatum <$> Contexts.findDatum dh txInfo
  typedDatum <- PlutusTx.fromBuiltinData datum
  return (out, typedDatum)

{-# INLINEABLE getAllOutputsWithDatum #-}

{- | Get a list of pairs (utxo, datum) consisting of outputs
     whose datums succeded to parse as the passed `datum`
     type and those datums themselves
     that go to any address

  @since 1.2
-}
getAllOutputsWithDatum ::
  forall (datum :: Type).
  (PlutusTx.FromData datum) =>
  Contexts.ScriptContext ->
  [(Contexts.TxOut, datum)]
getAllOutputsWithDatum
  Contexts.ScriptContext {scriptContextTxInfo = txInfo} =
    mapMaybe (parseDatum txInfo) (Contexts.txInfoOutputs txInfo)

{-# INLINEABLE getScriptOutputsWithDatum #-}

{- | Get a list of pairs (utxo, datum) consisting of outputs
     whose datums succeded to parse as the passed `datum`
     type and those datums themselves
     that go to the script address

  @since 1.2
-}
getScriptOutputsWithDatum ::
  forall (datum :: Type).
  (PlutusTx.FromData datum) =>
  Contexts.ScriptContext ->
  [(Contexts.TxOut, datum)]
getScriptOutputsWithDatum
  sc@Contexts.ScriptContext {scriptContextTxInfo = txInfo} =
    mapMaybe (parseDatum txInfo) (Contexts.getContinuingOutputs sc)

--------------------------------------------------------------------------------

{-# INLINEABLE runsValidator #-}

-- | Checks that a validator is run by a transaction
runsValidator :: Contexts.TxInfo -> Scripts.ValidatorHash -> Bool
runsValidator Contexts.TxInfo {txInfoInputs} vHash =
  any (inputFromValidator vHash) txInfoInputs

{-# INLINEABLE inputFromValidator #-}
inputFromValidator :: Scripts.ValidatorHash -> Contexts.TxInInfo -> Bool
inputFromValidator
  vhash
  Contexts.TxInInfo
    { txInInfoResolved = Contexts.TxOut {txOutAddress = Address.toValidatorHash -> Just vhash'}
    } = vhash == vhash'
inputFromValidator _ _ = False

--------------------------------------------------------------------------------

{-# INLINEABLE scriptInputsAt #-}

-- | Get the DatumHashes and Values from the transaction inputs from the given script
scriptInputsAt :: Scripts.ValidatorHash -> Contexts.TxInfo -> [(Scripts.DatumHash, Ledger.Value)]
scriptInputsAt script Contexts.TxInfo {txInfoInputs} =
  mapMaybe scriptInput txInfoInputs
  where
    scriptInput :: Contexts.TxInInfo -> Maybe (Scripts.DatumHash, Ledger.Value)
    scriptInput
      Contexts.TxInInfo
        { txInInfoResolved =
          Contexts.TxOut
            { txOutAddress = Address.toValidatorHash -> Just script'
            , txOutValue = value
            , txOutDatumHash = Just hash
            }
        } | script == script' = Just (hash, value)
    scriptInput _ = Nothing

-- | Get the DatumHashes and Values from the transaction inputs from the given script
scriptOutputsAt :: Scripts.ValidatorHash -> Contexts.TxInfo -> [(Scripts.DatumHash, Ledger.Value)]
scriptOutputsAt script Contexts.TxInfo {txInfoOutputs} =
  mapMaybe scriptOutput txInfoOutputs
  where
    scriptOutput :: Contexts.TxOut -> Maybe (Scripts.DatumHash, Ledger.Value)
    scriptOutput
      Contexts.TxOut
        { txOutAddress = Address.toValidatorHash -> Just script'
        , txOutValue = value
        , txOutDatumHash = Just hash
        } | script == script' = Just (hash, value)
    scriptOutput _ = Nothing

--------------------------------------------------------------------------------

-- | Returns True if the current input is the first input of the validator
firstInputOfValidator :: Contexts.ScriptContext -> Bool
firstInputOfValidator
  ctx@Contexts.ScriptContext
    { scriptContextTxInfo =
      _txInfo@Contexts.TxInfo {txInfoInputs}
    } =
    (Contexts.txInInfoResolved <$> Foldable.find (inputFromValidator $ Contexts.ownHash ctx) txInfoInputs)
      == (Contexts.txInInfoResolved <$> Contexts.findOwnInput ctx)
