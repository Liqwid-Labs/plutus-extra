{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}

{- | This module provides additional functions for testing Plutus contracts using
 'Plutus.Trace.Emulator.EmulatorTrace' monad.
-}
module Plutus.Contract.Test.Extra (
  walletFundsChangeWithAccumState,
  walletFundsExactChangeWithAccumState,
  valueAtComputedAddress,
  dataAtComputedAddress,
  dataAtComputedAddressWithState,
  utxoAtComputedAddressWithState,
) where

--------------------------------------------------------------------------------

import Control.Foldl qualified as L

--------------------------------------------------------------------------------

import Control.Arrow ((>>>))
import Control.Lens (at, view, (^.))
import Control.Monad (unless)
import Control.Monad.Freer (Eff, Member)
import Control.Monad.Freer.Error (Error)
import Control.Monad.Freer.Reader (ask)
import Control.Monad.Freer.Writer (Writer, tell)
import Data.Foldable (fold)
import Data.Kind (Type)
import Data.List (foldl')
import Data.Map qualified as Map
import Data.Maybe (mapMaybe)
import Data.Row (Row)
import Data.Text.Prettyprint.Doc (Doc, Pretty, colon, indent, line, pretty, viaShow, vsep, (<+>))
import Data.Void (Void)
import Prelude

--------------------------------------------------------------------------------

import Ledger qualified
import Ledger.Ada qualified as Ada
import Ledger.AddressMap (UtxoMap)
import Ledger.AddressMap qualified as AM
import Plutus.Contract.Test (TracePredicate)
import Plutus.Contract.Trace (InitialDistribution, Wallet)
import Plutus.Contract.Types (IsContract (toContract))
import Plutus.Trace.Emulator (ContractInstanceTag)
import PlutusTx (FromData (fromBuiltinData))
import PlutusTx.Prelude qualified as P
import Wallet.Emulator.Chain (ChainEvent (..))
import Wallet.Emulator.Folds (postMapM)
import Wallet.Emulator.Folds qualified as Folds

--------------------------------------------------------------------------------

{- | Check that the funds in the wallet have changed by the given amount, exluding fees.

 @since 0.3.0.0
-}
walletFundsChangeWithAccumState ::
  forall
    (w :: Type)
    (s :: Row Type)
    (e :: Type)
    (a :: Type)
    (contract :: Type -> Row Type -> Type -> Type -> Type).
  ( Monoid w
  , Show w
  , IsContract contract
  ) =>
  -- | The 'IsContract' code
  contract w s e a ->
  -- | The 'ContractInstanceTag', acquired inside the
  -- 'Plutus.Trace.Emulator.EmulatorTrace'
  ContractInstanceTag ->
  -- | The 'Plutus.Contract.Trace.Wallet' running the contract
  Wallet ->
  -- | The function computing final 'Ledger.Value' held by the contract
  (w -> Ledger.Value) ->
  TracePredicate
walletFundsChangeWithAccumState = walletFundsChangeWithAccumStateImpl False

{- | Check that the funds in the wallet have changed by the given amount, including fees.
 This functions allows us to peek into the accumulated state, and make decisions about the
 expected value based on that.

 @since 0.3.0.0
-}
walletFundsExactChangeWithAccumState ::
  forall
    (w :: Type)
    (s :: Row Type)
    (e :: Type)
    (a :: Type)
    (contract :: Type -> Row Type -> Type -> Type -> Type).
  ( Monoid w
  , Show w
  , IsContract contract
  ) =>
  -- | The 'IsContract' code
  contract w s e a ->
  -- | The 'ContractInstanceTag', acquired inside the
  -- 'Plutus.Trace.Emulator.EmulatorTrace'
  ContractInstanceTag ->
  -- | The 'Plutus.Contract.Trace.Wallet' running the contract
  Wallet ->
  -- | The function computing final 'Ledger.Value' held by the contract
  (w -> Ledger.Value) ->
  TracePredicate
walletFundsExactChangeWithAccumState = walletFundsChangeWithAccumStateImpl True

{- | Internal implementation for fund checking
 Combining the implementations of walletFundsChangeImpl and assertAccumState functions

  @since 0.3.0.0
-}
walletFundsChangeWithAccumStateImpl ::
  forall
    (w :: Type)
    (s :: Row Type)
    (e :: Type)
    (a :: Type)
    (contract :: Type -> Row Type -> Type -> Type -> Type).
  ( Monoid w
  , Show w
  , IsContract contract
  ) =>
  -- | Is the computed 'Ledger.Value' takes fees into account
  Bool ->
  -- | The 'IsContract' code
  contract w s e a ->
  -- | The 'ContractInstanceTag', acquired inside the
  -- 'Plutus.Trace.Emulator.EmulatorTrace'
  ContractInstanceTag ->
  -- | The 'Plutus.Contract.Trace.Wallet' running the contract
  Wallet ->
  -- | The function computing final 'Ledger.Value' held by the contract
  (w -> Ledger.Value) ->
  TracePredicate
walletFundsChangeWithAccumStateImpl exact contract inst wallet toDlt =
  flip
    Folds.postMapM
    ( (,,) <$> L.generalize (Folds.walletFunds wallet)
        <*> L.generalize (Folds.walletFees wallet)
        <*> Folds.instanceAccumState (toContract contract) inst
    )
    $ \(finalValue', fees, w) -> do
      dist <- ask @InitialDistribution
      let dlt = toDlt w
          initialValue = fold (dist ^. at wallet)
          finalValue = finalValue' P.+ if exact then mempty else fees
          result = initialValue P.+ dlt == finalValue
      unless result $ do
        tell @(Doc Void) $
          vsep $
            [ "Expected funds of" <+> pretty wallet <+> "to change by"
            , " " <+> viaShow dlt
            ]
              ++ [ "  (excluding" <+> viaShow (Ada.getLovelace (Ada.fromValue fees))
                  <+> "lovelace in fees)"
                 | not exact
                 ]
              ++ ( if initialValue == finalValue
                    then ["but they did not change"]
                    else ["but they changed by", " " <+> viaShow (finalValue P.- initialValue)]
                 )
              ++ [ "with accumulated state of" <+> pretty inst <> colon
                 , indent 2 (viaShow w)
                 ]
      pure result

{- | Check that the funds at a computed address meet some condition.
 The address is computed using data acquired from contract's writer instance.

  @since 1.1
-}
valueAtComputedAddress ::
  forall
    (w :: Type)
    (s :: Row Type)
    (e :: Type)
    (a :: Type)
    (contract :: Type -> Row Type -> Type -> Type -> Type).
  ( Monoid w
  , IsContract contract
  ) =>
  -- | The 'IsContract' code
  contract w s e a ->
  -- | The 'ContractInstanceTag', acquired inside the
  -- 'Plutus.Trace.Emulator.EmulatorTrace'
  ContractInstanceTag ->
  -- | The function computing 'Ledger.Address'
  (w -> Maybe Ledger.Address) ->
  -- | The 'Ledger.Value' predicate
  (Ledger.Value -> Bool) ->
  TracePredicate
valueAtComputedAddress contract inst addressGetter check =
  utxoAtComputedAddress contract inst addressGetter $ \_ addr utxoMap -> do
    let value = foldMap (Ledger.txOutValue . Ledger.txOutTxOut) utxoMap
        result = check value
    unless result $
      tell @(Doc Void) ("Funds at address" <+> pretty addr <+> "were" <> pretty value)
    return result

{- | Check that the datum at a computed address meet some condition.
 The address is computed using data acquired from contract's writer instance.
-}
dataAtComputedAddress ::
  forall
    (datum :: Type)
    (w :: Type)
    (s :: Row Type)
    (e :: Type)
    (a :: Type)
    (contract :: Type -> Row Type -> Type -> Type -> Type).
  ( FromData datum
  , Monoid w
  , IsContract contract
  ) =>
  -- | The 'IsContract' code
  contract w s e a ->
  -- | The 'ContractInstanceTag', acquired inside the
  -- 'Plutus.Trace.Emulator.EmulatorTrace'
  ContractInstanceTag ->
  -- | The function computing 'Ledger.Address'
  (w -> Maybe Ledger.Address) ->
  -- | The @datum@ predicate
  (datum -> Bool) ->
  TracePredicate
dataAtComputedAddress contract inst addressGetter check =
  utxoAtComputedAddress contract inst addressGetter $ \_ addr utxoMap ->
    let datums = mapMaybe (uncurry $ getTxOutDatum @datum) $ Map.toList utxoMap
        result = any check datums
    unless result $
      tell @(Doc Void)
        ( "Data at address" <+> pretty addr <+> "was"
            <+> foldMap (foldMap pretty . Ledger.txData . Ledger.txOutTxTx) utxoMap
        )
    return result

{- | Check that the datum at a computed address
 and data aquired from contract's writer instance meet some condition.
 The address is computed using data acquired from contract's writer instance.
-}
dataAtComputedAddressWithState ::
  forall
    (datum :: Type)
    (w :: Type)
    (s :: Row Type)
    (e :: Type)
    (a :: Type)
    (contract :: Type -> Row Type -> Type -> Type -> Type).
  ( FromData datum
  , Monoid w
  , Pretty w
  , IsContract contract
  ) =>
  -- | The 'IsContract' code
  contract w s e a ->
  -- | The 'ContractInstanceTag', acquired inside the
  -- 'Plutus.Trace.Emulator.EmulatorTrace'
  ContractInstanceTag ->
  -- | The function computing 'Ledger.Address'
  (w -> Maybe Ledger.Address) ->
  -- | The @datum@ predicate
  (w -> datum -> Bool) ->
  TracePredicate
dataAtComputedAddressWithState contract inst addressGetter check =
  utxoAtComputedAddress contract inst addressGetter $ \w addr utxoMap ->
    let datums = mapMaybe (uncurry $ getTxOutDatum @datum) $ Map.toList utxoMap
          (any (check w) datums)

{- | Check that the UTxO at a computed address
 and data aquired from contract's writer instance meet some condition.
 The address is computed using data acquired from contract's writer instance.

  @since 1.1
-}
utxoAtComputedAddressWithState ::
  forall
    (w :: Type)
    (s :: Row Type)
    (e :: Type)
    (a :: Type)
    (contract :: Type -> Row Type -> Type -> Type -> Type).
  ( Monoid w
  , Pretty w
  , IsContract contract
  ) =>
  contract w s e a ->
  ContractInstanceTag ->
  (w -> Maybe Ledger.Address) ->
  (w -> UtxoMap -> Bool) ->
  TracePredicate
utxoAtComputedAddressWithState contract inst getter check =
  utxoAtComputedAddress contract inst getter $ \w addr utxoMap ->

{- | Similar to 'utxoAtComputedAddress' but continuation have access
 to a data aquired from contract's writer instance.
-}
utxoAtComputedAddress ::
  forall
    (effs :: [Type -> Type])
    (w :: Type)
    (s :: Row Type)
    (e :: Type)
    (a :: Type)
    (contract :: Type -> Row Type -> Type -> Type -> Type).
  ( Member (Error Folds.EmulatorFoldErr) effs
  , Member (Writer (Doc Void)) effs
  , Monoid w
  , IsContract contract
  ) =>
  -- | The 'IsContract' code
  contract w s e a ->
  -- | The 'ContractInstanceTag', acquired inside the
  -- 'Plutus.Trace.Emulator.EmulatorTrace'
  ContractInstanceTag ->
  -- | The function computing 'Ledger.Address'
  (w -> Maybe Ledger.Address) ->
  -- | The continuation function acting as a predicate
  (w -> Ledger.Address -> UtxoMap -> Eff effs Bool) ->
  Folds.EmulatorEventFoldM effs Bool
utxoAtComputedAddress contract inst addressGetter cont =
  flip
    postMapM
    ( (,)
        <$> Folds.instanceAccumState (toContract contract) inst
        <*> L.generalize Folds.chainEvents
    )
    $ \(w, chainEvents) -> do
      case addressGetter w of
        Nothing -> do
          tell @(Doc Void) $ "Could not compute address using the given getter"
          return False
        Just addr -> do
          let step = \case
                TxnValidate _ txn _ -> AM.updateAddresses (Ledger.Valid txn)
                TxnValidationFail Ledger.Phase2 _ txn _ _ -> AM.updateAddresses (Ledger.Invalid txn)
                _ -> id
              am = foldl' (flip step) (AM.addAddress addr mempty) chainEvents
              utxoMap = view (AM.fundsAt addr) am
          cont w addr utxoMap

-- | Get a datum of a given type 'd' out of a Transaction Output.
getTxOutDatum ::
  forall (datum :: Type).
  (FromData datum) =>
  Ledger.TxOutRef ->
  Ledger.TxOutTx ->
  Maybe datum
getTxOutDatum _ (Ledger.TxOutTx _ (Ledger.TxOut _ _ Nothing)) = Nothing
getTxOutDatum _ (Ledger.TxOutTx tx' (Ledger.TxOut _ _ (Just datumHash))) =
  Ledger.lookupDatum tx' datumHash >>= (Ledger.getDatum >>> fromBuiltinData @datum)
