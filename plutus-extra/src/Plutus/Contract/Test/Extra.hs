{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}

{- | This module provides additional functions for testing Plutus contracts using
 'Plutus.Trace.Emulator.EmulatorTrace' monad.
-}
module Plutus.Contract.Test.Extra (
  namedPredicate,
  walletFundsChangeWithAccumState,
  walletFundsExactChangeWithAccumState,
  valueAtComputedAddress,
  dataAtComputedAddress,
  dataAtComputedAddressWithState,
  valueAtComputedAddressWithState,
  utxoAtComputedAddressWithState,
  utxoAtComputedAddress,
  utxoAtAddress,
  addressValueOptions,
  checkPredicateGenAll,
  checkPredicateGenAllShow,
  checkPredicateGenAllShows,
) where

--------------------------------------------------------------------------------

import Control.Foldl qualified as L

--------------------------------------------------------------------------------

import Control.Arrow ((>>>))
import Control.Lens (at, view, (^.))
import Control.Monad (forM_, unless, (>=>))
import Control.Monad.Freer (Eff, Member)
import Control.Monad.Freer.Error (Error)
import Control.Monad.Freer.Reader (Reader, ask)
import Control.Monad.Freer.Writer (Writer, tell)
import Data.Default.Class (Default (def))
import Data.Foldable (fold)
import Data.Kind (Type)
import Data.List (foldl')
import Data.Map qualified as Map
import Data.Maybe (mapMaybe)
import Data.Row (Row)
import Data.Void (Void)
import Prettyprinter (
  Doc,
  Pretty,
  align,
  colon,
  indent,
  pretty,
  viaShow,
  vsep,
  (<+>),
 )
import Prelude

--------------------------------------------------------------------------------

import Control.Foldl (generalize)
import Hedgehog qualified
import Ledger qualified
import Ledger.Ada qualified as Ada
import Ledger.AddressMap (UtxoMap)
import Ledger.AddressMap qualified as AM
import Ledger.Scripts qualified as Scripts
import Plutus.Contract.Test (CheckOptions, TracePredicate, checkPredicateInner)
import Plutus.Contract.Trace (InitialDistribution, Wallet)
import Plutus.Contract.Types (IsContract (toContract))
import Plutus.Trace.Emulator (
  ContractInstanceTag,
  EmulatorConfig (EmulatorConfig),
  EmulatorTrace,
 )
import PlutusTx (FromData (fromBuiltinData))
import PlutusTx.Prelude qualified as P
import Wallet.Emulator.Chain (ChainEvent (..))
import Wallet.Emulator.Folds (EmulatorFoldErr, postMapM)
import Wallet.Emulator.Folds qualified as Folds

--------------------------------------------------------------------------------

{- | Postcompose Kleisli arrow to FoldM.

 @since 4.1
-}
postComposeM ::
  forall (a :: Type) (b :: Type) (c :: Type) (m :: Type -> Type).
  Monad m =>
  (b -> m c) ->
  L.FoldM m a b ->
  L.FoldM m a c
postComposeM q (L.FoldM f g h) = L.FoldM f g (h >=> q)

{- | Give name to a 'TracePredicate'.

 @since 4.1
-}
namedPredicate :: String -> TracePredicate -> TracePredicate
namedPredicate name = postComposeM notify
  where
    notify False = False <$ tell message
    notify b = pure b

    message = "On predicate: " <> viaShow @String @Void name

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

data CheckedState = CheckedState
  { checkedAddress :: Ledger.Address
  , checkedData :: [CheckedData]
  }

data CheckedData where
  CheckedDatas :: UtxoMap -> CheckedData
  CheckedValue :: Ledger.Value -> CheckedData
  CheckedUtxos :: UtxoMap -> CheckedData
  CheckedWriter :: forall w. Show w => w -> CheckedData

instance Pretty CheckedState where
  pretty (CheckedState address datas) =
    "At the address" <+> pretty address <+> ":"
      <+> align (vsep $ map pretty datas)

instance Pretty CheckedData where
  pretty (CheckedDatas utxos) =
    "Data was"
      <+> foldMap (foldMap pretty . Ledger.txData . Ledger.txOutTxTx) utxos
  pretty (CheckedValue value) =
    "Funds was"
      <+> pretty value
  pretty (CheckedUtxos utxos) =
    "UTxO was"
      <+> foldMap viaShow utxos
  pretty (CheckedWriter w) =
    "Contract writer data was"
      <+> viaShow w

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
  utxoAtComputedAddress contract inst addressGetter $ \_ addr utxoMap ->
    let value = foldMap (Ledger.txOutValue . Ledger.txOutTxOut) utxoMap
     in showStateIfFailAndReturn
          [CheckedValue value]
          addr
          (check value)

{- | Check that the funds at a computed address
 and data aquired from contract's writer instance meet some condition.
 The address is computed using data acquired from contract's writer instance.

  @since 5.0
-}
valueAtComputedAddressWithState ::
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
  -- | The function computing 'Ledger.Address'
  (w -> Maybe Ledger.Address) ->
  -- | The @datum@ predicate
  (w -> Ledger.Value -> Bool) ->
  TracePredicate
valueAtComputedAddressWithState contract inst addressGetter check =
  utxoAtComputedAddress contract inst addressGetter $ \w addr utxoMap ->
    let value = foldMap (Ledger.txOutValue . Ledger.txOutTxOut) utxoMap
     in showStateIfFailAndReturn
          [CheckedValue value, CheckedWriter w]
          addr
          (check w value)

{- | Check that the datum at a computed address meet some condition.
 The address is computed using data acquired from contract's writer instance.

  @since 1.1
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
     in showStateIfFailAndReturn
          [CheckedDatas utxoMap]
          addr
          (any check datums)

{- | Check that the datum at a computed address
 and data aquired from contract's writer instance meet some condition.
 The address is computed using data acquired from contract's writer instance.

  @since 5.0
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
  , Show w
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
     in showStateIfFailAndReturn
          [CheckedDatas utxoMap, CheckedWriter w]
          addr
          (any (check w) datums)

showStateIfFailAndReturn ::
  forall effs.
  Member (Writer (Doc Void)) effs =>
  [CheckedData] ->
  Ledger.Address ->
  Bool ->
  Eff effs Bool
showStateIfFailAndReturn datas addr result = do
  unless result $ tell @(Doc Void) $ pretty $ CheckedState addr datas
  pure result

{- | Check that the UTxO at a computed address
 and data aquired from contract's writer instance meet some condition.
 The address is computed using data acquired from contract's writer instance.

  @since 5.0
-}
utxoAtComputedAddressWithState ::
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
  contract w s e a ->
  ContractInstanceTag ->
  (w -> Maybe Ledger.Address) ->
  (w -> UtxoMap -> Bool) ->
  TracePredicate
utxoAtComputedAddressWithState contract inst getter check =
  utxoAtComputedAddress contract inst getter $ \w addr utxoMap ->
    showStateIfFailAndReturn
      [CheckedUtxos utxoMap, CheckedWriter w]
      addr
      (check w utxoMap)

{- | Extract UTxOs at a computed address and call continuation returning
 Boolean value based the address, UTxOs, and data aquired from contract's writer instance.
 The address is computed using data acquired from contract's writer instance.

   @since 4.2
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
          pure False
        Just addr -> do
          let step = \case
                TxnValidate _ txn _ -> AM.updateAddresses (Ledger.Valid txn)
                TxnValidationFail Ledger.Phase2 _ txn _ _ -> AM.updateAddresses (Ledger.Invalid txn)
                _ -> id
              am = foldl' (flip step) (AM.addAddress addr mempty) chainEvents
              utxoMap = view (AM.fundsAt addr) am
          cont w addr utxoMap

{- | Extract UTxOs at an address and perform a validation computation on it.

  @since 4.2
-}
utxoAtAddress ::
  Ledger.Address ->
  ( UtxoMap ->
    Eff
      '[ Reader InitialDistribution
       , Error EmulatorFoldErr
       , Writer (Doc Void)
       ]
      Bool
  ) ->
  TracePredicate
utxoAtAddress addr = flip postMapM (generalize $ Folds.utxoAtAddress addr)

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

{- | Setting up an emulator config with values at wallets/pubKeyHashes
     and values and datums at validators.

     Example Usage:

     > emuConfig :: EmulatorConfig
     > emuConfig =
     >   let val = lovelaceValueOf 100_000_000
     >    in addressValueOptions
     >         [ (ownerPkh, v)
     >         , (user1Pkh, v)
     >         , (user2Pkh, v)
     >         ]
     >         [ (valHash, validatorVal1, datum1)
     >         , (valHash, validatorVal2, datum2)
     >         ]
     >
     > validatorVal1 :: Value
     > validatorVal1 = Value.singleton "abcd" "State Token 1" 1
     >
     > validatorVal2 :: Value
     > validatorVal2 = Value.singleton "abcd" "State Token 2" 1
     >
     > test1 :: TestTree
     > test1 = checkPredicateOptions
     >   (defaultCheckOptions & emulatorConfig .~ emuConfig)
     >   "State Token Test"
     >   (assertDone contract1 (walletInstanceTag ownerWallet) (const True) "Test #1")
     >   trace1

     Would start trace1 with 100 Ada at the owner's wallet and the
     wallets of the two users, and also have state tokens 1 and 2
     locked at the validator with datum1 and datum2 respectively.

     @since 3.2
-}
addressValueOptions ::
  [(Ledger.PaymentPubKeyHash, Ledger.Value)] ->
  [(Scripts.ValidatorHash, Ledger.Value, Ledger.Datum)] ->
  EmulatorConfig
addressValueOptions walletAllocs validatorAllocs = EmulatorConfig (Right [tx]) def def
  where
    tx :: Ledger.Tx
    tx =
      mempty
        { Ledger.txOutputs =
            fmap
              ( \(pkh, val) ->
                  Ledger.TxOut (Ledger.pubKeyHashAddress pkh Nothing) val Nothing
              )
              walletAllocs
              <> fmap (\(vh, val, d) -> Ledger.TxOut (Ledger.scriptHashAddress vh) val $ Just $ Scripts.datumHash d) validatorAllocs
        , Ledger.txData =
            Map.fromList $
              (\(_, _, d) -> (Scripts.datumHash d, d)) <$> validatorAllocs
        , Ledger.txMint = foldMap snd walletAllocs <> foldMap (\(_, v, _) -> v) validatorAllocs
        }

{- | Use a Hedgehog generator to create a property
     test of Contract traces. Unlike
     checkPredicateGenOptions, this allows the use
     of an arbitrary type for the generatef value,
     and also lets the emulator config, predicate,
     and emulator trace to depend on the generated
     value.

     A common use is to generate a list of 'actions'
     that correspond to a sequence of endpoint calls.
     This list of actions is then used to generate
     the options, trace, and predicate.

     Example usage:

     > data MyDatum a = ...
     >
     > data Action a
     >    = Add BuiltinByteString a
     >    | Rem BuiltinByteString
     >    | Mod BuiltinByteString a
     >
     > -- Generate a list of actions to perform in order.
     > -- (May want to use StateT to keep information from
     > --  preceding in scope)
     > genActions :: forall (a :: Type). (...) => Gen [Action a]
     > genActions = ...
     >
     > genEmuConfig :: forall (a :: Type). (PlutusTx.ToData a) =>
     >   Wallet -> ValidatorHash -> [Action a] -> EmulatorConfig
     > genEmuConfig wallet valHash acts =
     >   let val = lovelaceValueOf 100_000_000
     >    in addressValueOptions
     >         [ (wallet  , v <> genValue acts) ]
     >         [ (valHash , starterValue acts , starterDatum acts) ]
     >   where
     >     genValue :: [Action a] -> Value
     >     starterValue :: [Action a] -> Value
     >     starterDatum :: [Action a] -> Datum
     >
     > -- Create a list of the expected final data after running the trace.
     > -- This is essentially a user-defined simulator that should produce
     > -- the same set of data after running the actions through the trace.
     > genData :: forall (a :: Type). [Action a] -> [MyDatum a]
     > genData = ...
     >
     > -- Generate a trace from a list of actions
     > genTrace ::
     >   forall (a :: Type).
     >   ( FromJSON a
     >   , ToJSON a
     >   ) =>
     >   Wallet ->
     >   Contract ... () ->
     >   [Action a] ->
     >   EmulatorTrace
     > genTrace wallet endpoints acts = do
     >   hnd <- activateContractWallet wallet endpoints
     >   forM_ acts $ \case
     >     (Add bs val) -> do
     >       callEndpoint @"Add" hnd (bs,val)
     >       void $ waitNSlots 3
     >     (Rem bs) -> do
     >       callEndpoint @"Rem" hnd bs
     >       void $ waitNSlots 3
     >     (Mod bs val) -> do
     >       callEndpoint @"Mod" hnd (bs,val)
     >       void $ waitNSlots 3
     >
     > -- Turn a trace into a Hedgehog Property.
     > testTrace ::
     >   forall (a :: Type).
     >   ( FromJSON a
     >   , ToJSON a
     >   , FromData a
     >   , ToData a
     >   , ...
     >   ) =>
     >   ValidatorHash ->
     >   Wallet ->
     >   Contract ... () ->
     >   Gen [Action a] ->
     >   Property
     > testTrace valHash endpoints =
     >   checkPredicateGenAll
     >     (\acts -> defaultCheckOptions & emulatorConfig .~ emuConfig acts)
     >     (\acts ->
     >       dataAtAddress
     >         (scriptHashAddress valHash)
     >         (foldl (\acc x -> acc /\ elem x) top $ genData acts)
     >     )
     >     theTrace
     >   where
     >     theTrace acts = genTrace wallet endpoints acts
     >     emuConfig acts = genEmuConfig wallet valHash acts
     >
     > -- The actual Tasty test group.
     > tests :: Int -> TestTree
     > tests n =
     >   localOption (HedgehogTestLimit (Just n)) $
     >     testProperty "Testing Contract Traces" $
     >       testTrace
     >         myWallet
     >         myValidatorHash
     >         myEndpoints
     >         genActions

    This creates a Hedgehog property that generates a list of
    actions according to a user-defined generator, and then
    runs a trace calling the endpoints according to the list
    of actions generated. This is then used to check the final
    list of data against a simulator that should produce the
    same set of final data as the contract trace.

    Note that these tests can take a very long time
    to run; you'll probably want to set the number
    of tests to below 100.

     @since 5.2
-}
checkPredicateGenAll ::
  forall (a :: Type).
  Show a =>
  (a -> CheckOptions) ->
  (a -> TracePredicate) ->
  (a -> EmulatorTrace ()) ->
  Hedgehog.Gen a ->
  Hedgehog.Property
checkPredicateGenAll goptions gpredicate gtrace gen =
  Hedgehog.property $ do
    myVal <- Hedgehog.forAll gen
    let options = goptions myVal
        predicate = gpredicate myVal
        action = gtrace myVal
    checkPredicateInner options predicate action Hedgehog.annotate Hedgehog.assert

{- | The same as checkPredicateGenAll, but
     with an added value to show as a footnote
     in the case of failure.

     Exmple Usage:
     (Same as for checkPredicateGenAll, but with
      a modification to testTrace)

     > -- Turn a trace into a Hedgehog Property.
     > testTrace ::
     >   forall (a :: Type).
     >   (...) =>
     >   ValidatorHash ->
     >   Wallet ->
     >   Contract ... () ->
     >   Gen [Action a] ->
     >   Property
     > testTrace valHash endpoints =
     >   checkPredicateGenAllShow
     >     (\acts -> defaultCheckOptions & emulatorConfig .~ emuConfig acts)
     >     (\acts ->
     >       dataAtAddress
     >         (scriptHashAddress valHash)
     >         (foldl (\acc x -> acc /\ elem x) top $ genData acts)
     >     )
     >     theTrace
     >     "Simulated Data:"
     >     genData
     >   where
     >     theTrace acts = genTrace wallet endpoints acts
     >     emuConfig acts = genEmuConfig wallet valHash acts

     This is the same as the example for checkPredicateGenAll,
     but also prints out the list of the final data from their
     simulator so the user can troubleshoot possible errors with
     their simulator.

     @since 5.2
-}
checkPredicateGenAllShow ::
  forall (a :: Type) (b :: Type).
  (Show a, Show b) =>
  (a -> CheckOptions) ->
  (a -> TracePredicate) ->
  (a -> EmulatorTrace ()) ->
  String ->
  (a -> b) ->
  Hedgehog.Gen a ->
  Hedgehog.Property
checkPredicateGenAllShow goptions gpredicate gtrace str shower gen =
  Hedgehog.property $ do
    myVal <- Hedgehog.forAll gen
    let options = goptions myVal
        predicate = gpredicate myVal
        action = gtrace myVal
        modVal = shower myVal
    Hedgehog.footnoteShow modVal
    Hedgehog.footnote str
    checkPredicateInner options predicate action Hedgehog.annotate Hedgehog.assert

{- | When you want to examine multiple
     views of the generated value. Note that
     the views are appended in reverse order;
     i.e. the first view will be the last one
     printed in the event of a failure.

     Example Usage:

          > testTrace ::
     >   forall (a :: Type).
     >   (...) =>
     >   ValidatorHash ->
     >   Wallet ->
     >   Contract ... () ->
     >   Gen [Action a] ->
     >   Property
     > testTrace valHash endpoints =
     >   checkPredicateGenAllShows
     >     (\acts -> defaultCheckOptions & emulatorConfig .~ emuConfig acts)
     >     (\acts ->
     >       dataAtAddress
     >         (scriptHashAddress valHash)
     >         (foldl (\acc x -> acc /\ elem x) top $ genData acts)
     >     )
     >     theTrace
     >     [( "Simulated Data:", show . genData)
     >     ,( "Action List:"   , show)
     >     ]
     >   where
     >     theTrace acts = genTrace wallet endpoints acts
     >     emuConfig acts = genEmuConfig wallet valHash acts

     Again, this is the same as checkPredicateGenAll and
     checkPredicateGenAllShow, but will output:

       * The generated action list.
       * The final list of data at the validator,
         according to the user-supplied simulator.

     in that order, at the end of the error information
     in the event of a case that fails the predicate.

     @since 5.2
-}
checkPredicateGenAllShows ::
  forall (a :: Type).
  (Show a) =>
  (a -> CheckOptions) ->
  (a -> TracePredicate) ->
  (a -> EmulatorTrace ()) ->
  [(String, a -> String)] ->
  Hedgehog.Gen a ->
  Hedgehog.Property
checkPredicateGenAllShows goptions gpredicate gtrace showers gen =
  Hedgehog.property $ do
    myVal <- Hedgehog.forAll gen
    let options = goptions myVal
        predicate = gpredicate myVal
        action = gtrace myVal
    forM_ showers $ \(nm, fnc) -> do
      Hedgehog.footnote $ fnc myVal
      Hedgehog.footnote nm
    checkPredicateInner options predicate action Hedgehog.annotate Hedgehog.assert
