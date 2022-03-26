{-# OPTIONS_GHC -fno-omit-interface-pragmas #-}

module Plutus.V1.Ledger.Extra (
  utxoMapValue,
  serialiseAddress,
  deserialiseAddress,
  unsafeDeserialiseAddress,
  unsafeSerialiseAddress,
  mustPayToAddress,
) where

import Control.Arrow ((>>>))
import Data.ByteArray qualified as ByteArray (length)
import Data.Either.Combinators (mapLeft, maybeToRight)
import Data.Kind (Type)
import Data.Text (Text)
import Data.Text qualified as Text
import Text.Printf (printf)

import Prelude

--------------------------------------------------------------------------------

import Cardano.Api (AsType (AsAddressInEra, AsAlonzoEra, AsByronEra), IsCardanoEra, NetworkId)
import Cardano.Api qualified as CAPI
import Cardano.Api.Byron qualified as CAPI
import Cardano.Chain.Common (decodeAddressBase58)
import Cardano.Prelude (MonadError, decodeUtf8)
import Ledger qualified
import Ledger.Address (
  Address (Address),
  PaymentPubKeyHash (PaymentPubKeyHash),
  StakePubKeyHash (StakePubKeyHash),
 )
import Ledger.Address qualified as Address
import Ledger.AddressMap qualified as AddressMap
import Ledger.Constraints (TxConstraints)
import Ledger.Constraints qualified as TxConstraints
import Ledger.Credential (
  Credential (PubKeyCredential),
  StakingCredential (StakingHash),
 )
import Ledger.Crypto (PubKeyHash (PubKeyHash))
import Plutus.Contract (ContractError (OtherContractError), throwError)
import Plutus.Contract.CardanoAPI (fromCardanoAddress, toCardanoAddress)
import Plutus.V1.Ledger.Value (Value)
import PlutusCore.Pretty (Pretty (pretty))
import PlutusTx.Prelude qualified as PlutusTx

--------------------------------------------------------------------------------

-- | Get all Value in a UTXOMap
utxoMapValue :: AddressMap.UtxoMap -> Value
utxoMapValue = foldMap (Ledger.txOutTxOut >>> Ledger.txOutValue)

{- | Serialise an 'Address' to the bech32 form.

 For more details on the different address types see:
 https://github.com/cardano-foundation/CIPs/blob/master/CIP-0019/CIP-0019.md

 @since 5.1
-}
serialiseAddress :: NetworkId -> Address -> Either Text Text
serialiseAddress _ address@(Address (PubKeyCredential (PubKeyHash bytes)) _)
  | ByteArray.length bytes > 28 =
    -- When serialised from Byron,
    -- the pubkeyhashes are much longer than Alonzo era address pubkeyhashes
    CAPI.serialiseAddress <$> serialiseByronAddress address
serialiseAddress networkId address = CAPI.serialiseAddress <$> serialiseAlonzoAddress networkId address

serialiseByronAddress ::
  Address ->
  Either Text (CAPI.AddressInEra CAPI.ByronEra)
serialiseByronAddress (Address (PubKeyCredential (PubKeyHash bytes)) _) = do
  b58 <-
    mapLeft (Text.pack . show) $
      decodeAddressBase58 $ decodeUtf8 $ PlutusTx.fromBuiltin bytes
  pure $ CAPI.AddressInEra CAPI.ByronAddressInAnyEra (CAPI.ByronAddress b58)
serialiseByronAddress _ = Left "Invalid Byron address"

serialiseAlonzoAddress ::
  NetworkId ->
  Address ->
  Either Text (CAPI.AddressInEra CAPI.AlonzoEra)
serialiseAlonzoAddress networkId address = do
  mapLeft (\err -> (Text.pack . show . pretty $ err) <> "\n" <> (Text.pack . show $ address)) $
    toCardanoAddress networkId address

{- | Unsafe version of 'serialiseAddress'.

 @since 5.1
-}
unsafeSerialiseAddress :: NetworkId -> Address -> Text
unsafeSerialiseAddress networkId address =
  either (error . Text.unpack) id $ serialiseAddress networkId address

{- | Deserialise an 'Address' from the bech32 form.

 @since 5.1
-}
deserialiseAddress :: Text -> Either Text Address
deserialiseAddress addr =
  if "addr" `Text.isPrefixOf` addr
    then deserialiseAddress' AsAlonzoEra addr
    else deserialiseAddress' AsByronEra addr

deserialiseAddress' ::
  forall (era :: Type).
  IsCardanoEra era =>
  AsType era ->
  Text ->
  Either Text Address
deserialiseAddress' eraType addr = do
  cardanoAddr <-
    maybeToRight "Couldn't deserialise address" $
      CAPI.deserialiseAddress (AsAddressInEra eraType) addr
  mapLeft (const "Couldn't convert deserialised address") $ fromCardanoAddress cardanoAddr

{- | Unsafe version of `deserialiseAddress`.

 @since 5.1
-}
unsafeDeserialiseAddress :: Text -> Address
unsafeDeserialiseAddress address =
  either (error . Text.unpack) id $ deserialiseAddress address

{- | Lock the 'Value' with an 'Address'.

 @since 5.1
-}
mustPayToAddress ::
  forall (i :: Type) (m :: Type -> Type) (o :: Type).
  (MonadError ContractError m) =>
  Address ->
  Value ->
  m (TxConstraints i o)
mustPayToAddress addr =
  case ( PaymentPubKeyHash <$> Address.toPubKeyHash addr
       , Address.toValidatorHash addr
       , Address.stakingCredential addr
       ) of
    (Just pkh, Nothing, Nothing) -> pure . TxConstraints.mustPayToPubKey pkh
    (Nothing, Just valh, Nothing) -> pure . TxConstraints.mustPayToOtherScript valh Ledger.unitDatum
    (Just pkh, Nothing, Just (StakingHash (PubKeyCredential sth))) ->
      pure . TxConstraints.mustPayToPubKeyAddress pkh (StakePubKeyHash sth)
    triple ->
      const $
        throwError $
          OtherContractError $
            Text.pack $
              printf "Unkown type of address %s: %s" (show addr) (show triple)
