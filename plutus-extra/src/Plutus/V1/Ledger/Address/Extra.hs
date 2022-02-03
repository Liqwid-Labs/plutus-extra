module Plutus.V1.Ledger.Address.Extra (
  serialiseAddress,
  deserialiseAddress,
  unsafeDeserialiseAddress,
  unsafeSerialiseAddress,
) where

import Cardano.Api (AsType (AsAddressInEra, AsAlonzoEra, AsByronEra), IsCardanoEra, NetworkId)
import Cardano.Api qualified as CAPI
import Cardano.Api.Byron qualified as CAPI
import Cardano.Chain.Common (decodeAddressBase58)
import Cardano.Prelude (decodeUtf8)
import Data.ByteArray (length)
import Data.Either.Combinators (mapLeft, maybeToRight)
import Data.Kind (Type)
import Data.Text (Text)
import Data.Text qualified as Text
import Ledger.Address (Address (..))
import Ledger.Credential (Credential (..))
import Ledger.Crypto (PubKeyHash (PubKeyHash))
import Plutus.Contract.CardanoAPI (fromCardanoAddress, toCardanoAddress)
import PlutusCore.Pretty (Pretty (pretty))
import PlutusTx.Prelude qualified as PlutusTx
import Prelude hiding (length)

{- | Serialise an 'Address' to the bech32 form.

 For more details on the different address types see:
 https://github.com/cardano-foundation/CIPs/blob/master/CIP-0019/CIP-0019.md

 @since 5.1
-}
serialiseAddress :: NetworkId -> Address -> Either Text Text
serialiseAddress _ address@(Address (PubKeyCredential (PubKeyHash bytes)) _)
  | length bytes > 28 =
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
