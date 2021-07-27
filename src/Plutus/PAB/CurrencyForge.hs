module Plutus.PAB.CurrencyForge (initCurrency) where

--------------------------------------------------------------------------------

import Control.Monad (when)
import Prelude

--------------------------------------------------------------------------------

import Data.Kind (Type)
import Data.Text qualified as Text

--------------------------------------------------------------------------------

import Ledger.Constraints qualified as Constraints
import Plutus.Contract qualified as Contract
import Plutus.Contract.State (Contract)
import Plutus.Contracts.Currency qualified as Currency
import Plutus.Contracts.PubKey (PubKeyError)
import Plutus.PAB.Effects.Contract.Builtin qualified as Builtin
import Plutus.V1.Ledger.Crypto qualified as Ledger
import Plutus.V1.Ledger.Tx qualified as Tx
import Plutus.V1.Ledger.Value qualified as Ledger (AssetClass, TokenName, Value)
import Plutus.V1.Ledger.Value qualified as Value (assetClass, assetClassValue)
import Wallet.Emulator.Types (Wallet (..))
import Wallet.Emulator.Wallet qualified as Wallet

--------------------------------------------------------------------------------

import Plutus.PAB.OutputBus qualified as OutputBus (OutputBus, sendBus)

--------------------------------------------------------------------------------

fromCurrencyError :: Currency.CurrencyError -> Contract.ContractError
fromCurrencyError = \case
  (Currency.CurPubKeyError e) -> toContractError e
  (Currency.CurContractError e) -> e
  where
    toContractError :: PubKeyError -> Contract.ContractError
    toContractError = Contract.OtherError . Text.pack . show

-- | Send value to a wallet acting as a public key.
giveTo ::
  forall (w :: Type).
  Wallet ->
  Ledger.Value ->
  Contract w Builtin.EmptySchema Contract.ContractError ()
giveTo wallet value = do
  ownPK <- Contract.mapError fromCurrencyError (Ledger.pubKeyHash <$> Contract.ownPubKey)
  let pubKeyHash = Ledger.pubKeyHash $ Wallet.walletPubKey wallet
  when (pubKeyHash /= ownPK) $ do
    tx <- Contract.submitTx $ Constraints.mustPayToPubKey pubKeyHash value
    Contract.awaitTxConfirmed $ Tx.txId tx

initCurrency ::
  Ledger.TokenName ->
  Integer ->
  Wallet ->
  Contract (OutputBus.OutputBus Ledger.AssetClass) Builtin.EmptySchema Contract.ContractError ()
initCurrency tokenName forgedAmount receivingWallet = do
  ownPK <- Contract.mapError fromCurrencyError (Ledger.pubKeyHash <$> Contract.ownPubKey)
  cur <- Contract.mapError fromCurrencyError $ Currency.mintContract ownPK [(tokenName, forgedAmount)]
  let currencySymbol = Currency.currencySymbol cur
      assetClass = Value.assetClass currencySymbol tokenName

  giveTo receivingWallet (Value.assetClassValue assetClass forgedAmount)
  OutputBus.sendBus assetClass
