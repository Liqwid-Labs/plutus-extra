module Plutus.PAB.CurrencyForge (initCurrency) where

--------------------------------------------------------------------------------

import Control.Monad (when)
import Prelude

--------------------------------------------------------------------------------

import Data.Kind (Type)

--------------------------------------------------------------------------------

import Ledger qualified
import Ledger.Constraints qualified as Constraints
import Plutus.Contract qualified as Contract
import Plutus.Contract.State (Contract)
import Plutus.Contracts.Currency qualified as Currency
import Plutus.PAB.Effects.Contract.Builtin qualified as Builtin
import Plutus.V1.Ledger.Value qualified as Value (assetClass, assetClassValue)
import Wallet.Emulator.Types (Wallet (..))
import Wallet.Emulator.Wallet qualified as Wallet

--------------------------------------------------------------------------------

import Plutus.PAB.OutputBus qualified as OutputBus (OutputBus, sendBus)

--------------------------------------------------------------------------------

fromCurrencyError :: Currency.CurrencyError -> Contract.ContractError
fromCurrencyError = \case
  (Currency.CurContractError e) -> e

-- | Send value to a wallet acting as a public key.
giveTo ::
  forall (w :: Type).
  Wallet ->
  Ledger.Value ->
  Contract w Builtin.EmptySchema Contract.ContractError ()
giveTo wallet value = do
  ownPK <- Contract.mapError fromCurrencyError Contract.ownPubKeyHash
  let pubKeyHash = Wallet.walletPubKeyHash wallet
  when (pubKeyHash /= ownPK) $ do
    tx <- Contract.submitTx $ Constraints.mustPayToPubKey pubKeyHash value
    Contract.awaitTxConfirmed $ Ledger.getCardanoTxId tx

-- | Mint currency and give to a specific wallet
initCurrency ::
  Ledger.TokenName ->
  Integer ->
  Wallet ->
  Contract (OutputBus.OutputBus Ledger.AssetClass) Builtin.EmptySchema Contract.ContractError ()
initCurrency tokenName forgedAmount receivingWallet = do
  ownPK <- Contract.mapError fromCurrencyError Contract.ownPubKeyHash
  cur <- Contract.mapError fromCurrencyError $ Currency.mintContract ownPK [(tokenName, forgedAmount)]
  let currencySymbol = Currency.currencySymbol cur
      assetClass = Value.assetClass currencySymbol tokenName

  giveTo receivingWallet (Value.assetClassValue assetClass forgedAmount)
  OutputBus.sendBus assetClass
