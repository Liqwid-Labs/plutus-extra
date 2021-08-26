module Suites.Plutus.PAB.CurrencyForge (
  tests,
) where

import Control.Monad (void)
import Data.Semigroup (Last (Last))
import Ledger qualified
import Ledger.Ada qualified as Ada
import Ledger.Value qualified as Value
import Plutus.Contract (ContractError)
import Plutus.Contract.Schema (EmptySchema)
import Plutus.Contract.Test
import Plutus.Contract.Test.Extra
import Plutus.Contract.Types (Contract)
import Plutus.PAB.CurrencyForge qualified as CurrencyForge
import Plutus.PAB.OutputBus (OutputBus (..))
import Plutus.Trace.Emulator (ContractInstanceTag, activateContractWallet, waitNSlots, walletInstanceTag)
import Test.Tasty
import Prelude

w1, w2 :: Wallet
w1 = Wallet 1
w2 = Wallet 2

t1 :: ContractInstanceTag
t1 = walletInstanceTag w1

initCurrency :: Contract (OutputBus Ledger.AssetClass) EmptySchema ContractError ()
initCurrency =
  CurrencyForge.initCurrency "testToken" 4 w2

tests :: [TestTree]
tests =
  [ checkPredicate
      "Mint currency at wallet 1 and give to wallet 2"
      ( let getValue output =
              case getOutputBus output of
                Nothing -> Ada.lovelaceValueOf 10
                Just (Last ac) -> Value.assetClassValue ac 4
         in walletFundsChange w1 mempty
              .&&. walletFundsChangeWithAccumState initCurrency t1 w2 getValue
      )
      $ void $ activateContractWallet w1 initCurrency >> waitNSlots 2
  ]
