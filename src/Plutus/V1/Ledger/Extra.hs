{-# OPTIONS_GHC -fno-omit-interface-pragmas #-}

module Plutus.V1.Ledger.Extra (utxoMapValue) where

import Control.Arrow ((>>>))
import Prelude qualified as P (foldMap)

--------------------------------------------------------------------------------

import Ledger.AddressMap qualified as Ledger
import Plutus.V1.Ledger.Tx qualified as Ledger (TxOut (..), TxOutTx (..))
import Plutus.V1.Ledger.Value qualified as Ledger (Value)

--------------------------------------------------------------------------------

-- | Get all Value in a UTXOMap
utxoMapValue :: Ledger.UtxoMap -> Ledger.Value
utxoMapValue = P.foldMap (Ledger.txOutTxOut >>> Ledger.txOutValue)
