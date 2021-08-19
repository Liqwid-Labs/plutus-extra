{-# LANGUAGE RankNTypes #-}

module Plutus.Contract.Test.Extra (walletFundsChangeWithAccumState, walletFundsExactChangeWithAccumState) where

import Control.Foldl qualified as L

import Control.Lens (at, (^.))
import Control.Monad (unless)
import Control.Monad.Freer.Reader (ask)
import Control.Monad.Freer.Writer (tell)
import Data.Foldable (fold)
import Data.Text.Prettyprint.Doc (Doc, colon, indent, pretty, viaShow, vsep, (<+>))
import Data.Void
import Ledger qualified
import Ledger.Ada qualified as Ada
import Plutus.Contract.Test (TracePredicate)
import Plutus.Contract.Trace (InitialDistribution, Wallet)
import Plutus.Contract.Types (IsContract (toContract))
import Plutus.Trace.Emulator (ContractInstanceTag)
import PlutusTx.Prelude qualified as P
import Wallet.Emulator.Folds qualified as Folds
import Prelude

-- | Check that the funds in the wallet have changed by the given amount, exluding fees.
walletFundsChangeWithAccumState ::
  ( Monoid w
  , Show w
  , IsContract contract
  ) =>
  contract w s e a ->
  ContractInstanceTag ->
  Wallet ->
  (w -> Ledger.Value) ->
  TracePredicate
walletFundsChangeWithAccumState = walletFundsChangeWithAccumStateImpl False

{- | Check that the funds in the wallet have changed by the given amount, including fees.
 This functions allows us to peek into the accumulated state, and make decisions about the
 expected value based on that.
-}
walletFundsExactChangeWithAccumState ::
  ( Monoid w
  , Show w
  , IsContract contract
  ) =>
  contract w s e a ->
  ContractInstanceTag ->
  Wallet ->
  (w -> Ledger.Value) ->
  TracePredicate
walletFundsExactChangeWithAccumState = walletFundsChangeWithAccumStateImpl False

{- | Internal implementation for fund checking
 Combining the implementations of walletFundsChangeImpl and assertAccumState functions

  @since 0.3.0.0
-}
walletFundsChangeWithAccumStateImpl ::
  ( Monoid w
  , Show w
  , IsContract contract
  ) =>
  Bool ->
  contract w s e a ->
  ContractInstanceTag ->
  Wallet ->
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
