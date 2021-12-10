{-# LANGUAGE TemplateHaskell #-}

module Main (main) where

--------------------------------------------------------------------------------

import Prelude hiding (($), (&&), (*), (+), (==))

import Ledger.Crypto (PubKeyHash)
import Plutus.V1.Ledger.Value (Value)
import Test.QuickCheck.Plutus.Instances ()
import Test.Tasty (TestTree, defaultMain, localOption)
import Test.Tasty.Plutus.Context (
  ContextBuilder,
  Purpose (ForSpending),
  paysToPubKey,
 )
import Test.Tasty.Plutus.Script.Property (scriptProperty, scriptPropertyPass)
import Test.Tasty.Plutus.TestData (
  Generator (GenForSpending),
  Methodology (Methodology),
  Outcome (Fail, Pass),
  TestItems (
    ItemsForSpending,
    spendCB,
    spendDatum,
    spendOutcome,
    spendRedeemer,
    spendValue
  ),
 )
import Test.Tasty.Plutus.WithScript (toTestValidator, withValidator)
import Test.Tasty.QuickCheck (
  Gen,
  QuickCheckMaxSize (QuickCheckMaxSize),
  QuickCheckTests (QuickCheckTests),
  arbitrary,
  genericShrink,
  oneof,
 )

--------------------------------------------------------------------------------

import Plutus.V1.Ledger.Contexts (ScriptContext)
import Plutus.V1.Ledger.Scripts (
  Validator,
  mkValidatorScript,
 )
import PlutusTx (BuiltinData, applyCode)
import PlutusTx.Prelude (traceIfFalse, ($), (&&), (*), (+), (==))
import PlutusTx.TH (compile)
import Wallet.Emulator.Types (WalletNumber (WalletNumber))
import Wallet.Emulator.Wallet (fromWalletNumber, walletPubKeyHash)

--------------------------------------------------------------------------------

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  localOption (QuickCheckMaxSize 10) $
    localOption (QuickCheckTests 100) $
      withValidator "Property based testing" testSimpleValidator $ do
        scriptProperty "Validator checks the sum of the inputs" $
          GenForSpending gen1 transform1
        scriptProperty "Validator checks the product of the inputs" $
          GenForSpending gen1 transform2
        scriptPropertyPass "Validator succeeds if the sum and product are correct" $
          GenForSpending gen1 transform3

gen1 :: Methodology (Integer, Integer, Integer, Integer, Value)
gen1 = Methodology gen' genericShrink
  where
    gen' :: Gen (Integer, Integer, Integer, Integer, Value)
    gen' = do
      (i1, i2, val) <- arbitrary
      (iSum, iProd) <- oneof [pure (i1 + i2, i1 * i2), arbitrary]
      pure (i1, i2, iSum, iProd, val)

-- | Creates TestItems with an arbitrary sum used in Redeemer
transform1 :: (Integer, Integer, Integer, Integer, Value) -> TestItems 'ForSpending
transform1 (i1, i2, iSum, _, val) =
  ItemsForSpending
    { spendDatum = (i1, i2)
    , spendRedeemer = (iSum, i1 * i2)
    , spendValue = val
    , spendCB = cb
    , spendOutcome = out
    }
  where
    cb :: ContextBuilder 'ForSpending
    cb = paysToPubKey userPKHash val
    out :: Outcome
    out = if iSum == i1 + i2 then Pass else Fail

-- | Creates TestItems with an arbitrary product used in Redeemer
transform2 :: (Integer, Integer, Integer, Integer, Value) -> TestItems 'ForSpending
transform2 (i1, i2, _, iProd, val) =
  ItemsForSpending
    { spendDatum = (i1, i2)
    , spendRedeemer = (i1 + i2, iProd)
    , spendValue = val
    , spendCB = cb
    , spendOutcome = out
    }
  where
    cb :: ContextBuilder 'ForSpending
    cb = paysToPubKey userPKHash val
    out :: Outcome
    out = if iProd == i1 * i2 then Pass else Fail

-- | Always creates TestItems with correct sum and product
transform3 :: (Integer, Integer, Integer, Integer, Value) -> TestItems 'ForSpending
transform3 (i1, i2, _, _, val) =
  ItemsForSpending
    { spendDatum = (i1, i2)
    , spendRedeemer = (i1 + i2, i1 * i2)
    , spendValue = val
    , spendCB = cb
    , spendOutcome = out
    }
  where
    cb :: ContextBuilder 'ForSpending
    cb = paysToPubKey userPKHash val
    out :: Outcome
    out = Pass

{- | A validator for testing property-based testing functionality.

 Validator logic:

  To spend some value, locked by the script with two integers
  you must provide the correct pair of sum and product of these integers.
-}
simpleValidator :: (Integer, Integer) -> (Integer, Integer) -> ScriptContext -> Bool
simpleValidator (i1, i2) (iSum, iProd) _ = correctSum && correctProduct
  where
    correctSum :: Bool
    correctSum =
      traceIfFalse "The sum is wrong" $
        iSum == i1 + i2

    correctProduct :: Bool
    correctProduct =
      traceIfFalse "The product is wrong" $
        iProd == i1 * i2

testSimpleValidator :: Validator
testSimpleValidator =
  mkValidatorScript $
    $$(compile [||wrap||])
      `applyCode` $$(compile [||simpleValidator||])
  where
    wrap ::
      ((Integer, Integer) -> (Integer, Integer) -> ScriptContext -> Bool) ->
      (BuiltinData -> BuiltinData -> BuiltinData -> ())
    wrap = toTestValidator

userPKHash :: PubKeyHash
userPKHash = walletPubKeyHash $ fromWalletNumber $ WalletNumber 1
