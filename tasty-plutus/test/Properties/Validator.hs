{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Validator (tests) where

--------------------------------------------------------------------------------

import Prelude hiding (($), (&&), (*), (+), (==))

import Ledger.Crypto (PubKeyHash)
import Plutus.V1.Ledger.Value (Value)
import Test.Plutus.ContextBuilder (
  ContextBuilder,
  Purpose (ForSpending),
  outToPubKey,
 )
import Test.QuickCheck.Plutus.Instances ()
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Plutus.Script.Property (
  paramScriptProperty,
  scriptProperty,
  scriptPropertyPass,
 )
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
  passIf,
 )
import Test.Tasty.Plutus.TestScript (
  TestScript,
  mkTestValidator,
  toTestValidator,
 )
import Test.Tasty.Plutus.WithScript (withTestScript)
import Test.Tasty.QuickCheck (
  Gen,
  arbitrary,
  genericShrink,
  oneof,
 )

--------------------------------------------------------------------------------

import Ledger.Address (PaymentPubKeyHash (unPaymentPubKeyHash))
import Plutus.V1.Ledger.Contexts (ScriptContext)
import PlutusTx (applyCode, liftCode)
import PlutusTx.Prelude (traceIfFalse, ($), (&&), (*), (+), (==))
import PlutusTx.TH (compile)
import Wallet.Emulator.Types (WalletNumber (WalletNumber))
import Wallet.Emulator.Wallet (fromWalletNumber, mockWalletPaymentPubKeyHash)

--------------------------------------------------------------------------------

tests :: TestTree
tests =
  testGroup
    "Validator tests"
    [ withTestScript "With simple TestValidator:" simpleTestValidator $ do
        scriptProperty "Validator checks the sum of the inputs" $
          GenForSpending genForSimple transformForSimple1
        scriptProperty "Validator checks the product of the inputs" $
          GenForSpending genForSimple transformForSimple2
        scriptPropertyPass "Validator succeeds if the sum and product are correct" $
          GenForSpending genForSimple transformForSimple3
    , paramScriptProperty
        "Validator checks secret key"
        (\(secret, _, _) -> paramTestValidator secret)
        $ GenForSpending genForParam transformForParam
    ]

genForSimple :: Methodology (Integer, Integer, Integer, Integer, Value)
genForSimple = Methodology gen' genericShrink
  where
    gen' :: Gen (Integer, Integer, Integer, Integer, Value)
    gen' = do
      (i1, i2, val) <- arbitrary
      (iSum, iProd) <- oneof [pure (i1 + i2, i1 * i2), arbitrary]
      pure (i1, i2, iSum, iProd, val)

-- | Creates TestItems with an arbitrary sum used in Redeemer
transformForSimple1 ::
  (Integer, Integer, Integer, Integer, Value) ->
  TestItems ( 'ForSpending (Integer, Integer) (Integer, Integer))
transformForSimple1 (i1, i2, iSum, _, val) =
  ItemsForSpending
    { spendDatum = (i1, i2)
    , spendRedeemer = (iSum, i1 * i2)
    , spendValue = val
    , spendCB = cb
    , spendOutcome = out
    }
  where
    cb :: ContextBuilder ( 'ForSpending (Integer, Integer) (Integer, Integer))
    cb = outToPubKey "outToUser" userPKHash val
    out :: Outcome
    out = if iSum == i1 + i2 then Pass else Fail

-- | Creates TestItems with an arbitrary product used in Redeemer
transformForSimple2 ::
  (Integer, Integer, Integer, Integer, Value) ->
  TestItems ( 'ForSpending (Integer, Integer) (Integer, Integer))
transformForSimple2 (i1, i2, _, iProd, val) =
  ItemsForSpending
    { spendDatum = (i1, i2)
    , spendRedeemer = (i1 + i2, iProd)
    , spendValue = val
    , spendCB = cb
    , spendOutcome = out
    }
  where
    cb :: ContextBuilder ( 'ForSpending (Integer, Integer) (Integer, Integer))
    cb = outToPubKey "outToUser" userPKHash val
    out :: Outcome
    out = passIf $ iProd == i1 * i2

-- | Always creates TestItems with correct sum and product
transformForSimple3 ::
  (Integer, Integer, Integer, Integer, Value) ->
  TestItems ( 'ForSpending (Integer, Integer) (Integer, Integer))
transformForSimple3 (i1, i2, _, _, val) =
  ItemsForSpending
    { spendDatum = (i1, i2)
    , spendRedeemer = (i1 + i2, i1 * i2)
    , spendValue = val
    , spendCB = cb
    , spendOutcome = out
    }
  where
    cb :: ContextBuilder ( 'ForSpending (Integer, Integer) (Integer, Integer))
    cb = outToPubKey "outToUser" userPKHash val
    out :: Outcome
    out = Pass

{- | A validator for testing property-based testing functionality.

  Validator logic:

  To spend some value, locked by the script with two integers
  you must provide the correct pair of sum and product of these integers.
-}
simpleValidator ::
  (Integer, Integer) -> (Integer, Integer) -> ScriptContext -> Bool
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

simpleTestValidator ::
  TestScript ( 'ForSpending (Integer, Integer) (Integer, Integer))
simpleTestValidator =
  mkTestValidator
    $$(compile [||simpleValidator||])
    $$(compile [||toTestValidator||])

genForParam :: Methodology (Integer, Integer, Value)
genForParam = Methodology gen genericShrink
  where
    gen :: Gen (Integer, Integer, Value)
    gen = do
      (i, val) <- arbitrary
      i' <- oneof [pure i, arbitrary]
      pure (i, i', val)

-- | Creates TestItems with an arbitrary sum used in Redeemer
transformForParam ::
  (Integer, Integer, Value) ->
  TestItems ( 'ForSpending () Integer)
transformForParam (secret, guess, val) =
  ItemsForSpending
    { spendDatum = ()
    , spendRedeemer = guess
    , spendValue = val
    , spendCB = cb
    , spendOutcome = out
    }
  where
    cb :: ContextBuilder ( 'ForSpending () Integer)
    cb = outToPubKey "outToUser" userPKHash val
    out :: Outcome
    out = if secret == guess then Pass else Fail

paramValidator :: Integer -> () -> Integer -> ScriptContext -> Bool
paramValidator secret _ guess _ = correctGuess
  where
    correctGuess :: Bool
    correctGuess =
      traceIfFalse "The guess is wrong" $
        guess == secret

paramTestValidator :: Integer -> TestScript ( 'ForSpending () Integer)
paramTestValidator secret =
  mkTestValidator
    ($$(compile [||paramValidator||]) `applyCode` liftCode secret)
    $$(compile [||toTestValidator||])

userPKHash :: PubKeyHash
userPKHash =
  unPaymentPubKeyHash
    . mockWalletPaymentPubKeyHash
    . fromWalletNumber
    $ WalletNumber 1
