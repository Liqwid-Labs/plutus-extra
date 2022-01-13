{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-all #-}

module Validator (tests) where

--------------------------------------------------------------------------------

import Prelude hiding (($), (&&), (*), (+), (==))

import Ledger.Crypto (PubKeyHash)
import Ledger.Typed.Scripts (
  TypedValidator,
  ValidatorTypes (type DatumType, type RedeemerType),
  mkTypedValidator,
 )
import Plutus.V1.Ledger.Value (Value)
import Test.QuickCheck.Plutus.Instances ()
import Test.Tasty (TestTree)
import Test.Tasty.Plutus.Context (
  ContextBuilder,
  Purpose (ForSpending),
  paysToPubKey,
 )
import Test.Tasty.Plutus.Script.Property (scriptProperty, scriptPropertyPass, withSpendGeneratorProp)
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
    spendValue,
    spendValidator
  ),
  passIf,
  TestValidator (TestValidator),
  mkTestValidator,
 )
import Test.Tasty.Plutus.WithScript (toTestValidator, withValidator)
import Test.Tasty.QuickCheck (
  Gen,
  arbitrary,
  genericShrink,
  oneof,
 )

--------------------------------------------------------------------------------

import Plutus.V1.Ledger.Contexts (ScriptContext)
import PlutusTx.Prelude (traceIfFalse, ($), (&&), (*), (+), (==))
import PlutusTx.TH (compile)
import PlutusTx (applyCode, liftCode)
import Wallet.Emulator.Types (WalletNumber (WalletNumber))
import Wallet.Emulator.Wallet (fromWalletNumber, walletPubKeyHash)

--------------------------------------------------------------------------------

tests :: TestTree
tests =
  withSpendGeneratorProp
    "parameterizedValidator:"
    gen1
    genericShrink
    transform1

gen1 :: Gen (Integer, Integer, Value)
gen1 = do
  (i, val) <- arbitrary
  i' <- oneof [pure i, arbitrary]
  pure (i, i', val)

-- | Creates TestItems with an arbitrary sum used in Redeemer
transform1 :: (Integer, Integer, Value) -> TestItems ( 'ForSpending () Integer)
transform1 (secret, guess, val) =
  ItemsForSpending
    { spendDatum = ()
    , spendRedeemer = guess
    , spendValue = val
    , spendCB = cb
    , spendOutcome = out
    , spendValidator = typedSimpleValidator secret

    }
  where
    cb :: ContextBuilder ( 'ForSpending () Integer)
    cb = paysToPubKey userPKHash val
    out :: Outcome
    out = if secret == guess then Pass else Fail

-- -- | Creates TestItems with an arbitrary product used in Redeemer
-- transform2 :: (Integer, Integer, Integer, Integer, Value) -> TestItems ( 'ForSpending (Integer, Integer) (Integer, Integer))
-- transform2 (i1, i2, _, iProd, val) =
--   ItemsForSpending
--     { spendDatum = (i1, i2)
--     , spendRedeemer = (i1 + i2, iProd)
--     , spendValue = val
--     , spendCB = cb
--     , spendOutcome = out
--     }
--   where
--     cb :: ContextBuilder ( 'ForSpending (Integer, Integer) (Integer, Integer))
--     cb = paysToPubKey userPKHash val
--     out :: Outcome
--     out = passIf $ iProd == i1 * i2

-- -- | Always creates TestItems with correct sum and product
-- transform3 :: (Integer, Integer, Integer, Integer, Value) -> TestItems ( 'ForSpending (Integer, Integer) (Integer, Integer))
-- transform3 (i1, i2, _, _, val) =
--   ItemsForSpending
--     { spendDatum = (i1, i2)
--     , spendRedeemer = (i1 + i2, i1 * i2)
--     , spendValue = val
--     , spendCB = cb
--     , spendOutcome = out
--     }
--   where
--     cb :: ContextBuilder ( 'ForSpending (Integer, Integer) (Integer, Integer))
--     cb = paysToPubKey userPKHash val
--     out :: Outcome
--     out = Pass

{- | A parameterized validator for testing property-based testing functionality.

  Validator logic:

  To spend some value, locked by the script with secret
  you have to provide the correct key.
-}
simpleValidator :: Integer -> () -> Integer -> ScriptContext -> Bool
simpleValidator secret _ guess _ = correctGuess
  where
    correctGuess :: Bool
    correctGuess =
      traceIfFalse "The guess is wrong" $
        guess == secret

typedSimpleValidator :: Integer -> TestValidator () Integer
typedSimpleValidator secret =
  mkTestValidator
    ( $$(compile [||simpleValidator||]) `applyCode` (liftCode secret))
    $$(compile [||toTestValidator||])

userPKHash :: PubKeyHash
userPKHash = walletPubKeyHash $ fromWalletNumber $ WalletNumber 1
