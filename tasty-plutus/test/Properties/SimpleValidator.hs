{-# LANGUAGE TemplateHaskell #-}

module Properties.SimpleValidator (tests) where

--------------------------------------------------------------------------------

import Prelude hiding (fmap, ($), (&&), (+), (-), (==))

import Ledger.Crypto (PubKeyHash)
import Plutus.V1.Ledger.Value (Value)
import Test.QuickCheck.Plutus.Instances ()
import Test.Tasty (TestTree, localOption)
import Test.Tasty.Plutus.Context (
  ContextBuilder,
  Purpose (ForSpending),
  paysSelf,
  paysToPubKey,
 )
import Test.Tasty.Plutus.Script.Property (scriptProperty)
import Test.Tasty.Plutus.Script.Unit (toTestValidator)
import Test.Tasty.Plutus.TestData (
  Example (Bad, Good),
  Generator (GenForSpending),
  Methodology (Methodology),
  TestItems (
    ItemsForSpending,
    spendCB,
    spendDatum,
    spendExample,
    spendRedeemer,
    spendValue
  ),
 )
import Test.Tasty.Plutus.WithScript (withValidator)
import Test.Tasty.QuickCheck (Gen, QuickCheckTests (QuickCheckTests), arbitrary, genericShrink, oneof)

--------------------------------------------------------------------------------

import Plutus.V1.Ledger.Contexts (ScriptContext, TxOut)
import Plutus.V1.Ledger.Contexts qualified as Contexts
import Plutus.V1.Ledger.Contexts.Extra qualified as Contexts.Extra
import Plutus.V1.Ledger.Scripts (
  Validator,
  mkValidatorScript,
 )
import Plutus.V1.Ledger.Value qualified as Value
import PlutusTx (BuiltinData, applyCode)
import PlutusTx.Prelude (fmap, fromMaybe, trace, traceIfFalse, uniqueElement, zero, ($), (&&), (-), (==))
import PlutusTx.TH (compile)
import Wallet.Emulator.Types (WalletNumber (WalletNumber))
import Wallet.Emulator.Wallet (Wallet, fromWalletNumber, walletPubKeyHash)

--------------------------------------------------------------------------------

tests :: TestTree
tests =
  localOption (QuickCheckTests 100) $
    withValidator "Property based testing" testSimpleValidator $ do
      scriptProperty "Output secret" $ GenForSpending gen1 transform1
      scriptProperty "Guessed secret" $ GenForSpending gen1 transform2

gen1 :: Methodology (PubKeyHash, Integer, Integer, Value)
gen1 = Methodology gen' genericShrink
  where
    gen' :: Gen (PubKeyHash, Integer, Integer, Value)
    gen' = do
      (pkh, inInt, val) <- arbitrary
      outInt <- oneof [pure inInt, arbitrary]
      pure (pkh, inInt, outInt, val)

transform1 :: (PubKeyHash, Integer, Integer, Value) -> TestItems 'ForSpending
transform1 (pkh, int, int', val) =
  ItemsForSpending
    { spendDatum = (pkh, int)
    , spendRedeemer = (int, val)
    , spendValue = val
    , spendCB = cb
    , spendExample = ex
    }
  where
    cb :: ContextBuilder 'ForSpending
    cb =
      paysSelf zero (pkh, int')
        <> paysToPubKey userPKHash val
    ex :: Example
    ex = if int == int' then Good else Bad

transform2 :: (PubKeyHash, Integer, Integer, Value) -> TestItems 'ForSpending
transform2 (pkh, int, int', val) =
  ItemsForSpending
    { spendDatum = (pkh, int)
    , spendRedeemer = (int', val)
    , spendValue = val
    , spendCB = cb
    , spendExample = ex
    }
  where
    cb :: ContextBuilder 'ForSpending
    cb =
      paysSelf zero (pkh, int)
        <> paysToPubKey userPKHash val
    ex :: Example
    ex = if int == int' then Good else Bad

simpleValidator :: (PubKeyHash, Integer) -> (Integer, Value) -> ScriptContext -> Bool
simpleValidator (_, secret) (guess, value) sc =
  secretUnlocked
    && enoughtValue
    && correctOutputValue
    && correctOutputDatum
  where
    secretUnlocked :: Bool
    secretUnlocked =
      traceIfFalse "The guess is wrong" $
        secret == guess

    enoughtValue :: Bool
    enoughtValue =
      traceIfFalse "There is not enougth value" $
        ownValue `Value.geq` value

    correctOutputValue :: Bool
    correctOutputValue =
      traceIfFalse "Incorrect output value" $
        Just (Value.unionWith (-) ownValue value) == ownOutputValue

    correctOutputDatum :: Bool
    correctOutputDatum =
      traceIfFalse "Incorrect output Datum" $
        fromMaybe False $ do
          ownInput' <- ownInput
          ownOutput' <- ownOutput
          pure $
            Contexts.txOutDatumHash ownInput' == Contexts.txOutDatumHash ownOutput'

    ownValue :: Value
    ownValue = Contexts.Extra.ownInputValue sc

    ownInput :: Maybe TxOut
    ownInput = case Contexts.findOwnInput sc of
      Nothing -> trace "Missing own input" Nothing
      Just res -> pure . Contexts.txInInfoResolved $ res

    ownOutput :: Maybe TxOut
    ownOutput = case uniqueElement . Contexts.getContinuingOutputs $ sc of
      Nothing -> trace "Extra outputs" Nothing
      Just output -> pure output

    ownOutputValue :: Maybe Value
    ownOutputValue = fmap Contexts.txOutValue ownOutput

testSimpleValidator :: Validator
testSimpleValidator =
  mkValidatorScript $
    $$(compile [||wrap||])
      `applyCode` $$(compile [||simpleValidator||])
  where
    wrap ::
      ((PubKeyHash, Integer) -> (Integer, Value) -> ScriptContext -> Bool) ->
      (BuiltinData -> BuiltinData -> BuiltinData -> ())
    wrap = toTestValidator

userPKHash :: PubKeyHash
userPKHash = walletPubKeyHash $ mkEmulatorWallet 1

mkEmulatorWallet :: Integer -> Wallet
mkEmulatorWallet i
  | i < 11 = fromWalletNumber $ WalletNumber i
  | i < 0 = error $ "Negative wallet number: " <> show i
  | otherwise = error $ "Your wallet number " <> show i <> " is bigger than 10 and hence there's no plutus mock wallet for it"
