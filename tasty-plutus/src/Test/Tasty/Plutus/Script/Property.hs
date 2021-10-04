{- |
 Module: Test.Tasty.Plutus.Script.Property
 Copyright: (C) MLabs 2021
 License: Apache 2.0
 Maintainer: Koz Ross <koz@mlabs.city>
 Portability: GHC only
 Stability: Experimental

 Generates QuickCheck property tests for validator and minting policy testing.
-}
module Test.Tasty.Plutus.Script.Property (
  scriptProperty
  ) where

import Plutus.V1.Ledger.Interval (Interval)
import Plutus.V1.Ledger.Time (POSIXTime)
import Type.Reflection (Typeable)
import Plutus.V1.Ledger.Scripts (Validator, ValidatorHash, MintingPolicy,
  Context (Context), Datum (Datum), Redeemer (Redeemer), runScript,
  ScriptError)
import PlutusTx.IsData.Class (ToData (toBuiltinData), FromData)
import Data.Sequence qualified as Seq
import Control.Monad.RWS.Strict (tell)
import Control.Monad (guard)
import Data.Kind (Type)
import Control.Monad.Reader (ask)
import Prelude
import Test.Tasty.Plutus.Internal (Purpose (ForSpending, ForMinting), ContextBuilder,
  WithScript (WithSpending, WithMinting), 
  TransactionConfig (TransactionConfig, testFee, testTimeRange,
                     testTxId, testCurrencySymbol, testValidatorHash),
  PropertyTestCount (PropertyTestCount), PropertyMaxSize (PropertyMaxSize),
  compileSpending)
import Test.Tasty.Plutus.TestData (TestData (SpendingTest, MintingTest),
  Generator (GenForSpending, GenForMinting),
  Methodology (Methodology),
  Example (Good))
import Plutus.V1.Ledger.Value (Value, CurrencySymbol)
import Plutus.V1.Ledger.TxId (TxId)
import Test.Tasty.Providers (singleTest, IsTest (run, testOptions),
  testPassed, testFailed)
import Test.Tasty.Plutus.Options (Fee (Fee), TimeRange (TimeRange), TestTxId (TestTxId), 
  TestCurrencySymbol (TestCurrencySymbol), TestValidatorHash (TestValidatorHash))
import Test.QuickCheck (Gen, forAllShrink, Property, Args (maxSuccess, maxSize),
  stdArgs, quickCheckWithResult, Result (Success, GaveUp, Failure, NoExpectedFailure),
  forAllShrinkShow, classify, counterexample)
import Data.Tagged (Tagged (Tagged))
import Test.Tasty.Options (OptionDescription (Option), lookupOption)
import Data.Proxy (Proxy (Proxy))

-- | @since 3.1
scriptProperty :: forall (p :: Purpose) . 
  String -> 
  Generator p -> 
  (TestData p -> ContextBuilder p) ->
  WithScript p ()
scriptProperty name gen mkCB = case gen of
  GenForSpending f mDat mRed mVal -> WithSpending $ do
    val <- ask
    let gen = spendingTupleGen f mDat mRed mVal
    let shrinker = spendingTupleShrink f mDat mRed mVal
    tell . 
      Seq.singleton . 
      singleTest name . 
      Spender val gen shrinker $ mkCB
  GenForMinting f mRed -> WithMinting $ do
    mp <- ask
    let gen = mintingTupleGen f mRed
    let shrinker = mintingTupleShrink f mRed
    tell . 
      Seq.singleton . 
      singleTest name . 
      Minter mp gen shrinker $ mkCB

-- Helpers

data PropertyTest (p :: Purpose) where
  Spender ::
    (ToData datum, ToData redeemer,
      FromData datum, FromData redeemer,
      Show datum, Show redeemer) => 
    Validator -> 
    Gen (Example, datum, redeemer, Value) -> 
    ((Example, datum, redeemer, Value) -> [(Example, datum, redeemer, Value)]) -> 
    (TestData 'ForSpending -> ContextBuilder 'ForSpending) -> 
    PropertyTest 'ForSpending
  Minter :: 
    MintingPolicy -> 
    Gen (Example, redeemer) -> 
    ((Example, redeemer) -> [(Example, redeemer)]) -> 
    (TestData 'ForMinting -> ContextBuilder 'ForMinting) -> 
    PropertyTest 'ForMinting

instance (Typeable p) => IsTest (PropertyTest p) where
  run opts vt _ = do
    let conf = TransactionConfig { testFee = testFee',
                                   testTimeRange = testTimeRange',
                                   testTxId = testTxId',
                                   testCurrencySymbol = testCurrencySymbol',
                                   testValidatorHash = testValidatorHash'
                                 }
    let PropertyTestCount testCount' = lookupOption opts
    let PropertyMaxSize maxSize' = lookupOption opts
    let args = stdArgs { maxSuccess = testCount', maxSize = maxSize' }
    res <- quickCheckWithResult args . go $ conf
    pure $ case res of 
      Success{} -> testPassed ""
      GaveUp{} -> testFailed "Internal error: gave up."
      Failure{} -> testFailed ""
      NoExpectedFailure{} -> testFailed "Internal error: expected failure but saw none."
    where
      go :: TransactionConfig -> Property
      go tc = case vt of 
        Spender val gen shrinker mkCB -> 
          forAllShrinkShow gen shrinker prettySpender . spenderProperty val tc $ mkCB
        Minter mp gen shrinker mkCB -> 
          forAllShrinkShow gen shrinker prettyMinter . minterProperty mp tc $ mkCB
      testFee' :: Value
      Fee testFee' = lookupOption opts
      testTimeRange' :: Interval POSIXTime
      TimeRange testTimeRange' = lookupOption opts
      testTxId' :: TxId
      TestTxId testTxId' = lookupOption opts
      testCurrencySymbol' :: CurrencySymbol
      TestCurrencySymbol testCurrencySymbol' = lookupOption opts
      testValidatorHash' :: ValidatorHash
      TestValidatorHash testValidatorHash' = lookupOption opts
  testOptions = Tagged 
    [ Option @Fee Proxy,
      Option @TimeRange Proxy,
      Option @TestTxId Proxy,
      Option @TestCurrencySymbol Proxy,
      Option @TestValidatorHash Proxy,
      Option @PropertyTestCount Proxy,
      Option @PropertyMaxSize Proxy
    ]

spenderProperty :: 
  forall (datum :: Type) (redeemer :: Type) . 
  (ToData datum, ToData redeemer,
    FromData datum, FromData redeemer,
    Show datum, Show redeemer) =>
  Validator -> 
  TransactionConfig -> 
  (TestData 'ForSpending -> ContextBuilder 'ForSpending) -> 
  (Example, datum, redeemer, Value) -> 
  Property
spenderProperty val tc mkCB (ex, d, r, v) =
  let td = SpendingTest d r v 
      context = compileSpending tc (mkCB td) d v
      context' = Context . toBuiltinData $ context 
      d' = Datum . toBuiltinData $ d
      r' = Redeemer . toBuiltinData $ r in
    classify (ex == Good) "Good" $ case runScript context' val d' r' of 
      Left err -> counterexample (formatError err) False
      Right (_, logs) -> deliverResult ex logs context td

prettySpender :: 
  forall (datum :: Type) (redeemer :: Type) . 
  (Example, datum, redeemer, Value) -> 
  String
prettySpender = _

minterProperty :: 
  forall (redeemer :: Type) . 
  _ -> 
  TransactionConfig -> 
  (TestData 'ForMinting -> ContextBuilder 'ForMinting) -> 
  (Example, redeemer) -> 
  Property
minterProperty = _

prettyMinter :: 
  forall (redeemer :: Type) . 
  (Example, redeemer) -> 
  String
prettyMinter = _

spendingTupleGen :: 
  forall (datum :: Type) (redeemer :: Type) . 
  (datum -> redeemer -> Value -> Example) -> 
  Methodology datum -> 
  Methodology redeemer -> 
  Methodology Value -> 
  Gen (Example, datum, redeemer, Value)
spendingTupleGen f mDat mRed mVal = do
  let Methodology genD _ = mDat
  let Methodology genR _ = mRed
  let Methodology genVal _ = mVal
  (d, r, v) <- (,,) <$> genD <*> genR <*> genVal
  pure (f d r v, d, r, v)

spendingTupleShrink :: 
  forall (datum :: Type) (redeemer :: Type) .
  (datum -> redeemer -> Value -> Example) -> 
  Methodology datum -> 
  Methodology redeemer -> 
  Methodology Value -> 
  (Example, datum, redeemer, Value) -> 
  [(Example, datum, redeemer, Value)]
spendingTupleShrink f mDat mRed mVal (ex, d, r, v) = do
  let Methodology _ shrinkD = mDat
  let Methodology _ shrinkR = mRed
  let Methodology _ shrinkV = mVal
  (d', r', v') <- (,,) <$> shrinkD d <*> shrinkR r <*> shrinkV v
  guard (f d' r' v' == ex)
  pure (ex, d', r', v')

mintingTupleGen :: 
  forall (redeemer :: Type) . 
  (redeemer -> Example) -> 
  Methodology redeemer -> 
  Gen (Example, redeemer)
mintingTupleGen f mRed = do
  let Methodology genR _ = mRed
  r <- genR
  pure (f r, r)

mintingTupleShrink :: 
  forall (redeemer :: Type) . 
  (redeemer -> Example) -> 
  Methodology redeemer -> 
  (Example, redeemer) -> 
  [(Example, redeemer)]
mintingTupleShrink f mRed (ex, r) = do
  let Methodology _ shrinkR = mRed
  r' <- shrinkR r
  guard (f r == ex)
  pure (ex, r')

formatError :: ScriptError -> String
formatError = _
