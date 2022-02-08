module Test.Tasty.Plutus.Internal.Env (
  SomeScript (SomeSpender, SomeMinter),
  prepareConf,
  getScriptContext,
  getContext,
  getScriptResult,
) where

import Data.Kind (Type)
import Data.Text (Text)
import Plutus.V1.Ledger.Api (
  CurrencySymbol,
  Datum (Datum),
  Interval,
  MintingPolicy,
  POSIXTime,
  Redeemer (Redeemer),
  ScriptContext,
  ToData (toBuiltinData),
  TxId,
  Validator,
  ValidatorHash,
  Value,
 )
import Plutus.V1.Ledger.Scripts (Context (Context), ScriptError)
import Test.Tasty.Options (OptionSet, lookupOption)

import Test.Plutus.ContextBuilder (
  ContextBuilder,
  InputPosition,
  Purpose (ForMinting, ForSpending),
  TestUTXO (TestUTXO),
  TransactionConfig (TransactionConfig),
  mintingScriptContext,
  spendingScriptContext,
  testCurrencySymbol,
  testFee,
  testInputPosition,
  testTimeRange,
  testTxId,
  testValidatorHash,
 )
import Test.Tasty.Plutus.Internal.Run (ScriptResult, testMintingPolicyScript, testValidatorScript)
import Test.Tasty.Plutus.Options (
  Fee (Fee),
  ScriptInputPosition (ScriptInputPosition),
  TestCurrencySymbol (TestCurrencySymbol),
  TestTxId (TestTxId),
  TestValidatorHash (TestValidatorHash),
  TimeRange (TimeRange),
 )
import Test.Tasty.Plutus.TestData (TestData (MintingTest, SpendingTest))

data SomeScript (p :: Purpose) where
  SomeSpender ::
    forall (d :: Type) (r :: Type).
    Validator ->
    SomeScript ( 'ForSpending d r)
  SomeMinter ::
    forall (r :: Type).
    MintingPolicy ->
    SomeScript ( 'ForMinting r)

prepareConf ::
  forall (a :: Type).
  (a -> OptionSet) ->
  a ->
  TransactionConfig
prepareConf getOpts env =
  TransactionConfig
    { testFee = testFee'
    , testTimeRange = testTimeRange'
    , testTxId = testTxId'
    , testCurrencySymbol = testCurrencySymbol'
    , testValidatorHash = testValidatorHash'
    , testInputPosition = inputPosition'
    }
  where
    opts :: OptionSet
    opts = getOpts env
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
    inputPosition' :: InputPosition
    ScriptInputPosition inputPosition' = lookupOption opts

getScriptContext ::
  forall (a :: Type) (p :: Purpose).
  (a -> TransactionConfig) ->
  (a -> ContextBuilder p) ->
  (a -> TestData p) ->
  a ->
  ScriptContext
getScriptContext getConf getCb getTd env = case getTd env of
  SpendingTest d _ v -> spendingScriptContext conf cb (TestUTXO d v)
  MintingTest _ v -> mintingScriptContext conf cb v
  where
    conf :: TransactionConfig
    conf = getConf env
    cb :: ContextBuilder p
    cb = getCb env

getContext ::
  forall (a :: Type).
  (a -> ScriptContext) ->
  a ->
  Context
getContext getSc = Context . toBuiltinData . getSc

getScriptResult ::
  forall (a :: Type) (p :: Purpose).
  (a -> SomeScript p) ->
  (a -> TestData p) ->
  (a -> Context) ->
  a ->
  Either ScriptError ([Text], ScriptResult)
getScriptResult getScript getTd getCtx env =
  case (getScript env, getTd env) of
    (SomeSpender val, SpendingTest d r _) ->
      let d' = Datum . toBuiltinData $ d
          r' = Redeemer . toBuiltinData $ r
       in testValidatorScript context val d' r'
    (SomeMinter mp, MintingTest r _) ->
      let r' = Redeemer . toBuiltinData $ r
       in testMintingPolicyScript context mp r'
  where
    context :: Context
    context = getCtx env
