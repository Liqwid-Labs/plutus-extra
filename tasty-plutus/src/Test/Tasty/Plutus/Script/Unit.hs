{-# LANGUAGE Trustworthy #-}

{- |
 Module: Test.Tasty.Plutus.Script.Unit
 Copyright: (C) MLabs 2021
 License: Apache 2.0
 Maintainer: Koz Ross <koz@mlabs.city>
 Portability: GHC only
 Stability: Experimental

 A unit-test-like interface for validator and minting policy testing.

 = Example usage

 > myTests :: TestTree
 > myTests = withValidator "Testing my spending" myValidator $ do
 >    shouldValidate "Valid case" validData validContext
 >    shouldn'tValidate "Invalid context" validData invalidContext
 >    shouldn'tValidate "Invalid data" invalidData validContext
 >    shouldn'tValidate "Everything is bad" invalidData invalidContext
 >    ...
-}
module Test.Tasty.Plutus.Script.Unit (
  -- * Testing API
  shouldValidate,
  shouldn'tValidate,
) where

import Control.Monad.Reader (asks)
import Control.Monad.Writer (tell)
import Data.Proxy (Proxy (Proxy))
import Data.Sequence qualified as Seq
import Data.Tagged (Tagged (Tagged))
import Data.Text (Text)
import Data.Text qualified as Text
import Ledger.Value (CurrencySymbol, Value)
import Plutus.V1.Ledger.Contexts (ScriptContext)
import Plutus.V1.Ledger.Interval (Interval)
import Plutus.V1.Ledger.Scripts (
  Context (Context),
  Datum (Datum),
  MintingPolicy,
  Redeemer (Redeemer),
  ScriptError (
    EvaluationError,
    EvaluationException,
    MalformedScript
  ),
  Validator,
  ValidatorHash,
  runMintingPolicyScript,
  runScript,
 )
import Plutus.V1.Ledger.Time (POSIXTime)
import Plutus.V1.Ledger.TxId (TxId)
import Plutus.V1.Pretty (scriptContextToValue)
import PlutusTx.IsData.Class (ToData (toBuiltinData))
import Safe (lastMay)
import Test.Tasty.Options (
  OptionDescription (Option),
  lookupOption,
 )
import Test.Tasty.Plutus.Internal (
  ContextBuilder,
  Purpose (ForMinting, ForSpending),
  TransactionConfig (
    TransactionConfig,
    testCurrencySymbol,
    testFee,
    testTimeRange,
    testTxId,
    testValidatorHash
  ),
  WithScript (WithMinting, WithSpending),
  compileMinting,
  compileSpending,
  ourStyle,
 )
import Test.Tasty.Plutus.Options (
  Fee (Fee),
  PlutusTracing (Always, OnlyOnFail),
  TestCurrencySymbol (TestCurrencySymbol),
  TestTxId (TestTxId),
  TestValidatorHash (TestValidatorHash),
  TimeRange (TimeRange),
 )
import Test.Tasty.Plutus.TestData (TestData (MintingTest, SpendingTest))
import Test.Tasty.Providers (
  IsTest (run, testOptions),
  Result,
  singleTest,
  testFailed,
  testPassed,
 )
import Text.PrettyPrint (
  Doc,
  colon,
  hang,
  int,
  renderStyle,
  text,
  vcat,
  ($+$),
  (<+>),
 )
import Text.Show.Pretty (ppDoc, valToDoc)
import Type.Reflection (Typeable)

{- | Specify that, given this test data and context, the validation should
 succeed.

 @since 3.0
-}
shouldValidate ::
  forall (p :: Purpose).
  (Typeable p) =>
  String ->
  TestData p ->
  ContextBuilder p ->
  WithScript p ()
shouldValidate name td cb = case td of
  SpendingTest {} -> WithSpending $ do
    tt <- asks (singleTest name . Spender Pass td cb)
    tell . Seq.singleton $ tt
  MintingTest {} -> WithMinting $ do
    tt <- asks (singleTest name . Minter Pass td cb)
    tell . Seq.singleton $ tt

{- | Specify that, given this test data and context, the validation should fail.

 @since 3.0
-}
shouldn'tValidate ::
  forall (p :: Purpose).
  (Typeable p) =>
  String ->
  TestData p ->
  ContextBuilder p ->
  WithScript p ()
shouldn'tValidate name td cb = case td of
  SpendingTest {} -> WithSpending $ do
    tt <- asks (singleTest name . Spender Fail td cb)
    tell . Seq.singleton $ tt
  MintingTest {} -> WithMinting $ do
    tt <- asks (singleTest name . Minter Fail td cb)
    tell . Seq.singleton $ tt

-- Helpers

data Outcome = Fail | Pass

data ScriptTest (p :: Purpose) where
  Spender ::
    Outcome ->
    TestData 'ForSpending ->
    ContextBuilder 'ForSpending ->
    Validator ->
    ScriptTest 'ForSpending
  Minter ::
    Outcome ->
    TestData 'ForMinting ->
    ContextBuilder 'ForMinting ->
    MintingPolicy ->
    ScriptTest 'ForMinting

instance (Typeable p) => IsTest (ScriptTest p) where
  run opts vt _ = pure $ case vt of
    Spender expected td@(SpendingTest d r v) cb val ->
      let context = compileSpending conf cb d v
          context' = Context . toBuiltinData $ context
          d' = Datum . toBuiltinData $ d
          r' = Redeemer . toBuiltinData $ r
       in case runScript context' val d' r' of
            Left err -> handleError shouldChat expected conf context td err
            Right (_, logs) -> deliverResult shouldChat expected logs conf context td
    Minter expected td@(MintingTest r) cb mp ->
      let context = compileMinting conf cb
          context' = Context . toBuiltinData $ context
          r' = Redeemer . toBuiltinData $ r
       in case runMintingPolicyScript context' mp r' of
            Left err -> handleError shouldChat expected conf context td err
            Right (_, logs) -> deliverResult shouldChat expected logs conf context td
    where
      conf :: TransactionConfig
      conf =
        TransactionConfig
          { testFee = testFee'
          , testTimeRange = testTimeRange'
          , testTxId = testTxId'
          , testCurrencySymbol = testCurrencySymbol'
          , testValidatorHash = testValidatorHash'
          }
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
      shouldChat :: PlutusTracing
      shouldChat = lookupOption opts
  testOptions =
    Tagged
      [ Option @Fee Proxy
      , Option @TimeRange Proxy
      , Option @TestTxId Proxy
      , Option @TestCurrencySymbol Proxy
      , Option @TestValidatorHash Proxy
      , Option @PlutusTracing Proxy
      ]

handleError ::
  forall (p :: Purpose).
  PlutusTracing ->
  Outcome ->
  TransactionConfig ->
  ScriptContext ->
  TestData p ->
  ScriptError ->
  Result
handleError shouldChat expected conf sc td = \case
  EvaluationError logs msg -> case expected of
    Pass -> testFailed . unexpectedFailure msg $ logs
    Fail -> testPassed $ case shouldChat of
      Always ->
        renderStyle ourStyle $
          ""
            $+$ hang "Logs" 4 (dumpLogs logs)
      OnlyOnFail -> ""
  EvaluationException name msg ->
    testFailed . renderStyle ourStyle $
      "Unexpected behaviour in script:" <+> text name
        $+$ hang "Description" 4 (text msg)
  MalformedScript msg ->
    testFailed . renderStyle ourStyle $
      "Script was malformed"
        $+$ hang "Details" 4 (text msg)
  where
    unexpectedFailure :: String -> [Text] -> String
    unexpectedFailure msg logs =
      renderStyle ourStyle $
        "Unexpected failure: " <+> text msg
          $+$ dumpState logs
    dumpState :: [Text] -> Doc
    dumpState logs =
      ""
        $+$ hang "Context" 4 (valToDoc . scriptContextToValue $ sc)
        $+$ hang "Configuration" 4 (ppDoc conf)
        $+$ hang "Inputs" 4 dumpInputs
        $+$ hang "Logs" 4 (dumpLogs logs)
    dumpInputs :: Doc
    dumpInputs = case td of
      SpendingTest d r v ->
        "Datum"
          $+$ ppDoc d
          $+$ "Redeemer"
          $+$ ppDoc r
          $+$ "Value"
          $+$ ppDoc v
      MintingTest r ->
        "Redeemer" $+$ ppDoc r

deliverResult ::
  forall (p :: Purpose).
  PlutusTracing ->
  Outcome ->
  [Text] ->
  TransactionConfig ->
  ScriptContext ->
  TestData p ->
  Result
deliverResult shouldChat expected logs conf sc td =
  case (expected, lastMay logs >>= Text.stripPrefix "tasty-plutus: ") of
    (_, Nothing) -> testFailed noOutcome
    (Fail, Just "Pass") -> testFailed unexpectedSuccess
    (Fail, Just "Fail") -> doPass
    (Pass, Just "Pass") -> doPass
    (Pass, Just "Fail") -> testFailed unexpectedFailure
    (_, Just t) -> case Text.stripPrefix "Parse failed: " t of
      Nothing -> testFailed . internalError $ t
      Just t' -> testFailed . noParse $ t'
  where
    doPass :: Result
    doPass = testPassed $ case shouldChat of
      Always ->
        renderStyle ourStyle $
          ""
            $+$ hang "Logs" 4 (dumpLogs logs)
      OnlyOnFail -> ""
    noOutcome :: String
    noOutcome =
      renderStyle ourStyle $
        "No outcome from run"
          $+$ dumpState
          $+$ ""
          $+$ "Did you forget to use toTestValidator or toTestMintingPolicy?"
    unexpectedSuccess :: String
    unexpectedSuccess =
      renderStyle ourStyle $
        "Unexpected success" $+$ dumpState
    unexpectedFailure :: String
    unexpectedFailure =
      renderStyle ourStyle $
        "Unexpected failure" $+$ dumpState
    internalError :: Text -> String
    internalError msg =
      renderStyle ourStyle $
        ("Internal error" <+> (text . show $ msg)) $+$ dumpState
    noParse :: Text -> String
    noParse what =
      renderStyle ourStyle $
        ((text . show $ what) <+> "did not parse") $+$ dumpState
    dumpState :: Doc
    dumpState =
      ""
        $+$ hang "Context" 4 (valToDoc . scriptContextToValue $ sc)
        $+$ hang "Configuration" 4 (ppDoc conf)
        $+$ hang "Inputs" 4 dumpInputs
        $+$ hang "Logs" 4 (dumpLogs logs)
    dumpInputs :: Doc
    dumpInputs = case td of
      SpendingTest d r v ->
        "Datum"
          $+$ ppDoc d
          $+$ "Redeemer"
          $+$ ppDoc r
          $+$ "Value"
          $+$ ppDoc v
      MintingTest r ->
        "Redeemer" $+$ ppDoc r

dumpLogs :: [Text] -> Doc
dumpLogs = vcat . fmap go . zip [1 ..]
  where
    go :: (Int, Text) -> Doc
    go (ix, line) = (int ix <> colon) <+> (text . show $ line)
