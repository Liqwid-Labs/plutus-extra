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
 >    shouldValidateTracing "Gotta get good messages" tracePred validData validContext
 >    shouldn'tValidateTracing "Oh damn" tracePred invalidData validContext
 >    ...
-}
module Test.Tasty.Plutus.Script.Unit (
  -- * Testing API
  shouldValidate,
  shouldn'tValidate,
  shouldValidateTracing,
  shouldn'tValidateTracing,
) where

import Control.Monad.Reader (asks)
import Control.Monad.Writer (tell)
import Data.Proxy (Proxy (Proxy))
import Data.Sequence qualified as Seq
import Data.Tagged (Tagged (Tagged))
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Vector (Vector)
import Data.Vector qualified as Vector
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
    tt <- asks (singleTest name . Spender Pass Nothing td cb)
    tell . Seq.singleton $ tt
  MintingTest {} -> WithMinting $ do
    tt <- asks (singleTest name . Minter Pass Nothing td cb)
    tell . Seq.singleton $ tt

{- | Specify that, given this test data and context, as well as a predicate on
 the entire trace:

 * The validation should succeed; and
 * The trace that results should satisfy the predicate.

 @since 3.3
-}
shouldValidateTracing ::
  forall (p :: Purpose).
  (Typeable p) =>
  String ->
  (Vector Text -> Bool) ->
  TestData p ->
  ContextBuilder p ->
  WithScript p ()
shouldValidateTracing name f td cb = case td of
  SpendingTest {} -> WithSpending $ do
    tt <- asks (singleTest name . Spender Pass (Just f) td cb)
    tell . Seq.singleton $ tt
  MintingTest {} -> WithMinting $ do
    tt <- asks (singleTest name . Minter Pass (Just f) td cb)
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
    tt <- asks (singleTest name . Spender Fail Nothing td cb)
    tell . Seq.singleton $ tt
  MintingTest {} -> WithMinting $ do
    tt <- asks (singleTest name . Minter Fail Nothing td cb)
    tell . Seq.singleton $ tt

{- | Specify that, given this test data and context, as well as a predicate on
 the entire trace:

 * The validation should fail; and
 * The resulting trace should satisfy the predicate.

 @since 3.3
-}
shouldn'tValidateTracing ::
  forall (p :: Purpose).
  (Typeable p) =>
  String ->
  (Vector Text -> Bool) ->
  TestData p ->
  ContextBuilder p ->
  WithScript p ()
shouldn'tValidateTracing name f td cb = case td of
  SpendingTest {} -> WithSpending $ do
    tt <- asks (singleTest name . Spender Fail (Just f) td cb)
    tell . Seq.singleton $ tt
  MintingTest {} -> WithMinting $ do
    tt <- asks (singleTest name . Minter Fail (Just f) td cb)
    tell . Seq.singleton $ tt

-- Helpers

data Outcome = Fail | Pass

data ScriptTest (p :: Purpose) where
  Spender ::
    Outcome ->
    Maybe (Vector Text -> Bool) ->
    TestData 'ForSpending ->
    ContextBuilder 'ForSpending ->
    Validator ->
    ScriptTest 'ForSpending
  Minter ::
    Outcome ->
    Maybe (Vector Text -> Bool) ->
    TestData 'ForMinting ->
    ContextBuilder 'ForMinting ->
    MintingPolicy ->
    ScriptTest 'ForMinting

instance (Typeable p) => IsTest (ScriptTest p) where
  run opts vt _ = pure $ case vt of
    Spender expected mPred td@(SpendingTest d r v) cb val ->
      let context = compileSpending conf cb d v
          context' = Context . toBuiltinData $ context
          d' = Datum . toBuiltinData $ d
          r' = Redeemer . toBuiltinData $ r
       in case runScript context' val d' r' of
            Left err -> handleError mPred shouldChat expected conf context td err
            Right (_, logs) ->
              deliverResult mPred shouldChat expected logs conf context td
    Minter expected mPred td@(MintingTest r) cb mp ->
      let context = compileMinting conf cb
          context' = Context . toBuiltinData $ context
          r' = Redeemer . toBuiltinData $ r
       in case runMintingPolicyScript context' mp r' of
            Left err -> handleError mPred shouldChat expected conf context td err
            Right (_, logs) ->
              deliverResult mPred shouldChat expected logs conf context td
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
  Maybe (Vector Text -> Bool) ->
  PlutusTracing ->
  Outcome ->
  TransactionConfig ->
  ScriptContext ->
  TestData p ->
  ScriptError ->
  Result
handleError mPred shouldChat expected conf sc td = \case
  EvaluationError logs msg -> case expected of
    Pass -> testFailed . unexpectedFailure msg $ logs
    Fail -> case mPred of
      Nothing -> doPass logs
      Just f ->
        let logs' = Vector.fromList logs
         in if f logs'
              then doPass logs
              else testFailed . didn'tLog $ logs
  {-
  Fail -> testPassed $ case shouldChat of
    Always ->
      renderStyle ourStyle $
        ""
          $+$ hang "Logs" 4 (dumpLogs logs)
    OnlyOnFail -> ""
  -}
  EvaluationException name msg ->
    testFailed . renderStyle ourStyle $
      "Unexpected behaviour in script:" <+> text name
        $+$ hang "Description" 4 (text msg)
  MalformedScript msg ->
    testFailed . renderStyle ourStyle $
      "Script was malformed"
        $+$ hang "Details" 4 (text msg)
  where
    doPass :: [Text] -> Result
    doPass logs = testPassed $ case shouldChat of
      Always ->
        renderStyle ourStyle $
          ""
            $+$ hang "Logs" 4 (dumpLogs logs)
      OnlyOnFail -> ""
    didn'tLog :: [Text] -> String
    didn'tLog logs =
      renderStyle ourStyle $
        "Trace did not contain expected contents"
          $+$ dumpState logs
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
  Maybe (Vector Text -> Bool) ->
  PlutusTracing ->
  Outcome ->
  [Text] ->
  TransactionConfig ->
  ScriptContext ->
  TestData p ->
  Result
deliverResult mPred shouldChat expected logs conf sc td =
  case (expected, lastMay logs >>= Text.stripPrefix "tasty-plutus: ") of
    (_, Nothing) -> testFailed noOutcome
    (Fail, Just "Pass") -> testFailed unexpectedSuccess
    (Fail, Just "Fail") -> tryPass
    (Pass, Just "Pass") -> tryPass
    (Pass, Just "Fail") -> testFailed unexpectedFailure
    (_, Just t) -> case Text.stripPrefix "Parse failed: " t of
      Nothing -> testFailed . internalError $ t
      Just t' -> testFailed . noParse $ t'
  where
    tryPass :: Result
    tryPass = case mPred of
      Nothing -> doPass
      Just f ->
        let logs' = Vector.fromList logs
         in if f logs'
              then doPass
              else testFailed didn'tLog
    doPass :: Result
    doPass = testPassed $ case shouldChat of
      Always ->
        renderStyle ourStyle $
          ""
            $+$ hang "Logs" 4 (dumpLogs logs)
      OnlyOnFail -> ""
    didn'tLog :: String
    didn'tLog =
      renderStyle ourStyle $
        "Trace did not contain expected contents"
          $+$ dumpState
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
