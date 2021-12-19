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

import Control.Arrow ((>>>))
import Control.Monad.Reader (Reader, asks, runReader)
import Control.Monad.Writer (tell)
import Data.Kind (Type)
import Data.Proxy (Proxy (Proxy))
import Data.Sequence qualified as Seq
import Data.Tagged (Tagged (Tagged))
import Data.Text (Text)
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import Plutus.V1.Ledger.Api (MintingPolicy, ScriptContext, Validator)
import Plutus.V1.Ledger.Scripts (
  ScriptError (
    EvaluationError,
    EvaluationException,
    MalformedScript
  ),
 )
import Test.Tasty.Options (
  OptionDescription (Option),
  OptionSet,
  lookupOption,
 )
import Test.Tasty.Plutus.Internal.Context (
  ContextBuilder,
  Purpose (ForMinting, ForSpending),
  TransactionConfig,
 )
import Test.Tasty.Plutus.Internal.Env (
  SomeScript (SomeMinter, SomeSpender),
  getContext,
  getScriptContext,
  getScriptResult,
  prepareConf,
 )
import Test.Tasty.Plutus.Internal.Feedback (
  didn'tLog,
  doPass,
  dumpState,
  internalError,
  malformedScript,
  noOutcome,
  noParse,
  scriptException,
  unexpectedFailure,
  unexpectedSuccess,
 )
import Test.Tasty.Plutus.Internal.Run (
  ScriptResult (
    InternalError,
    NoOutcome,
    ParseFailed,
    ScriptFailed,
    ScriptPassed
  ),
 )
import Test.Tasty.Plutus.Internal.WithScript (
  WithScript (WithMinting, WithSpending),
 )
import Test.Tasty.Plutus.Options (
  Fee,
  PlutusTracing,
  ScriptInputPosition,
  TestCurrencySymbol,
  TestTxId,
  TestValidatorHash,
  TimeRange,
 )
import Test.Tasty.Plutus.TestData (
  Outcome (Fail, Pass),
  TestData (MintingTest, SpendingTest),
 )
import Test.Tasty.Providers (
  IsTest (run, testOptions),
  Result,
  singleTest,
  testFailed,
  testPassed,
 )
import Text.PrettyPrint (Doc)
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
shouldValidate = addUnitTest Pass Nothing

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
shouldValidateTracing name f = addUnitTest Pass (Just f) name

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
shouldn'tValidate = addUnitTest Fail Nothing

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
shouldn'tValidateTracing name f = addUnitTest Fail (Just f) name

-- Helpers

addUnitTest ::
  forall (p :: Purpose).
  (Typeable p) =>
  Outcome ->
  Maybe (Vector Text -> Bool) ->
  String ->
  TestData p ->
  ContextBuilder p ->
  WithScript p ()
addUnitTest out trace name td cb = case td of
  SpendingTest {} -> WithSpending $ do
    tt <- asks (singleTest name . Spender out trace td cb)
    tell . Seq.singleton $ tt
  MintingTest {} -> WithMinting $ do
    tt <- asks (singleTest name . Minter out trace td cb)
    tell . Seq.singleton $ tt

data ScriptTest (p :: Purpose) where
  Spender ::
    forall (d :: Type) (r :: Type).
    Outcome ->
    Maybe (Vector Text -> Bool) ->
    TestData ( 'ForSpending d r) ->
    ContextBuilder ( 'ForSpending d r) ->
    Validator ->
    ScriptTest ( 'ForSpending d r)
  Minter ::
    forall (r :: Type).
    Outcome ->
    Maybe (Vector Text -> Bool) ->
    TestData ( 'ForMinting r) ->
    ContextBuilder ( 'ForMinting r) ->
    MintingPolicy ->
    ScriptTest ( 'ForMinting r)

data UnitEnv (p :: Purpose) = UnitEnv
  { envOpts :: OptionSet
  , envScriptTest :: ScriptTest p
  }

getShouldChat ::
  forall (p :: Purpose).
  UnitEnv p ->
  PlutusTracing
getShouldChat = lookupOption . envOpts

getConf ::
  forall (p :: Purpose).
  UnitEnv p ->
  TransactionConfig
getConf = prepareConf envOpts

getCB ::
  forall (p :: Purpose).
  UnitEnv p ->
  ContextBuilder p
getCB =
  envScriptTest >>> \case
    Spender _ _ _ cb _ -> cb
    Minter _ _ _ cb _ -> cb

getTestData ::
  forall (p :: Purpose).
  UnitEnv p ->
  TestData p
getTestData =
  envScriptTest >>> \case
    Spender _ _ td _ _ -> td
    Minter _ _ td _ _ -> td

getScript ::
  forall (p :: Purpose).
  UnitEnv p ->
  SomeScript p
getScript =
  envScriptTest >>> \case
    Spender _ _ _ _ val -> SomeSpender val
    Minter _ _ _ _ mp -> SomeMinter mp

getMPred ::
  forall (p :: Purpose).
  UnitEnv p ->
  Maybe (Vector Text -> Bool)
getMPred =
  envScriptTest >>> \case
    Spender _ mPred _ _ _ -> mPred
    Minter _ mPred _ _ _ -> mPred

getExpected ::
  forall (p :: Purpose).
  UnitEnv p ->
  Outcome
getExpected =
  envScriptTest >>> \case
    Spender expected _ _ _ _ -> expected
    Minter expected _ _ _ _ -> expected

getSC ::
  forall (p :: Purpose).
  UnitEnv p ->
  ScriptContext
getSC = getScriptContext getConf getCB getTestData

getDumpedState ::
  forall (p :: Purpose).
  [Text] ->
  UnitEnv p ->
  Doc
getDumpedState = dumpState getConf getCB getTestData

instance (Typeable p) => IsTest (ScriptTest p) where
  run opts vt _ = pure $ case result of
    Left err -> runReader (handleError err) env
    Right (logs, sr) -> runReader (deliverResult logs sr) env
    where
      env :: UnitEnv p
      env =
        UnitEnv
          { envOpts = opts
          , envScriptTest = vt
          }
      result :: Either ScriptError ([Text], ScriptResult)
      result = getScriptResult getScript getTestData (getContext getSC) env
  testOptions =
    Tagged
      [ Option @Fee Proxy
      , Option @TimeRange Proxy
      , Option @TestTxId Proxy
      , Option @TestCurrencySymbol Proxy
      , Option @TestValidatorHash Proxy
      , Option @PlutusTracing Proxy
      , Option @ScriptInputPosition Proxy
      ]

handleError ::
  forall (p :: Purpose).
  ScriptError ->
  Reader (UnitEnv p) Result
handleError = \case
  EvaluationError logs msg ->
    asks getExpected >>= \case
      Pass -> asks (testFailed . unexpectedFailure (getDumpedState logs) msg)
      Fail -> asks getMPred >>= (`tryPass` logs)
  EvaluationException name msg -> pure . testFailed $ scriptException name msg
  MalformedScript msg -> pure . testFailed $ malformedScript msg

deliverResult ::
  forall (p :: Purpose).
  [Text] ->
  ScriptResult ->
  Reader (UnitEnv p) Result
deliverResult logs result = do
  expected <- asks getExpected
  case (expected, result) of
    (_, NoOutcome) -> asks (testFailed . noOutcome state)
    (Fail, ScriptPassed) -> asks (testFailed . unexpectedSuccess state)
    (Fail, ScriptFailed) -> asks getMPred >>= (`tryPass` logs)
    (Pass, ScriptPassed) -> asks getMPred >>= (`tryPass` logs)
    (Pass, ScriptFailed) -> asks (testFailed . unexpectedFailure state mempty)
    (_, InternalError t) -> asks (testFailed . internalError state t)
    (_, ParseFailed t) -> asks (testFailed . noParse state t)
  where
    state :: UnitEnv p -> Doc
    state = getDumpedState logs

tryPass :: Maybe (Vector Text -> Bool) -> [Text] -> Reader (UnitEnv p) Result
tryPass mPred logs = case mPred of
  Nothing -> asks (testPassed . doPass getShouldChat logs)
  Just f ->
    let logs' = Vector.fromList logs
     in if f logs'
          then asks (testPassed . doPass getShouldChat logs)
          else asks (testFailed . didn'tLog (getDumpedState logs))
