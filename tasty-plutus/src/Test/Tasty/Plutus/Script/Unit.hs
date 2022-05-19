{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE Trustworthy #-}

{- |
 Module: Test.Tasty.Plutus.Script.Unit
 Copyright: (C) MLabs 2021-2022
 License: Apache 2.0
 Maintainer: Koz Ross <koz@mlabs.city>
 Portability: GHC only
 Stability: Experimental

 A unit-test-like interface for validator and minting policy testing.

 = Example usage

 > validatorTests :: TestTree
 > validatorTests = withTestScript "Testing my spending" myValidator $ do
 >    shouldValidate "Valid case" validData validContext
 >    shouldn'tValidate "Invalid context" validData invalidContext
 >    shouldn'tValidate "Invalid data" invalidData validContext
 >    shouldn'tValidate "Everything is bad" invalidData invalidContext
 >    shouldValidateTracing "Gotta get good messages" tracePred validData validContext
 >    shouldn'tValidateTracing "Oh damn" tracePred invalidData validContext
 >    ...

 = Example with whole-transaction testing

 > validatorTests :: TestTree
 > validatorTests = do
 >    shouldValidateTransaction "Valid transaction" validData mintingPolicies validTransactionContext
 >    shouldValidateTransaction "Valid inputs" validData mempty validTransactionContext
 >    shouldn'tValidateTransaction "Invalid transaction" validData mintingPolicies invalidTransactionContext
 >    ...
-}
module Test.Tasty.Plutus.Script.Unit (
  -- * Testing API
  shouldValidate,
  shouldn'tValidate,
  shouldValidateTracing,
  shouldn'tValidateTracing,

  -- ** Whole-transaction testing
  shouldValidateTransaction,
  shouldn'tValidateTransaction,
  shouldValidateTransactionTracing,
  shouldn'tValidateTransactionTracing,
  SomeMintingPolicy (SomeMintingPolicy),
) where

import Control.Arrow ((>>>))
import Control.Monad.Reader (Reader, asks, runReader)
import Control.Monad.Writer (tell)
import Data.Kind (Type)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Monoid (Ap (Ap, getAp))
import Data.Proxy (Proxy (Proxy))
import Data.Sequence qualified as Seq
import Data.Tagged (Tagged (Tagged))
import Data.Text (Text)
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import Plutus.V1.Ledger.Api (
  CurrencySymbol,
  ExBudget (ExBudget),
  ExCPU (ExCPU),
  ExMemory (ExMemory),
  ScriptContext,
  toBuiltinData,
  unsafeFromBuiltinData,
 )
import Plutus.V1.Ledger.Scripts (
  ScriptError (
    EvaluationError,
    EvaluationException,
    MalformedScript
  ),
 )
import Plutus.V1.Ledger.Value (TokenName, Value, getValue)
import PlutusTx.AssocMap qualified as AssocMap
import PlutusTx.Positive (Positive)
import Test.Plutus.ContextBuilder (
  ContextBuilder,
  ContextFragment (cfValidatorInputs, cfValidatorOutputs),
  MintingPolicyAction (BurnAction, MintAction),
  MintingPolicyTask (MPTask),
  Naming,
  Purpose (ForMinting, ForSpending, ForTransaction),
  SomeValidatedUTXO (SomeValidatedUTXO, someRedeemer, someSpendingScript, someUTxO),
  Tokens (Tokens),
  TransactionConfig,
  ValidatorUTXO (vUtxoDatum, vUtxoValue),
  ValidatorUTXOs (MultiValidatorUTXOs, NoValidatorUTXOs),
  foldBuilt,
  transactionMinting,
  transactionSpending,
 )
import Test.Tasty.Options (
  OptionDescription (Option),
  OptionSet,
  lookupOption,
 )
import Test.Tasty.Plutus.Instances (ResultJoin (ResultJoin, getResultJoin))
import Test.Tasty.Plutus.Internal.Env (
  SomeScript (SomeMinter, SomeSpender),
  getContext,
  getScriptContext,
  getScriptResult,
  prepareConf,
 )
import Test.Tasty.Plutus.Internal.Estimate (minterEstimate, spenderEstimate)
import Test.Tasty.Plutus.Internal.Feedback (
  didn'tLog,
  doPass,
  dumpState,
  errorNoEstimate,
  explainFailureEstimation,
  malformedScript,
  noParse,
  reportBudgets,
  scriptException,
  unexpectedFailure,
  unexpectedSuccess,
 )
import Test.Tasty.Plutus.Internal.Run (
  ScriptResult (
    ParseFailed,
    PlutusEvaluationException,
    PlutusMalformedScript,
    ScriptFailed
  ),
 )
import Test.Tasty.Plutus.Internal.TestScript (
  TestScript,
  getTestMintingPolicy,
  getTestValidator,
 )
import Test.Tasty.Plutus.Internal.WithScript (
  WithScript (WithMinting, WithSpending),
 )
import Test.Tasty.Plutus.Options (
  Fee,
  PlutusEstimate (EstimateOnly, NoEstimates),
  PlutusTracing,
  ScriptInputPosition,
  TestCurrencySymbol,
  TestTxId,
  TestValidatorHash,
  TimeRange,
 )
import Test.Tasty.Plutus.TestData (
  Outcome (Fail, Pass),
  SomeMintingPolicy (SomeMintingPolicy),
  TestData (MintingTest, SpendingTest),
  TestItems (ItemsForMinting, ItemsForSpending),
 )
import Test.Tasty.Providers (
  IsTest (run, testOptions),
  Result,
  TestTree,
  singleTest,
  testFailed,
  testPassed,
 )
import Text.PrettyPrint (Doc)
import Type.Reflection (Typeable)

import PlutusTx.Prelude ((-))
import Prelude hiding ((-))

{- | Specify that, given this test data and context, the validation should
 succeed.

 @since 9.0
-}
shouldValidate ::
  forall (p :: Purpose) (n :: Naming).
  (Typeable p, Typeable n) =>
  String ->
  TestData p ->
  ContextBuilder p n ->
  WithScript p ()
shouldValidate = addUnitTest Pass Nothing

{- | Specify that, given this test data and context, as well as a predicate on
 the entire trace:

 * The validation should succeed; and
 * The trace that results should satisfy the predicate.

 @since 9.0
-}
shouldValidateTracing ::
  forall (p :: Purpose) (n :: Naming).
  (Typeable p, Typeable n) =>
  String ->
  (Vector Text -> Bool) ->
  TestData p ->
  ContextBuilder p n ->
  WithScript p ()
shouldValidateTracing name f = addUnitTest Pass (Just f) name

{- | Specify that, given these minting policies and context, the whole
   transaction should succeed. Note that, if the transaction mints a currency
   whose minting policy is /not/ provided by the @mintingPolicies@ map, it is
   assumed to be passing.

 @since 9.1.1
-}
shouldValidateTransaction ::
  forall (n :: Naming).
  Typeable n =>
  String ->
  Map CurrencySymbol SomeMintingPolicy ->
  ContextBuilder 'ForTransaction n ->
  TestTree
shouldValidateTransaction name mintingPolicies cb =
  singleTest name (TransactionTester Pass Nothing mintingPolicies cb)

{- | Specify that, given these minting policies and context, the whole
   transaction should /not/ succeed. Note that, if the transaction mints a
   currency whose minting policy is /not/ provided by the @mintingPolicies@
   map, the policy is assumed to be passing.

 @since 9.1.1
-}
shouldn'tValidateTransaction ::
  forall (n :: Naming).
  Typeable n =>
  String ->
  Map CurrencySymbol SomeMintingPolicy ->
  ContextBuilder 'ForTransaction n ->
  TestTree
shouldn'tValidateTransaction name mintingPolicies cb =
  singleTest name (TransactionTester Fail Nothing mintingPolicies cb)

{- | Specify that, given these minting policies and context, as well as a
 predicate on the entire trace:

 * validation of every consumed input should succeed;
 * every policy specified in @mintingPolicies@ whose currency was minted or
   burned by the transaction should succeed; and
 * the trace that results should satisfy the predicate.

 @since 9.1.1
-}
shouldValidateTransactionTracing ::
  forall (n :: Naming).
  Typeable n =>
  String ->
  (Vector Text -> Bool) ->
  Map CurrencySymbol SomeMintingPolicy ->
  ContextBuilder 'ForTransaction n ->
  TestTree
shouldValidateTransactionTracing name f mintingPolicies cb =
  singleTest name (TransactionTester Pass (Just f) mintingPolicies cb)

{- | Specify that, given these minting policies and context, as well as a
 predicate on the entire trace, at least one of these criteria fails:

 * validation of an input consumed by the transaction;
 * a policy specified in @mintingPolicies@ whose currency was minted or
   burned by the transaction; or
 * the trace predicate.

 @since 9.1.1
-}
shouldn'tValidateTransactionTracing ::
  forall (n :: Naming).
  Typeable n =>
  String ->
  (Vector Text -> Bool) ->
  Map CurrencySymbol SomeMintingPolicy ->
  ContextBuilder 'ForTransaction n ->
  TestTree
shouldn'tValidateTransactionTracing name f mintingPolicies cb =
  singleTest name (TransactionTester Fail (Just f) mintingPolicies cb)

{- | Specify that, given this test data and context, the validation should fail.

 @since 9.0
-}
shouldn'tValidate ::
  forall (p :: Purpose) (n :: Naming).
  (Typeable p, Typeable n) =>
  String ->
  TestData p ->
  ContextBuilder p n ->
  WithScript p ()
shouldn'tValidate = addUnitTest Fail Nothing

{- | Specify that, given this test data and context, as well as a predicate on
 the entire trace:

 * The validation should fail; and
 * The resulting trace should satisfy the predicate.

 @since 9.0
-}
shouldn'tValidateTracing ::
  forall (p :: Purpose) (n :: Naming).
  (Typeable p, Typeable n) =>
  String ->
  (Vector Text -> Bool) ->
  TestData p ->
  ContextBuilder p n ->
  WithScript p ()
shouldn'tValidateTracing name f = addUnitTest Fail (Just f) name

-- Helpers

addUnitTest ::
  forall (p :: Purpose) (n :: Naming).
  (Typeable p, Typeable n) =>
  Outcome ->
  Maybe (Vector Text -> Bool) ->
  String ->
  TestData p ->
  ContextBuilder p n ->
  WithScript p ()
addUnitTest out trace name td cb = case td of
  SpendingTest {} -> WithSpending $ do
    tt <- asks (singleTest name . Spender out trace td cb)
    tell . Seq.singleton $ tt
  MintingTest {} -> WithMinting $ do
    tt <- asks (singleTest name . Minter out trace td cb)
    tell . Seq.singleton $ tt

data ScriptTest (p :: Purpose) (n :: Naming) where
  Spender ::
    forall (d :: Type) (r :: Type) (n :: Naming).
    Outcome ->
    Maybe (Vector Text -> Bool) ->
    TestData ( 'ForSpending d r) ->
    ContextBuilder ( 'ForSpending d r) n ->
    TestScript ( 'ForSpending d r) ->
    ScriptTest ( 'ForSpending d r) n
  Minter ::
    forall (r :: Type) (n :: Naming).
    Outcome ->
    Maybe (Vector Text -> Bool) ->
    TestData ( 'ForMinting r) ->
    ContextBuilder ( 'ForMinting r) n ->
    TestScript ( 'ForMinting r) ->
    ScriptTest ( 'ForMinting r) n
  TransactionTester ::
    forall (n :: Naming).
    Outcome ->
    Maybe (Vector Text -> Bool) ->
    Map CurrencySymbol SomeMintingPolicy ->
    ContextBuilder 'ForTransaction n ->
    ScriptTest 'ForTransaction n

getOutcome ::
  forall (p :: Purpose) (n :: Naming).
  ScriptTest p n ->
  Outcome
getOutcome = \case
  Spender out _ _ _ _ -> out
  Minter out _ _ _ _ -> out
  TransactionTester out _ _ _ -> out

data UnitEnv (p :: Purpose) (n :: Naming) = UnitEnv
  { envOpts :: OptionSet
  , envScriptTest :: ScriptTest p n
  }

getShouldChat ::
  forall (p :: Purpose) (n :: Naming).
  UnitEnv p n ->
  PlutusTracing
getShouldChat = lookupOption . envOpts

getConf ::
  forall (p :: Purpose) (n :: Naming).
  UnitEnv p n ->
  TransactionConfig
getConf = prepareConf envOpts

getCB ::
  forall (p :: Purpose) (n :: Naming).
  UnitEnv p n ->
  ContextBuilder p n
getCB =
  envScriptTest >>> \case
    Spender _ _ _ cb _ -> cb
    Minter _ _ _ cb _ -> cb
    TransactionTester _ _ _ cb -> cb

getTestData ::
  forall (p :: Purpose) (n :: Naming).
  UnitEnv p n ->
  TestData p
getTestData =
  envScriptTest >>> \case
    Spender _ _ td _ _ -> td
    Minter _ _ td _ _ -> td
    TransactionTester {} -> error "There's no test data in TransactionTester"

getScript ::
  forall (p :: Purpose) (n :: Naming).
  UnitEnv p n ->
  SomeScript p
getScript =
  envScriptTest >>> \case
    Spender _ _ _ _ val -> SomeSpender . getTestValidator $ val
    Minter _ _ _ _ mp -> SomeMinter . getTestMintingPolicy $ mp
    TransactionTester {} -> error "There's more than one script in TransactionTester"

getMPred ::
  forall (p :: Purpose) (n :: Naming).
  UnitEnv p n ->
  Maybe (Vector Text -> Bool)
getMPred =
  envScriptTest >>> \case
    Spender _ mPred _ _ _ -> mPred
    Minter _ mPred _ _ _ -> mPred
    TransactionTester _ mPred _ _ -> mPred

getExpected ::
  forall (p :: Purpose) (n :: Naming).
  UnitEnv p n ->
  Outcome
getExpected =
  envScriptTest >>> getOutcome

getSC ::
  forall (p :: Purpose) (n :: Naming).
  UnitEnv p n ->
  ScriptContext
getSC = getScriptContext getConf getCB getTestData

getDumpedState ::
  forall (p :: Purpose) (n :: Naming).
  [Text] ->
  UnitEnv p n ->
  Doc
getDumpedState = dumpState getConf getCB getTestData

instance (Typeable p, Typeable n) => IsTest (ScriptTest p n) where
  run opts tt@TransactionTester {} x =
    getResultJoin <$> getAp (foldTransactionTests (\t -> Ap $ ResultJoin <$> run opts t x) tt)
  run opts vt _ = pure $ case lookupOption opts of
    EstimateOnly -> case getOutcome vt of
      Fail -> testPassed explainFailureEstimation
      _ -> case tryEstimate of
        Left err -> testFailed $ case err of
          EvaluationError logs msg -> errorNoEstimate logs msg
          EvaluationException name msg -> scriptException name msg
          MalformedScript msg -> malformedScript msg
        Right (ExBudget (ExCPU bCPU) (ExMemory bMem)) ->
          testPassed $ reportBudgets (fromIntegral bCPU) (fromIntegral bMem)
    NoEstimates -> runReader go env
    where
      tryEstimate :: Either ScriptError ExBudget
      tryEstimate = case vt of
        Spender out _ td cb ts -> case td of
          SpendingTest dat red v ->
            let ti = ItemsForSpending dat red v cb out
             in spenderEstimate opts ts ti
        Minter out _ td cb ts -> case td of
          MintingTest red tasks ->
            let ti = ItemsForMinting red tasks cb out
             in minterEstimate opts ts ti
        TransactionTester {} -> error "Should have been eliminated above"
      {-
                | let context :: ContextFragment 'ForTransaction
                      context = foldBuilt cb
                      validatedInputs :: Map Text SomeValidatedUTXO
                      validatedInputs = case cfValidatorInputs context of
                        MultiValidatorUTXOs inputs -> inputs
                        NoValidatorUTXOs -> mempty
                      utxosTotalValue :: ValidatorUTXOs 'ForTransaction -> Value
                      utxosTotalValue NoValidatorUTXOs = mempty
                      utxosTotalValue (MultiValidatorUTXOs utxos) = foldMap theValue utxos
                        where
                          theValue SomeValidatedUTXO {someUTxO = x} = vUtxoValue x
                      utxoSpenderEstimate :: SomeValidatedUTXO -> Ap (Either ScriptError) ExBudget
                      utxoSpenderEstimate = undefined
                      utxoMinterEstimate :: Value -> Ap (Either ScriptError) ExBudget
                      utxoMinterEstimate = undefined
                  ->
                  getAp
                    ( foldMap utxoSpenderEstimate validatedInputs
                        <> utxoMinterEstimate
                          ( utxosTotalValue (cfValidatorOutputs context)
                              - utxosTotalValue (cfValidatorInputs context)
                          )
                    )
      -}
      go :: Reader (UnitEnv p n) Result
      go = case getScriptResult getScript getTestData (getContext getSC) env of
        Left err -> handleError err
        Right logs -> deliverResult logs
      env :: UnitEnv p n
      env =
        UnitEnv
          { envOpts = opts
          , envScriptTest = vt
          }
  testOptions =
    Tagged
      [ Option @Fee Proxy
      , Option @TimeRange Proxy
      , Option @TestTxId Proxy
      , Option @TestCurrencySymbol Proxy
      , Option @TestValidatorHash Proxy
      , Option @PlutusTracing Proxy
      , Option @ScriptInputPosition Proxy
      , Option @PlutusEstimate Proxy
      ]

foldTransactionTests ::
  forall a (n :: Naming).
  Monoid a =>
  (forall (p :: Purpose). Typeable p => ScriptTest p n -> a) ->
  ScriptTest 'ForTransaction n ->
  a
foldTransactionTests test (TransactionTester outcome mPred mintScripts cb) =
  foldMap testSpending validatedInputs
    <> foldMap testMinting (AssocMap.toList $ getValue valueDifference)
  where
    context :: ContextFragment 'ForTransaction
    context = foldBuilt cb
    valueDifference :: Value
    valueDifference = utxosTotalValue (cfValidatorOutputs context) - utxosTotalValue (cfValidatorInputs context)
    testMinting :: (CurrencySymbol, AssocMap.Map TokenName Integer) -> a
    testMinting (symbol, tokenAmounts) =
      case Map.lookup symbol mintScripts of
        Nothing -> mempty
        Just (SomeMintingPolicy mp redeemer)
          | let testPair (name, amount)
                  | amount < 0 = testPositive BurnAction name (toPositive $ negate amount)
                  | amount > 0 = testPositive MintAction name (toPositive amount)
                  | otherwise = mempty
                testPositive action name amount =
                  test $
                    Minter
                      outcome
                      mPred
                      (MintingTest redeemer $ pure $ MPTask action $ Tokens name amount)
                      (transactionMinting mp cb)
                      mp
                toPositive :: Integer -> Positive
                toPositive = unsafeFromBuiltinData . toBuiltinData ->
            foldMap testPair (AssocMap.toList tokenAmounts)
    testSpending :: SomeValidatedUTXO -> a
    testSpending SomeValidatedUTXO {someUTxO, someRedeemer, someSpendingScript} =
      test $
        Spender
          outcome
          mPred
          (SpendingTest (vUtxoDatum someUTxO) someRedeemer (vUtxoValue someUTxO))
          (transactionSpending someSpendingScript someUTxO cb)
          someSpendingScript
    validatedInputs :: Map Text SomeValidatedUTXO
    validatedInputs = case cfValidatorInputs context of
      MultiValidatorUTXOs inputs -> inputs
      NoValidatorUTXOs -> mempty
    utxosTotalValue :: ValidatorUTXOs 'ForTransaction -> Value
    utxosTotalValue NoValidatorUTXOs = mempty
    utxosTotalValue (MultiValidatorUTXOs utxos) = foldMap theValue utxos
      where
        theValue SomeValidatedUTXO {someUTxO = x} = vUtxoValue x

handleError ::
  forall (p :: Purpose) (n :: Naming).
  ScriptResult ->
  Reader (UnitEnv p n) Result
handleError = \case
  ScriptFailed logs msg ->
    asks getExpected >>= \case
      Pass -> asks (testFailed . unexpectedFailure (getDumpedState logs) msg)
      Fail -> asks getMPred >>= (`tryPass` logs)
  ParseFailed logs t ->
    asks (testFailed . noParse (getDumpedState logs) t)
  PlutusEvaluationException name msg ->
    pure . testFailed $ scriptException name msg
  PlutusMalformedScript msg ->
    pure . testFailed $ malformedScript msg

deliverResult ::
  forall (p :: Purpose) (n :: Naming).
  [Text] ->
  Reader (UnitEnv p n) Result
deliverResult logs = do
  expected <- asks getExpected
  case expected of
    Fail -> asks (testFailed . unexpectedSuccess (getDumpedState logs))
    Pass -> asks getMPred >>= (`tryPass` logs)

tryPass ::
  forall (p :: Purpose) (n :: Naming).
  Maybe (Vector Text -> Bool) ->
  [Text] ->
  Reader (UnitEnv p n) Result
tryPass mPred logs = case mPred of
  Nothing -> asks (testPassed . doPass getShouldChat logs)
  Just f ->
    let logs' = Vector.fromList logs
     in if f logs'
          then asks (testPassed . doPass getShouldChat logs)
          else asks (testFailed . didn'tLog (getDumpedState logs))
