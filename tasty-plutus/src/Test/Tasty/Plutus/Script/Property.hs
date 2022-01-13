{-# LANGUAGE Trustworthy #-}

{- |
 Module: Test.Tasty.Plutus.Script.Property
 Copyright: (C) MLabs 2021
 License: Apache 2.0
 Maintainer: Koz Ross <koz@mlabs.city>
 Portability: GHC only
 Stability: Experimental

 Generates QuickCheck property tests for validator and minting policy testing.

 = Example usage

 > myProperties :: TestTree
 > myProperties = withValidator "Property testing my spending" myValidator $ do
 >   scriptProperty "Some spending property" $ GenForSpending gen' transform'
 >   scriptProperty "Some minting property" $ GenForMinting gen'' transform''
 >   scriptPropertyPass "Validator succeeds" $ GenForSpending genPass transformPass
 >   scriptPropertyFail "MintingPolicy fails" $ GenForMinting genFail transformFail
 >   ...

 A small example of using can be found
 <https://github.com/Liqwid-Labs/plutus-extra/tasty-plutus/test/Properties/Main.hs here>

 = Note

 In general, the purpose of scriptProperty is to test the stable behavior
 of the script for various inputs. Not independent, but mutually consistent
 random data are transferred to the script. Using the expected outcome
 allows us to control the correctness of the result of running the script.
-}
module Test.Tasty.Plutus.Script.Property (
  scriptProperty,
  scriptPropertyFail,
  scriptPropertyPass,
  validatorProperty,
  validatorPropertyPass,
  validatorPropertyFail,
  mintingPolicyProperty,
  mintingPolicyPropertyPass,
  mintingPolicyPropertyFail,
) where

import Control.Monad.RWS.Strict (tell)
import Control.Monad.Reader (Reader, ask, asks, runReader)
import Data.Kind (Type)
import Data.Proxy (Proxy (Proxy))
import Data.Sequence qualified as Seq
import Data.Tagged (Tagged (Tagged))
import Data.Text (Text)
import Plutus.V1.Ledger.Contexts (ScriptContext)
import Plutus.V1.Ledger.Scripts (ScriptError (EvaluationError, EvaluationException, MalformedScript))
import Test.QuickCheck (
  Args (maxSize, maxSuccess),
  Gen,
  Property,
  Result (Failure, GaveUp, NoExpectedFailure, Success),
  checkCoverage,
  counterexample,
  cover,
  forAllShrinkShow,
  property,
  quickCheckWithResult,
  stdArgs,
 )
import Test.Tasty (TestTree)
import Test.Tasty.Options (OptionDescription (Option), OptionSet, lookupOption)
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
  dumpState',
  internalError,
  malformedScript,
  noOutcome,
  noParse,
  ourStyle,
  scriptException,
  unexpectedFailure,
  unexpectedSuccess,
 )
import Test.Tasty.Plutus.Internal.Options (
  PropertyMaxSize (PropertyMaxSize),
  PropertyTestCount (PropertyTestCount),
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
import Test.Tasty.Plutus.Internal.TestScript (
  TestMintingPolicy,
  TestValidator,
  getTestMintingPolicy,
  getTestValidator,
 )
import Test.Tasty.Plutus.Internal.WithScript (
  WithScript (WithMinting, WithSpending),
 )
import Test.Tasty.Plutus.Options (
  Fee,
  ScriptInputPosition,
  TestCurrencySymbol,
  TestTxId,
  TestValidatorHash,
  TimeRange,
 )
import Test.Tasty.Plutus.TestData (
  Generator (GenForMinting, GenForSpending),
  Methodology (Methodology),
  Outcome (Fail, Pass),
  TestData (MintingTest, SpendingTest),
  TestItems (
    ItemsForMinting,
    ItemsForSpending,
    mpCB,
    mpOutcome,
    mpRedeemer,
    mpTasks,
    spendCB,
    spendDatum,
    spendOutcome,
    spendRedeemer,
    spendValue
  ),
 )
import Test.Tasty.Providers (
  IsTest (run, testOptions),
  singleTest,
  testFailed,
  testPassed,
 )
import Text.PrettyPrint (
  Doc,
  hang,
  renderStyle,
  text,
  ($+$),
 )
import Text.Show.Pretty (ppDoc)
import Type.Reflection (Typeable)
import Prelude

{- | Given a 'Generator' containig a way to generate a seed,
and a function to create 'TestItems' from the seed, check that:

 * For any 'TestItems' with Outcome equals 'Pass', the script succeeds; and
 * For any 'TestItems' with Outcome equals 'Fail', the script fails.

 This will also check /coverage/: specifically, the property will fail unless
 the provided way produces roughly equal numbers of 'Pass' and
 'Fail'-classified cases.

 @since 5.0
-}
scriptProperty ::
  forall (a :: Type) (p :: Purpose).
  (Typeable a, Typeable p) =>
  -- | Property name
  String ->
  -- | Data generator
  Generator a p ->
  WithScript p ()
scriptProperty = mkScriptPropertyWith OutcomeDependent

{- | Given a 'Generator' containig a way to generate a seed,
and a function to create 'TestItems' from the seed, check that:

 * For any 'TestItems' the script always fails.

 This test ignores 'Outcome' from 'TestItems' and changes it to 'Fail'

 @since 5.0
-}
scriptPropertyFail ::
  forall (a :: Type) (p :: Purpose).
  (Typeable a, Typeable p) =>
  -- | Property name
  String ->
  -- | Data generator
  Generator a p ->
  WithScript p ()
scriptPropertyFail = mkScriptPropertyWith OutcomeAlwaysFail

{- | Given a 'Generator' containig a way to generate a seed,
and a function to create 'TestItems' from the seed, check that:

 * For any 'TestItems' the script always succeeds.

 This test ignores 'Outcome' from 'TestItems' and changes it to 'Pass'

 @since 5.0
-}
scriptPropertyPass ::
  forall (a :: Type) (p :: Purpose).
  (Typeable a, Typeable p) =>
  -- | Property name
  String ->
  -- | Data generator
  Generator a p ->
  WithScript p ()
scriptPropertyPass = mkScriptPropertyWith OutcomeAlwaysPass

-- Helpers

mkScriptPropertyWith ::
  forall (a :: Type) (p :: Purpose).
  (Typeable a, Typeable p) =>
  OutcomeKind ->
  -- | Property name
  String ->
  -- | Data generator
  Generator a p ->
  WithScript p ()
mkScriptPropertyWith outKind name generator = case generator of
  GenForSpending (Methodology gen shrinker) fTi ->
    WithSpending $ do
      val <- ask
      tell
        . Seq.singleton
        . singleTest name
        $ Spender gen shrinker (const val) fTi outKind
  GenForMinting (Methodology gen shrinker) fTi ->
    WithMinting $ do
      mp <- ask
      tell
        . Seq.singleton
        . singleTest name
        $ Minter gen shrinker (const mp) fTi outKind

data PropertyTest (a :: Type) (p :: Purpose) where
  Spender ::
    forall (a :: Type) (d :: Type) (r :: Type).
    Typeable a =>
    Gen a ->
    (a -> [a]) ->
    (a -> TestValidator d r) ->
    (a -> TestItems ( 'ForSpending d r)) ->
    OutcomeKind ->
    PropertyTest a ( 'ForSpending d r)
  Minter ::
    forall (a :: Type) (r :: Type).
    Typeable a =>
    Gen a ->
    (a -> [a]) ->
    (a -> TestMintingPolicy r) ->
    (a -> TestItems ( 'ForMinting r)) ->
    OutcomeKind ->
    PropertyTest a ( 'ForMinting r)

data OutcomeKind
  = OutcomeAlwaysFail
  | OutcomeAlwaysPass
  | OutcomeDependent
  deriving stock (Show)

adjustOutcome :: OutcomeKind -> Outcome -> Outcome
adjustOutcome OutcomeAlwaysFail = const Fail
adjustOutcome OutcomeAlwaysPass = const Pass
adjustOutcome OutcomeDependent = id

adjustCoverage :: OutcomeKind -> Outcome -> Property -> Property
adjustCoverage OutcomeAlwaysFail _ = id
adjustCoverage OutcomeAlwaysPass _ = id
adjustCoverage OutcomeDependent outcome =
  checkCoverage . cover 45.0 (outcome == Pass) "Successful validation"

data PropertyEnv (p :: Purpose) = PropertyEnv
  { envOpts :: OptionSet
  , envScript :: SomeScript p
  , envTestData :: TestData p
  , envContextBuilder :: ContextBuilder p
  , envOutcome :: Outcome
  }

getConf ::
  forall (p :: Purpose).
  PropertyEnv p ->
  TransactionConfig
getConf = prepareConf envOpts

getSC ::
  forall (p :: Purpose).
  PropertyEnv p ->
  ScriptContext
getSC = getScriptContext getConf envContextBuilder envTestData

getDumpedState ::
  forall (p :: Purpose).
  [Text] ->
  PropertyEnv p ->
  Doc
getDumpedState = dumpState' getConf envContextBuilder envTestData

instance (Show a, Typeable a, Typeable p) => IsTest (PropertyTest a p) where
  run opts vt _ = do
    let PropertyTestCount testCount' = lookupOption opts
    let PropertyMaxSize maxSize' = lookupOption opts
    let args = stdArgs {maxSuccess = testCount', maxSize = maxSize'}
    res <- quickCheckWithResult args go
    pure $ case res of
      Success {} -> testPassed ""
      GaveUp {} -> testFailed "Internal error: gave up."
      Failure {} -> testFailed ""
      NoExpectedFailure {} -> testFailed "Internal error: expected failure but saw none."
    where
      go :: Property
      go = case vt of
        Spender gen shrinker fVal fTi out ->
          forAllShrinkShow gen shrinker (prettySpender fTi out) $
            spenderProperty opts fVal fTi out
        Minter gen shrinker fMp fTi out ->
          forAllShrinkShow gen shrinker (prettyMinter fTi out) $
            minterProperty opts fMp fTi out
  testOptions =
    Tagged
      [ Option @Fee Proxy
      , Option @TimeRange Proxy
      , Option @TestTxId Proxy
      , Option @TestCurrencySymbol Proxy
      , Option @TestValidatorHash Proxy
      , Option @PropertyTestCount Proxy
      , Option @PropertyMaxSize Proxy
      , Option @ScriptInputPosition Proxy
      ]

spenderProperty ::
  forall (a :: Type) (d :: Type) (r :: Type).
  OptionSet ->
  (a -> TestValidator d r) ->
  (a -> TestItems ( 'ForSpending d r)) ->
  OutcomeKind ->
  a ->
  Property
spenderProperty opts fVal fTi outKind seed = case fTi seed of
  ItemsForSpending
    { spendDatum = d :: datum
    , spendRedeemer = r :: redeemer
    , spendValue = v
    , spendCB = cb
    , spendOutcome = outcome
    } ->
      let td = SpendingTest d r v
          script = SomeSpender . getTestValidator $ fVal seed
          outcome' = adjustOutcome outKind outcome
          env =
            PropertyEnv
              { envOpts = opts
              , envScript = script
              , envTestData = td
              , envContextBuilder = cb
              , envOutcome = outcome'
              }
       in adjustCoverage outKind outcome
            . (`runReader` env)
            . produceResult
            $ getScriptResult envScript envTestData (getContext getSC) env

prettySpender ::
  forall (a :: Type) (d :: Type) (r :: Type).
  (Show a) =>
  (a -> TestItems ( 'ForSpending d r)) ->
  OutcomeKind ->
  a ->
  String
prettySpender f outKind seed = case f seed of
  ItemsForSpending
    { spendDatum = d :: datum
    , spendRedeemer = r :: redeemer
    , spendValue = v
    , spendCB = cb
    , spendOutcome = outcome
    } ->
      let outcome' :: Outcome
          outcome' = adjustOutcome outKind outcome
          dumpInputs :: Doc
          dumpInputs =
            "Seed"
              $+$ ppDoc seed
              $+$ "Datum"
              $+$ ppDoc @datum d
              $+$ "Redeemer"
              $+$ ppDoc @redeemer r
              $+$ "Value"
              $+$ ppDoc v
              $+$ "ContextBuilder"
              $+$ ppDoc cb
       in renderStyle ourStyle $
            ""
              $+$ hang "Case" 4 (text . show $ outcome')
              $+$ hang "Inputs" 4 dumpInputs

minterProperty ::
  forall (a :: Type) (r :: Type).
  OptionSet ->
  (a -> TestMintingPolicy r) ->
  (a -> TestItems ( 'ForMinting r)) ->
  OutcomeKind ->
  a ->
  Property
minterProperty opts fMp fTi outKind seed = case fTi seed of
  ItemsForMinting
    { mpRedeemer = r
    , mpTasks = toks
    , mpCB = cb
    , mpOutcome = outcome
    } ->
      let td = MintingTest r toks
          script = SomeMinter . getTestMintingPolicy $ fMp seed
          outcome' = adjustOutcome outKind outcome
          env =
            PropertyEnv
              { envOpts = opts
              , envScript = script
              , envTestData = td
              , envContextBuilder = cb
              , envOutcome = outcome'
              }
       in adjustCoverage outKind outcome
            . (`runReader` env)
            . produceResult
            $ getScriptResult envScript envTestData (getContext getSC) env

prettyMinter ::
  forall (a :: Type) (r :: Type).
  (Show a) =>
  (a -> TestItems ( 'ForMinting r)) ->
  OutcomeKind ->
  a ->
  String
prettyMinter f outKind seed = case f seed of
  ItemsForMinting
    { mpRedeemer = r :: redeemer
    , mpTasks = ts
    , mpCB = cb
    , mpOutcome = outcome
    } ->
      let outcome' :: Outcome
          outcome' = adjustOutcome outKind outcome
          dumpInputs :: Doc
          dumpInputs =
            "Seed"
              $+$ ppDoc seed
              $+$ "Redeemer"
              $+$ ppDoc @redeemer r
              $+$ "MintingPolicyTasks"
              $+$ ppDoc ts
              $+$ "ContextBuilder"
              $+$ ppDoc cb
       in renderStyle ourStyle $
            ""
              $+$ hang "Case" 4 (text . show $ outcome')
              $+$ hang "Inputs" 4 dumpInputs

counter :: String -> Property
counter s = counterexample s False

produceResult ::
  Either ScriptError ([Text], ScriptResult) ->
  Reader (PropertyEnv p) Property
produceResult sr = do
  outcome <- asks envOutcome
  case sr of
    Left err -> case err of
      EvaluationError logs msg ->
        asks envOutcome >>= \case
          Pass -> asks (counter . unexpectedFailure (getDumpedState logs) msg)
          Fail -> pass
      EvaluationException name msg -> pure . counter $ scriptException name msg
      MalformedScript msg -> pure . counter $ malformedScript msg
    Right (logs, res) -> case (outcome, res) of
      (_, NoOutcome) -> asks (counter . noOutcome state)
      (Fail, ScriptPassed) -> asks (counter . unexpectedSuccess state)
      (Fail, ScriptFailed) -> pass
      (Pass, ScriptPassed) -> pass
      (Pass, ScriptFailed) -> asks (counter . unexpectedFailure state mempty)
      (_, InternalError t) -> asks (counter . internalError state t)
      (_, ParseFailed t) -> asks (counter . noParse state t)
      where
        state :: PropertyEnv p -> Doc
        state = getDumpedState logs
  where
    pass :: Reader (PropertyEnv p) Property
    pass = pure $ property True

validatorProperty ::
  forall (a :: Type) (d :: Type) (r :: Type).
  (Show a, Typeable a, Typeable d, Typeable r) =>
  String ->
  Methodology a ->
  (a -> TestValidator d r) ->
  (a -> TestItems ( 'ForSpending d r)) ->
  TestTree
validatorProperty = mkValidatorPropertyWith OutcomeDependent

validatorPropertyPass ::
  forall (a :: Type) (d :: Type) (r :: Type).
  (Show a, Typeable a, Typeable d, Typeable r) =>
  String ->
  Methodology a ->
  (a -> TestValidator d r) ->
  (a -> TestItems ( 'ForSpending d r)) ->
  TestTree
validatorPropertyPass = mkValidatorPropertyWith OutcomeAlwaysPass

validatorPropertyFail ::
  forall (a :: Type) (d :: Type) (r :: Type).
  (Show a, Typeable a, Typeable d, Typeable r) =>
  String ->
  Methodology a ->
  (a -> TestValidator d r) ->
  (a -> TestItems ( 'ForSpending d r)) ->
  TestTree
validatorPropertyFail = mkValidatorPropertyWith OutcomeAlwaysFail

mkValidatorPropertyWith ::
  forall (a :: Type) (d :: Type) (r :: Type).
  (Show a, Typeable a, Typeable d, Typeable r) =>
  OutcomeKind ->
  String ->
  Methodology a ->
  (a -> TestValidator d r) ->
  (a -> TestItems ( 'ForSpending d r)) ->
  TestTree
mkValidatorPropertyWith out name (Methodology gen shr) fVal fTi =
  singleTest name $ Spender gen shr fVal fTi out

mintingPolicyProperty ::
  forall (a :: Type) (r :: Type).
  (Show a, Typeable a, Typeable r) =>
  String ->
  Methodology a ->
  (a -> TestMintingPolicy r) ->
  (a -> TestItems ( 'ForMinting r)) ->
  TestTree
mintingPolicyProperty = mkMintingPolicyPropertyWith OutcomeDependent

mintingPolicyPropertyPass ::
  forall (a :: Type) (r :: Type).
  (Show a, Typeable a, Typeable r) =>
  String ->
  Methodology a ->
  (a -> TestMintingPolicy r) ->
  (a -> TestItems ( 'ForMinting r)) ->
  TestTree
mintingPolicyPropertyPass = mkMintingPolicyPropertyWith OutcomeAlwaysPass

mintingPolicyPropertyFail ::
  forall (a :: Type) (r :: Type).
  (Show a, Typeable a, Typeable r) =>
  String ->
  Methodology a ->
  (a -> TestMintingPolicy r) ->
  (a -> TestItems ( 'ForMinting r)) ->
  TestTree
mintingPolicyPropertyFail = mkMintingPolicyPropertyWith OutcomeAlwaysFail

mkMintingPolicyPropertyWith ::
  forall (a :: Type) (r :: Type).
  (Show a, Typeable a, Typeable r) =>
  OutcomeKind ->
  String ->
  Methodology a ->
  (a -> TestMintingPolicy r) ->
  (a -> TestItems ( 'ForMinting r)) ->
  TestTree
mkMintingPolicyPropertyWith out name (Methodology gen shr) fMp fTi =
  singleTest name $ Minter gen shr fMp fTi out
