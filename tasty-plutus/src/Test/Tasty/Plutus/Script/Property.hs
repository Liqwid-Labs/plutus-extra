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
 >   scriptProperty "Some property" myGenerator mkContext
 >   scriptProperty "Some other property" anotherGenerator mkContext
 >   ...

 = Note

 In general, we assume that the 'ContextBuilder' is kept (fairly) stable; the
 goal of 'scriptProperty' is to test behaviour under changing 'TestData' (aka,
 inputs). However, frequently, there is a need to know about what kind of
 inputs we have to make a context that even makes sense at all, regardless of
 whether the inputs are \'good\' or not. Therefore, we provide a \'hook\' into
 this system for when it's needed.
-}
module Test.Tasty.Plutus.Script.Property (
  scriptProperty,
) where

import Control.Arrow ((>>>))
import Control.Monad (guard)
import Control.Monad.RWS.Strict (tell)
import Control.Monad.Reader (Reader, ask, asks, runReader)
import Data.Kind (Type)
import Data.Proxy (Proxy (Proxy))
import Data.Sequence qualified as Seq
import Data.Tagged (Tagged (Tagged))
import Data.Text (Text)
import Plutus.V1.Ledger.Contexts (ScriptContext)
import Plutus.V1.Ledger.Scripts (
  MintingPolicy,
  ScriptError (
    EvaluationError,
    EvaluationException,
    MalformedScript
  ),
  Validator,
 )
import Plutus.V1.Ledger.Value (Value)
import PlutusTx.IsData.Class (FromData, ToData)
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
  Tokens,
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

{- | Given a way of generating 'TestData', and converting a generated 'TestData'
 into a 'ContextBuilder', check that:

 * For any 'TestData' classified as 'Pass', the script succeeds; and
 * For any 'TestData' classified as 'Fail', the script fails.

 This will also check /coverage/: specifically, the property will fail unless
 the provided generation method produces roughly equal numbers of 'Pass' and
 'Fail'-classified cases.

 @since 3.1
-}
scriptProperty ::
  forall (p :: Purpose).
  String ->
  Generator p ->
  (TestData p -> ContextBuilder p) ->
  WithScript p ()
scriptProperty name gen mkCB = case gen of
  GenForSpending f mDat mRed mVal -> WithSpending $ do
    val <- ask
    let generator = spendingTupleGen f mDat mRed mVal
    let shrinker = spendingTupleShrink f mDat mRed mVal
    tell
      . Seq.singleton
      . singleTest name
      . Spender val generator shrinker
      $ mkCB
  GenForMinting f mRed mToks -> WithMinting $ do
    mp <- ask
    let generator = mintingTupleGen f mRed mToks
    let shrinker = mintingTupleShrink f mRed mToks
    tell
      . Seq.singleton
      . singleTest name
      . Minter mp generator shrinker
      $ mkCB

-- Helpers

data PropertyTest (p :: Purpose) where
  Spender ::
    ( ToData datum
    , ToData redeemer
    , FromData datum
    , FromData redeemer
    , Show datum
    , Show redeemer
    ) =>
    Validator ->
    Gen (Outcome, datum, redeemer, Value) ->
    ((Outcome, datum, redeemer, Value) -> [(Outcome, datum, redeemer, Value)]) ->
    (TestData 'ForSpending -> ContextBuilder 'ForSpending) ->
    PropertyTest 'ForSpending
  Minter ::
    ( ToData redeemer
    , FromData redeemer
    , Show redeemer
    ) =>
    MintingPolicy ->
    Gen (Outcome, redeemer, Tokens) ->
    ( (Outcome, redeemer, Tokens) ->
      [(Outcome, redeemer, Tokens)]
    ) ->
    (TestData 'ForMinting -> ContextBuilder 'ForMinting) ->
    PropertyTest 'ForMinting

data PropertyEnv (p :: Purpose) = PropertyEnv
  { envOpts :: OptionSet
  , envPropertyTest :: PropertyTest p
  , envTestData :: TestData p
  , envExpected :: Outcome
  }

getConf ::
  forall (p :: Purpose).
  PropertyEnv p ->
  TransactionConfig
getConf = prepareConf envOpts

getCB ::
  forall (p :: Purpose).
  PropertyEnv p ->
  ContextBuilder p
getCB env = case envPropertyTest env of
  Spender _ _ _ mkCB -> mkCB $ envTestData env
  Minter _ _ _ mkCB -> mkCB $ envTestData env

getScript ::
  forall (p :: Purpose).
  PropertyEnv p ->
  SomeScript p
getScript =
  envPropertyTest >>> \case
    Spender val _ _ _ -> SomeSpender val
    Minter mp _ _ _ -> SomeMinter mp

getSC ::
  forall (p :: Purpose).
  PropertyEnv p ->
  ScriptContext
getSC = getScriptContext getConf getCB envTestData

getDumpedState ::
  forall (p :: Purpose).
  [Text] ->
  PropertyEnv p ->
  Doc
getDumpedState = dumpState' getConf getCB envTestData

instance (Typeable p) => IsTest (PropertyTest p) where
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
        Spender _ gen shrinker _ ->
          forAllShrinkShow gen shrinker prettySpender $ spenderProperty opts vt
        Minter _ gen shrinker _ ->
          forAllShrinkShow gen shrinker prettyMinter $ minterProperty opts vt
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
  forall (datum :: Type) (redeemer :: Type).
  ( ToData datum
  , ToData redeemer
  , FromData datum
  , FromData redeemer
  , Show datum
  , Show redeemer
  ) =>
  OptionSet ->
  PropertyTest 'ForSpending ->
  (Outcome, datum, redeemer, Value) ->
  Property
spenderProperty opts vt (ex, d, r, v) =
  let td = SpendingTest d r v
      env =
        PropertyEnv
          { envOpts = opts
          , envPropertyTest = vt
          , envExpected = ex
          , envTestData = td
          }
   in checkCoverage
        . cover 0.5 (ex == Pass) "good"
        . (`runReader` env)
        . produceResult
        $ getScriptResult getScript envTestData (getContext getSC) env

prettySpender ::
  forall (datum :: Type) (redeemer :: Type).
  (Show datum, Show redeemer) =>
  (Outcome, datum, redeemer, Value) ->
  String
prettySpender (ex, d, r, v) =
  renderStyle ourStyle $
    ""
      $+$ hang "Case" 4 (text . show $ ex)
      $+$ hang "Inputs" 4 dumpInputs
  where
    dumpInputs :: Doc
    dumpInputs =
      "Datum"
        $+$ ppDoc d
        $+$ "Redeemer"
        $+$ ppDoc r
        $+$ "Value"
        $+$ ppDoc v

minterProperty ::
  forall (redeemer :: Type).
  (FromData redeemer, ToData redeemer, Show redeemer) =>
  OptionSet ->
  PropertyTest 'ForMinting ->
  (Outcome, redeemer, Tokens) ->
  Property
minterProperty opts vt (ex, r, toks) =
  let td = MintingTest r toks
      env =
        PropertyEnv
          { envOpts = opts
          , envPropertyTest = vt
          , envExpected = ex
          , envTestData = td
          }
   in checkCoverage
        . cover 0.5 (ex == Pass) "good"
        . (`runReader` env)
        . produceResult
        $ getScriptResult getScript envTestData (getContext getSC) env

prettyMinter ::
  forall (redeemer :: Type).
  (Show redeemer) =>
  (Outcome, redeemer, Tokens) ->
  String
prettyMinter (ex, r, ts) =
  renderStyle ourStyle $
    ""
      $+$ hang "Case" 4 (text . show $ ex)
      $+$ hang "Inputs" 4 dumpInputs
  where
    dumpInputs :: Doc
    dumpInputs =
      "Redeemer"
        $+$ ppDoc r
        $+$ "Tokens"
        $+$ ppDoc ts

spendingTupleGen ::
  forall (datum :: Type) (redeemer :: Type).
  (datum -> redeemer -> Value -> Outcome) ->
  Methodology datum ->
  Methodology redeemer ->
  Methodology Value ->
  Gen (Outcome, datum, redeemer, Value)
spendingTupleGen f mDat mRed mVal = do
  let Methodology genD _ = mDat
  let Methodology genR _ = mRed
  let Methodology genVal _ = mVal
  (d, r, v) <- (,,) <$> genD <*> genR <*> genVal
  pure (f d r v, d, r, v)

spendingTupleShrink ::
  forall (datum :: Type) (redeemer :: Type).
  (datum -> redeemer -> Value -> Outcome) ->
  Methodology datum ->
  Methodology redeemer ->
  Methodology Value ->
  (Outcome, datum, redeemer, Value) ->
  [(Outcome, datum, redeemer, Value)]
spendingTupleShrink f mDat mRed mVal (ex, d, r, v) = do
  let Methodology _ shrinkD = mDat
  let Methodology _ shrinkR = mRed
  let Methodology _ shrinkV = mVal
  (d', r', v') <- (,,) <$> shrinkD d <*> shrinkR r <*> shrinkV v
  guard (f d' r' v' == ex)
  pure (ex, d', r', v')

mintingTupleGen ::
  forall (redeemer :: Type).
  (redeemer -> Tokens -> Outcome) ->
  Methodology redeemer ->
  Methodology Tokens ->
  Gen (Outcome, redeemer, Tokens)
mintingTupleGen f mRed mToks = do
  let Methodology genR _ = mRed
  let Methodology genToks _ = mToks
  (r, ts) <- (,) <$> genR <*> genToks
  pure (f r ts, r, ts)

mintingTupleShrink ::
  forall (redeemer :: Type).
  (redeemer -> Tokens -> Outcome) ->
  Methodology redeemer ->
  Methodology Tokens ->
  (Outcome, redeemer, Tokens) ->
  [(Outcome, redeemer, Tokens)]
mintingTupleShrink f mRed mToks (ex, r, ts) = do
  let Methodology _ shrinkR = mRed
  let Methodology _ shrinkTs = mToks
  (r', ts') <- (,) <$> shrinkR r <*> shrinkTs ts
  guard (f r ts == ex)
  pure (ex, r', ts')

counter :: String -> Property
counter s = counterexample s False

produceResult ::
  Either ScriptError ([Text], ScriptResult) ->
  Reader (PropertyEnv p) Property
produceResult sr = do
  ex <- asks envExpected
  case sr of
    Left err -> case err of
      EvaluationError logs msg ->
        asks envExpected >>= \case
          Pass -> asks (counter . unexpectedFailure (getDumpedState logs) msg)
          Fail -> pass
      EvaluationException name msg -> pure . counter $ scriptException name msg
      MalformedScript msg -> pure . counter $ malformedScript msg
    Right (logs, res) -> case (ex, res) of
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
