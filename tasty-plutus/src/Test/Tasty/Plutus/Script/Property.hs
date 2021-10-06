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

import Control.Monad (guard)
import Control.Monad.RWS.Strict (tell)
import Control.Monad.Reader (ask)
import Data.Kind (Type)
import Data.Proxy (Proxy (Proxy))
import Data.Sequence qualified as Seq
import Data.Tagged (Tagged (Tagged))
import Data.Text (Text)
import Plutus.V1.Ledger.Contexts (ScriptContext)
import Plutus.V1.Ledger.Interval (Interval)
import Plutus.V1.Ledger.Scripts (
  Context (Context),
  Datum (Datum),
  MintingPolicy,
  Redeemer (Redeemer),
  ScriptError,
  Validator,
  ValidatorHash,
 )
import Plutus.V1.Ledger.Time (POSIXTime)
import Plutus.V1.Ledger.TxId (TxId)
import Plutus.V1.Ledger.Value (CurrencySymbol, Value)
import PlutusTx.IsData.Class (FromData, ToData (toBuiltinData))
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
import Test.Tasty.Options (OptionDescription (Option), lookupOption)
import Test.Tasty.Plutus.Internal (
  ContextBuilder,
  PropertyMaxSize (PropertyMaxSize),
  PropertyTestCount (PropertyTestCount),
  Purpose (ForMinting, ForSpending),
  ScriptResult (
    InternalError,
    NoOutcome,
    ParseFailed,
    ScriptFailed,
    ScriptPassed
  ),
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
  testMintingPolicyScript,
  testValidatorScript,
 )
import Test.Tasty.Plutus.Options (
  Fee (Fee),
  TestCurrencySymbol (TestCurrencySymbol),
  TestTxId (TestTxId),
  TestValidatorHash (TestValidatorHash),
  TimeRange (TimeRange),
 )
import Test.Tasty.Plutus.TestData (
  Example (Bad, Good),
  Generator (GenForMinting, GenForSpending),
  Methodology (Methodology),
  TestData (MintingTest, SpendingTest),
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
  (<+>),
 )
import Text.Show.Pretty (ppDoc)
import Type.Reflection (Typeable)
import Prelude

{- | Given a way of generating 'TestData', and converting a generated 'TestData'
 into a 'ContextBuilder', check that:

 * For any 'TestData' classified as 'Good', the script succeeds; and
 * For any 'TestData' classified as 'Bad', the script fails.

 This will also check /coverage/: specifically, the property will fail unless
 the provided generation method produces roughly equal numbers of 'Good' and
 'Bad'-classified cases.

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
  GenForMinting f mRed -> WithMinting $ do
    mp <- ask
    let generator = mintingTupleGen f mRed
    let shrinker = mintingTupleShrink f mRed
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
    Gen (Example, datum, redeemer, Value) ->
    ((Example, datum, redeemer, Value) -> [(Example, datum, redeemer, Value)]) ->
    (TestData 'ForSpending -> ContextBuilder 'ForSpending) ->
    PropertyTest 'ForSpending
  Minter ::
    ( ToData redeemer
    , FromData redeemer
    , Show redeemer
    ) =>
    MintingPolicy ->
    Gen (Example, redeemer) ->
    ((Example, redeemer) -> [(Example, redeemer)]) ->
    (TestData 'ForMinting -> ContextBuilder 'ForMinting) ->
    PropertyTest 'ForMinting

instance (Typeable p) => IsTest (PropertyTest p) where
  run opts vt _ = do
    let conf =
          TransactionConfig
            { testFee = testFee'
            , testTimeRange = testTimeRange'
            , testTxId = testTxId'
            , testCurrencySymbol = testCurrencySymbol'
            , testValidatorHash = testValidatorHash'
            }
    let PropertyTestCount testCount' = lookupOption opts
    let PropertyMaxSize maxSize' = lookupOption opts
    let args = stdArgs {maxSuccess = testCount', maxSize = maxSize'}
    res <- quickCheckWithResult args . go $ conf
    pure $ case res of
      Success {} -> testPassed ""
      GaveUp {} -> testFailed "Internal error: gave up."
      Failure {} -> testFailed ""
      NoExpectedFailure {} -> testFailed "Internal error: expected failure but saw none."
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
  testOptions =
    Tagged
      [ Option @Fee Proxy
      , Option @TimeRange Proxy
      , Option @TestTxId Proxy
      , Option @TestCurrencySymbol Proxy
      , Option @TestValidatorHash Proxy
      , Option @PropertyTestCount Proxy
      , Option @PropertyMaxSize Proxy
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
      r' = Redeemer . toBuiltinData $ r
   in checkCoverage . cover 0.5 (ex == Good) "good" $
        case testValidatorScript context' val d' r' of
          Left err -> counterexample (formatError err) False
          Right res -> produceResult ex res context td

prettySpender ::
  forall (datum :: Type) (redeemer :: Type).
  (Show datum, Show redeemer) =>
  (Example, datum, redeemer, Value) ->
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
  MintingPolicy ->
  TransactionConfig ->
  (TestData 'ForMinting -> ContextBuilder 'ForMinting) ->
  (Example, redeemer) ->
  Property
minterProperty mp tc mkCB (ex, r) =
  let td = MintingTest r
      context = compileMinting tc . mkCB $ td
      context' = Context . toBuiltinData $ context
      r' = Redeemer . toBuiltinData $ r
   in checkCoverage . cover 0.5 (ex == Good) "good" $
        case testMintingPolicyScript context' mp r' of
          Left err -> counterexample (formatError err) False
          Right res -> produceResult ex res context td
prettyMinter ::
  forall (redeemer :: Type).
  (Show redeemer) =>
  (Example, redeemer) ->
  String
prettyMinter (ex, r) =
  renderStyle ourStyle $
    ""
      $+$ hang "Case" 4 (text . show $ ex)
      $+$ hang "Inputs" 4 dumpRedeemer
  where
    dumpRedeemer :: Doc
    dumpRedeemer = "Redeemer" $+$ ppDoc r

spendingTupleGen ::
  forall (datum :: Type) (redeemer :: Type).
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
  forall (datum :: Type) (redeemer :: Type).
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
  forall (redeemer :: Type).
  (redeemer -> Example) ->
  Methodology redeemer ->
  Gen (Example, redeemer)
mintingTupleGen f mRed = do
  let Methodology genR _ = mRed
  r <- genR
  pure (f r, r)

mintingTupleShrink ::
  forall (redeemer :: Type).
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
formatError = renderStyle ourStyle . ppDoc

produceResult ::
  forall (p :: Purpose).
  Example ->
  ScriptResult ->
  ScriptContext ->
  TestData p ->
  Property
produceResult ex res sc td = case (ex, res) of
  (Good, ScriptPassed) -> property True
  (Bad, ScriptFailed) -> property True
  (Good, ScriptFailed) -> counterexample unexpectedFailure False
  (Bad, ScriptPassed) -> counterexample unexpectedSuccess False
  (_, ParseFailed what) -> counterexample (parseFailure what) False
  (_, InternalError what) -> counterexample (internalErr what) False
  (_, NoOutcome) -> counterexample noOutcome False
  where
    unexpectedSuccess :: String
    unexpectedSuccess =
      renderStyle ourStyle $
        "A good case failed unexpectedly" $+$ dumpState
    unexpectedFailure :: String
    unexpectedFailure =
      renderStyle ourStyle $
        "A bad case succeeded unexpectedly" $+$ dumpState
    noOutcome :: String
    noOutcome =
      renderStyle ourStyle $
        "No outcome from run"
          $+$ dumpState
          $+$ ""
          $+$ "Did you forget to use toTestValidator or toTestMintingPolicy?"
    parseFailure :: Text -> String
    parseFailure what =
      renderStyle ourStyle $
        ((text . show $ what) <+> "did not parse") $+$ dumpState
    internalErr :: Text -> String
    internalErr msg =
      renderStyle ourStyle $
        ("Internal error:" <+> (text.show $ msg)) $+$ dumpState
    dumpState :: Doc
    dumpState =
      ""
        $+$ hang "Context" 4 (ppDoc sc)
        $+$ hang "Inputs" 4 dumpInputs
    dumpInputs :: Doc
    dumpInputs = case td of
      SpendingTest d r v ->
        "Datum"
          $+$ ppDoc d
          $+$ "Redeemer"
          $+$ ppDoc r
          $+$ "Value"
          $+$ ppDoc v
      MintingTest r -> "Redeemer" $+$ ppDoc r
