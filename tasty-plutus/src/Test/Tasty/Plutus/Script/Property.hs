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
  ScriptError (
    EvaluationError,
    EvaluationException,
    MalformedScript
  ),
  Validator,
  ValidatorHash,
 )
import Plutus.V1.Ledger.Time (POSIXTime)
import Plutus.V1.Ledger.TxId (TxId)
import Plutus.V1.Ledger.Value (CurrencySymbol, Value)
import PlutusTx.IsData.Class (ToData (toBuiltinData))
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
import Test.Tasty.Plutus.Internal (ourStyle)
import Test.Tasty.Plutus.Internal.Context (
  Purpose (ForMinting, ForSpending),
  TransactionConfig (
    TransactionConfig,
    scriptInputPosition,
    testCurrencySymbol,
    testFee,
    testTimeRange,
    testTxId,
    testValidatorHash
  ),
  compileMinting,
  compileSpending,
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
  testMintingPolicyScript,
  testValidatorScript,
 )
import Test.Tasty.Plutus.Internal.WithScript (
  WithScript (WithMinting, WithSpending),
 )
import Test.Tasty.Plutus.Options (
  Fee (Fee),
  ScriptInputPosition,
  TestCurrencySymbol (TestCurrencySymbol),
  TestTxId (TestTxId),
  TestValidatorHash (TestValidatorHash),
  TimeRange (TimeRange),
 )
import Test.Tasty.Plutus.TestData (
  Example (Bad, Good),
  Generator (GenForMinting, GenForSpending),
  Methodology (Methodology),
  TestItems (
    ItemsForMinting,
    ItemsForSpending,
    mintCB,
    mintExample,
    mintRedeemer,
    spendCB,
    spendDatum,
    spendExample,
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
  colon,
  hang,
  int,
  renderStyle,
  text,
  vcat,
  ($+$),
  (<+>),
 )
import Text.Show.Pretty (ppDoc)
import Type.Reflection (Typeable)
import Prelude

-- {- | Given a way of generating 'TestData', and converting a generated 'TestData'
--  into a 'ContextBuilder', check that:

--  * For any 'TestData' classified as 'Good', the script succeeds; and
--  * For any 'TestData' classified as 'Bad', the script fails.

--  This will also check /coverage/: specifically, the property will fail unless
--  the provided generation method produces roughly equal numbers of 'Good' and
--  'Bad'-classified cases.

--  @since 3.1
-- -}
scriptProperty ::
  forall (a :: Type) (p :: Purpose).
  (Typeable a) =>
  String ->
  Generator a p ->
  WithScript p ()
scriptProperty name generator = case generator of
  GenForSpending (Methodology gen shrinker) f ->
    WithSpending $ do
      val <- ask
      tell
        . Seq.singleton
        . singleTest name
        $ Spender val gen shrinker f
  GenForMinting (Methodology gen shrinker) f ->
    WithMinting $ do
      mp <- ask
      tell
        . Seq.singleton
        . singleTest name
        $ Minter mp gen shrinker f

-- Helpers

data PropertyTest (a :: Type) (p :: Purpose) where
  Spender ::
    Validator ->
    Gen a ->
    (a -> [a]) ->
    (a -> TestItems 'ForSpending) ->
    PropertyTest a 'ForSpending
  Minter ::
    MintingPolicy ->
    Gen a ->
    (a -> [a]) ->
    (a -> TestItems 'ForMinting) ->
    PropertyTest a 'ForMinting

instance (Show a, Typeable a, Typeable p) => IsTest (PropertyTest a p) where
  run opts vt _ = do
    let conf =
          TransactionConfig
            { testFee = testFee'
            , testTimeRange = testTimeRange'
            , testTxId = testTxId'
            , testCurrencySymbol = testCurrencySymbol'
            , testValidatorHash = testValidatorHash'
            , scriptInputPosition = lookupOption opts
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
        Spender val gen shrinker f ->
          forAllShrinkShow gen shrinker (prettySpender f) $ spenderProperty val tc f
        Minter mp gen shrinker f ->
          forAllShrinkShow gen shrinker (prettyMinter f) $ minterProperty mp tc f
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
      , Option @ScriptInputPosition Proxy
      ]

spenderProperty ::
  forall (a :: Type).
  Validator ->
  TransactionConfig ->
  (a -> TestItems 'ForSpending) ->
  a ->
  Property
spenderProperty val tc f seed = case f seed of
  ItemsForSpending
    { spendDatum = d :: datum
    , spendRedeemer = r :: redeemer
    , spendValue = v
    , spendCB = cb
    , spendExample = ex
    } ->
      let context = compileSpending tc cb d v
          context' = Context . toBuiltinData $ context
          d' = Datum . toBuiltinData $ d
          r' = Redeemer . toBuiltinData $ r
       in checkCoverage
            . cover 45.0 (ex == Good) "Successful validation"
            . produceResult ex tc context
            . testValidatorScript context' val d'
            $ r'

prettySpender ::
  forall (a :: Type).
  (Show a) =>
  (a -> TestItems 'ForSpending) ->
  a ->
  String
prettySpender f seed = case f seed of
  ItemsForSpending
    { spendDatum = d :: datum
    , spendRedeemer = r :: redeemer
    , spendValue = v
    , spendCB = cb
    , spendExample = ex
    } ->
      let dumpInputs :: Doc
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
              $+$ hang "Case" 4 (text . show $ ex)
              $+$ hang "Inputs" 4 dumpInputs

minterProperty ::
  forall (a :: Type).
  MintingPolicy ->
  TransactionConfig ->
  (a -> TestItems 'ForMinting) ->
  a ->
  Property
minterProperty mp tc f seed = case f seed of
  ItemsForMinting
    { mintRedeemer = r
    , mintCB = cb
    , mintExample = ex
    } ->
      let context = compileMinting tc cb
          context' = Context . toBuiltinData $ context
          r' = Redeemer . toBuiltinData $ r
       in checkCoverage
            . cover 45.0 (ex == Good) "Successful validation"
            . produceResult ex tc context
            . testMintingPolicyScript context' mp
            $ r'

prettyMinter ::
  forall (a :: Type).
  (Show a) =>
  (a -> TestItems 'ForMinting) ->
  a ->
  String
prettyMinter f seed = case f seed of
  ItemsForMinting
    { mintRedeemer = r :: redeemer
    , mintCB = cb
    , mintExample = ex
    } ->
      let dumpInputs :: Doc
          dumpInputs =
            "Seed"
              $+$ ppDoc seed
              $+$ "Redeemer"
              $+$ ppDoc @redeemer r
              $+$ "ContextBuilder"
              $+$ ppDoc cb
       in renderStyle ourStyle $
            ""
              $+$ hang "Case" 4 (text . show $ ex)
              $+$ hang "Inputs" 4 dumpInputs

produceResult ::
  Example ->
  TransactionConfig ->
  ScriptContext ->
  Either ScriptError ([Text], ScriptResult) ->
  Property
produceResult ex tc sc = \case
  Left err -> case err of
    EvaluationError logs _ -> case ex of
      Good -> counterexample (unexpectedFailure logs) False
      Bad -> property True
    EvaluationException _ _ -> counterexample (scriptError err) False
    MalformedScript _ -> counterexample (scriptError err) False
  Right (logs, res) -> case res of
    ParseFailed what -> counterexample (parseFailure logs what) False
    InternalError what -> counterexample (internalErr logs what) False
    NoOutcome -> counterexample (noOutcome logs) False
    ScriptPassed -> case ex of
      Good -> property True
      Bad -> counterexample (unexpectedSuccess logs) False
    ScriptFailed -> case ex of
      Good -> counterexample (unexpectedFailure logs) False
      Bad -> property True
  where
    scriptError :: ScriptError -> String
    scriptError err =
      renderStyle ourStyle $
        "Script errored"
          $+$ hang "Error" 4 (ppDoc err)
          $+$ dumpState'
    parseFailure :: [Text] -> Text -> String
    parseFailure logs what =
      renderStyle ourStyle $
        ((text . show $ what) <+> "did not parse")
          $+$ dumpState logs
    internalErr :: [Text] -> Text -> String
    internalErr logs msg =
      renderStyle ourStyle $
        ("Internal error:" <+> (text . show $ msg))
          $+$ dumpState logs
    noOutcome :: [Text] -> String
    noOutcome logs =
      renderStyle ourStyle $
        "No outcome from run"
          $+$ dumpState logs
          $+$ ""
          $+$ "Did you forget to use toTestValidator or toTestMintingPolicy?"
    unexpectedFailure :: [Text] -> String
    unexpectedFailure logs =
      renderStyle ourStyle $
        "A good case failed unexpectedly"
          $+$ dumpState logs
    unexpectedSuccess :: [Text] -> String
    unexpectedSuccess logs =
      renderStyle ourStyle $
        "A bad case succeeded unexpectedly"
          $+$ dumpState logs
    dumpState :: [Text] -> Doc
    dumpState logs = dumpState' $+$ dumpLogs logs
    dumpState' :: Doc
    dumpState' =
      ""
        $+$ hang "Context" 4 (ppDoc sc)
        $+$ hang "Config" 4 (ppDoc tc)
    dumpLogs :: [Text] -> Doc
    dumpLogs = hang "Logs" 4 . vcat . fmap go . zip [1 ..]
    go :: (Int, Text) -> Doc
    go (ix, line) = (int ix <> colon) <+> (text . show $ line)
