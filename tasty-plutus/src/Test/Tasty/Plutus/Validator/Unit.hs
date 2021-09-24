{- |
 Module: Test.Tasty.Plutus.Validator.Unit
 Copyright: (C) MLabs 2021
 License: Apache 2.0
 Maintainer: Koz Ross <koz@mlabs.city>
 Portability: GHC only
 Stability: Experimental

 A unit-test-like interface for validator testing.
-}
module Test.Tasty.Plutus.Validator.Unit (
  -- * Validator context types
  TestData (..),
  WithValidator,

  -- * Wrappers
  toTestValidator,
  toTestMintingPolicy,

  -- * Testing API
  withValidator,
  withMintingPolicy,
  shouldValidate,
  shouldn'tValidate,

  -- * Options
  Fee (..),
  TimeRange (..),
  TestTxId (..),
  TestCurrencySymbol (..),
  TestValidatorHash (..),
) where

import Control.Monad.RWS.Strict (RWS, evalRWS)
import Control.Monad.Reader (MonadReader (ask, local), asks)
import Control.Monad.Writer (tell)
import Data.Kind (Type)
import Data.Proxy (Proxy (Proxy))
import Data.Sequence (Seq)
import Data.Sequence qualified as Seq
import Data.Tagged (Tagged (Tagged))
import Data.Text (Text)
import Data.Text qualified as Text
import GHC.Exts (IsString, toList)
import Ledger.Value (CurrencySymbol (CurrencySymbol), Value)
import Plutus.V1.Ledger.Contexts (ScriptContext)
import Plutus.V1.Ledger.Interval (Interval)
import Plutus.V1.Ledger.Interval qualified as Interval
import Plutus.V1.Ledger.Scripts (
  Context (Context),
  Datum (Datum),
  MintingPolicy,
  Redeemer (Redeemer),
  ScriptError,
  Validator,
  ValidatorHash (ValidatorHash),
  runMintingPolicyScript,
  runScript,
 )
import Plutus.V1.Ledger.Time (POSIXTime)
import Plutus.V1.Ledger.TxId (TxId (TxId))
import PlutusTx.Builtins (
  BuiltinData,
  BuiltinString,
  appendString,
  trace,
 )
import PlutusTx.IsData.Class (
  FromData (fromBuiltinData),
  ToData (toBuiltinData),
 )
import Safe (lastMay)
import Test.Tasty (testGroup)
import Test.Tasty.Options (
  IsOption (
    defaultValue,
    optionHelp,
    optionName,
    parseValue,
    showDefaultValue
  ),
  OptionDescription (Option),
  lookupOption,
 )
import Test.Tasty.Plutus.Context.Internal (
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
  compileMinting,
  compileSpending,
 )
import Test.Tasty.Providers (
  IsTest (run, testOptions),
  Result,
  TestTree,
  singleTest,
  testFailed,
  testPassed,
 )
import Text.PrettyPrint (
  Doc,
  Style (lineLength),
  colon,
  hang,
  int,
  renderStyle,
  style,
  text,
  vcat,
  ($+$),
  (<+>),
 )
import Text.Show.Pretty (ppDoc)
import Type.Reflection (Typeable)

{- | A wrapper for validators. Use this to construct 'Validator's suitable for
 passing to 'withValidator'.

 = Usage

 > testValidator :: Validator
 > testValidator = mkValidatorScript $
      $$(compile [|| go ||]) `applyCode` $$(compile [|| myValidator ||])
 >   where
 >    go = toTestValidator

 = Important note

 If @myValidator@ requires \'burned in\' arguments, these should be passed via
 'liftCode' and 'applyCode', rather than as literal arguments inside of
 'compile':

 > testValidatorWithArg :: Validator
 > testValidatorWithArg = mkValidatorScript $
 >    $$(compile [|| go ||]) `applyCode` ( $$(compile [|| myValidator ||])
 >                                          `applyCode`
 >                                         liftCode arg1
 >                                       )

 @since 3.0
-}
{-# INLINEABLE toTestValidator #-}
toTestValidator ::
  forall (datum :: Type) (redeemer :: Type).
  (FromData datum, FromData redeemer) =>
  (datum -> redeemer -> ScriptContext -> Bool) ->
  (BuiltinData -> BuiltinData -> BuiltinData -> ())
toTestValidator f d r p = case fromBuiltinData d of
  Nothing -> reportParseFailed "Datum"
  Just d' -> case fromBuiltinData r of
    Nothing -> reportParseFailed "Redeemer"
    Just r' -> case fromBuiltinData p of
      Nothing -> reportParseFailed "ScriptContext"
      Just p' ->
        if f d' r' p'
          then reportPass
          else reportFail

{- | A wrapper for minting policies. Use this to construct a 'MintingPolicy'
 suitable for passing to 'withMintingPolicy'.

 The usage (and caveats) of this function is similar to 'toTestValidator'; see
 its documentation for details.

 @since 3.0
-}
{-# INLINEABLE toTestMintingPolicy #-}
toTestMintingPolicy ::
  forall (redeemer :: Type).
  (FromData redeemer) =>
  (redeemer -> ScriptContext -> Bool) ->
  (BuiltinData -> BuiltinData -> ())
toTestMintingPolicy f r p = case fromBuiltinData r of
  Nothing -> reportParseFailed "Redeemer"
  Just r' -> case fromBuiltinData p of
    Nothing -> reportParseFailed "ScriptContext"
    Just p' ->
      if f r' p'
        then reportPass
        else reportFail

{- | All the data needed to test a validator or minting policy.

 @since 3.0
-}
data TestData (p :: Purpose) where
  -- | @since 3.0
  SpendingTest ::
    ( ToData datum
    , ToData redeemer
    , FromData datum
    , FromData redeemer
    , Show datum
    , Show redeemer
    ) =>
    datum ->
    redeemer ->
    Value ->
    TestData 'ForSpending
  -- | @since 3.0
  MintingTest ::
    (ToData redeemer, FromData redeemer, Show redeemer) =>
    redeemer ->
    TestData 'ForMinting

{- | Provides a monadic API for composing tests against the same validator or
 minting policy. While it has all the capabilities of a monad, you mostly
 won't need them. The intended usage is:

 > withValidator "Testing my validator" myValidator $ do
 >    shouldValidate "Valid case" validData validContext
 >    shouldn'tValidate "Invalid context" validData invalidContext
 >    shouldn'tValidate "Invalid data" invalidData validContext
 >    shouldn'tValidate "Everything is bad" invalidData invalidContext
 >    ...

 @since 3.0
-}
data WithValidator (p :: Purpose) (a :: Type) where
  WithSpending ::
    RWS Validator (Seq TestTree) () a ->
    WithValidator 'ForSpending a
  WithMinting ::
    RWS MintingPolicy (Seq TestTree) () a ->
    WithValidator 'ForMinting a

-- | @since 1.0
deriving stock instance Functor (WithValidator p)

-- | @since 1.0
instance Applicative (WithValidator 'ForSpending) where
  {-# INLINEABLE pure #-}
  pure = WithSpending . pure
  {-# INLINEABLE (<*>) #-}
  WithSpending fs <*> WithSpending xs = WithSpending (fs <*> xs)

-- | @since 1.0
instance Applicative (WithValidator 'ForMinting) where
  {-# INLINEABLE pure #-}
  pure = WithMinting . pure
  {-# INLINEABLE (<*>) #-}
  WithMinting fs <*> WithMinting xs = WithMinting (fs <*> xs)

-- | @since 1.0
instance Monad (WithValidator 'ForSpending) where
  {-# INLINEABLE (>>=) #-}
  WithSpending xs >>= f = WithSpending $ do
    x <- xs
    let (WithSpending ys) = f x
    ys

-- | @since 1.0
instance Monad (WithValidator 'ForMinting) where
  {-# INLINEABLE (>>=) #-}
  WithMinting xs >>= f = WithMinting $ do
    x <- xs
    let (WithMinting ys) = f x
    ys

-- | @since 3.0
instance MonadReader Validator (WithValidator 'ForSpending) where
  {-# INLINEABLE ask #-}
  ask = WithSpending ask
  {-# INLINEABLE local #-}
  local f (WithSpending comp) = WithSpending . local f $ comp

-- | @since 3.0
instance MonadReader MintingPolicy (WithValidator 'ForMinting) where
  {-# INLINEABLE ask #-}
  ask = WithMinting ask
  {-# INLINEABLE local #-}
  local f (WithMinting comp) = WithMinting . local f $ comp

{- | Given the name for the tests, a 'Validator', and a collection of
 spending-related tests, execute all of them as a 'TestTree'.

 = Usage

 > myTests :: TestTree
 > myTests = withValidator "Testing my spending" myValidator $ do
 >    shouldValidate "Valid case" validData validContext
 >    shouldn'tValidate "Invalid context" validData invalidContext
 >    shouldn'tValidate "Invalid data" invalidData validContext
 >    shouldn'tValidate "Everything is bad" invalidData invalidContext
 >    ...

 = Important note

 Unless your 'Validator' has been prepared using 'toTestValidator', this will
 likely not behave as intended.

 @since 3.0
-}
withValidator ::
  String ->
  Validator ->
  WithValidator 'ForSpending () ->
  TestTree
withValidator name val (WithSpending comp) =
  case evalRWS comp val () of
    ((), tests) -> testGroup name . toList $ tests

{- | Given the name for the tests, a 'MintingPolicy', and a collection of
 minting-related tests, execute all of them as a 'TestTree'.

 = Usage

 > myTests :: TestTree
 > myTests = withMintingPolicy "Testing my minting" mp $ do
 >    shouldValidate "Valid case" validData validContext
 >    shouldn'tValidate "Invalid context" validData invalidContext
 >    shouldn'tValidate "Invalid data" invalidData validContext
 >    shouldn'tValidate "Everything is bad" invalidData invalidContext
 >    ...

 = Important note

 Unless your 'MintingPolicy' has been prepared using 'toTestMintingPolicy',
 this will likely not behave as intended.

 @since 3.0
-}
withMintingPolicy ::
  String ->
  MintingPolicy ->
  WithValidator 'ForMinting () ->
  TestTree
withMintingPolicy name mp (WithMinting comp) =
  case evalRWS comp mp () of
    ((), tests) -> testGroup name . toList $ tests

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
  WithValidator p ()
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
  WithValidator p ()
shouldn'tValidate name td cb = case td of
  SpendingTest {} -> WithSpending $ do
    tt <- asks (singleTest name . Spender Fail td cb)
    tell . Seq.singleton $ tt
  MintingTest {} -> WithMinting $ do
    tt <- asks (singleTest name . Minter Fail td cb)
    tell . Seq.singleton $ tt

{- | The transaction fee used for the tests.

 Defaults to 'mempty'. Parsing this option is currently not supported.

 @since 3.0
-}
newtype Fee = Fee Value
  deriving stock
    ( -- | @since 3.0
      Show
    )

-- | @since 3.0
instance IsOption Fee where
  defaultValue = Fee mempty
  parseValue = const Nothing
  optionName = Tagged "fee"
  optionHelp = Tagged "CLI PASSING NOT SUPPORTED"
  showDefaultValue = Just . show

{- | Valid time range for tests.

 Defaults to 'Interval.always'. Parsing this option is currently not supported.

 @since 3.0
-}
newtype TimeRange = TimeRange (Interval POSIXTime)
  deriving stock
    ( -- | @since 3.0
      Show
    )

-- | @since 3.0
instance IsOption TimeRange where
  defaultValue = TimeRange Interval.always
  parseValue = const Nothing
  optionName = Tagged "time-range"
  optionHelp = Tagged "CLI PASSING NOT SUPPORTED"
  showDefaultValue = Just . show

{- | The 'TxId' whose inputs should be consumed.

 The default value is arbitrary - if you need a specific value, set it
 manually. Parsing this option is currently not supported.

 @since 3.0
-}
newtype TestTxId = TestTxId TxId
  deriving stock
    ( -- | @since 3.0
      Show
    )

-- | @since 3.0
instance IsOption TestTxId where
  defaultValue = TestTxId . TxId $ "abcd"
  parseValue = const Nothing
  optionName = Tagged "tx-id"
  optionHelp = Tagged "CLI PASSING NOT SUPPORTED"
  showDefaultValue = const Nothing

{- | The 'CurrencySymbol' used in the tests.

 The default value is arbitrary - if you need a specific value, set it
 manually. Parsing this option is currently not supported.

 @since 3.0
-}
newtype TestCurrencySymbol = TestCurrencySymbol CurrencySymbol
  deriving stock
    ( -- | @since 3.0
      Show
    )
  deriving
    ( -- | @since 3.0
      IsString
    )
    via CurrencySymbol

-- | @since 3.0
instance IsOption TestCurrencySymbol where
  defaultValue = "ff"
  parseValue = const Nothing
  optionName = Tagged "currency-symbol"
  optionHelp = Tagged "CLI PASSING NOT SUPPORTED"
  showDefaultValue = const Nothing

{- | Validator address during testing.

 The default value is arbitrary - if you need a specific value, set it
 manually. Parsing this option is currently not supported.

 @since 3.0
-}
newtype TestValidatorHash = TestValidatorHash ValidatorHash
  deriving stock
    ( -- | @since 3.0
      Show
    )
  deriving
    ( -- | @since 3.0
      IsString
    )
    via ValidatorHash

-- | @since 3.0
instance IsOption TestValidatorHash where
  defaultValue = "90ab"
  parseValue = const Nothing
  optionName = Tagged "validator-hash"
  optionHelp = Tagged "CLI PASSING NOT SUPPORTED"
  showDefaultValue = const Nothing

-- Helpers

data Outcome = Fail | Pass

data ValidatorTest (p :: Purpose) where
  Spender ::
    Outcome ->
    TestData 'ForSpending ->
    ContextBuilder 'ForSpending ->
    Validator ->
    ValidatorTest 'ForSpending
  Minter ::
    Outcome ->
    TestData 'ForMinting ->
    ContextBuilder 'ForMinting ->
    MintingPolicy ->
    ValidatorTest 'ForMinting

instance (Typeable p) => IsTest (ValidatorTest p) where
  run opts vt _ = pure $ case vt of
    Spender expected td@(SpendingTest d r v) cb val ->
      let context = compileSpending conf cb d v
          context' = Context . toBuiltinData $ context
          d' = Datum . toBuiltinData $ d
          r' = Redeemer . toBuiltinData $ r
       in case runScript context' val d' r' of
            Left err -> testFailed . formatScriptError $ err
            Right (_, logs) -> deliverResult expected logs conf context td
    Minter expected td@(MintingTest r) cb mp ->
      let context = compileMinting conf cb
          context' = Context . toBuiltinData $ context
          r' = Redeemer . toBuiltinData $ r
       in case runMintingPolicyScript context' mp r' of
            Left err -> testFailed . formatScriptError $ err
            Right (_, logs) -> deliverResult expected logs conf context td
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
  testOptions =
    Tagged
      [ Option @Fee Proxy
      , Option @TimeRange Proxy
      , Option @TestTxId Proxy
      , Option @TestCurrencySymbol Proxy
      , Option @TestValidatorHash Proxy
      ]

deliverResult ::
  forall (p :: Purpose).
  Outcome ->
  [Text] ->
  TransactionConfig ->
  ScriptContext ->
  TestData p ->
  Result
deliverResult expected logs conf sc td =
  case (expected, lastMay logs >>= Text.stripPrefix "tasty-plutus: ") of
    (_, Nothing) -> testFailed noOutcome
    (Fail, Just "Pass") -> testFailed unexpectedSuccess
    (Fail, Just "Fail") -> testPassed ""
    (Pass, Just "Pass") -> testPassed ""
    (Pass, Just "Fail") -> testFailed unexpectedFailure
    (_, Just t) -> case Text.stripPrefix "Parse failed: " t of
      Nothing -> testFailed . internalError $ t
      Just t' -> testFailed . noParse $ t'
  where
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
        $+$ hang "Context" 4 (ppDoc sc)
        $+$ hang "Configuration" 4 (ppDoc conf)
        $+$ hang "Inputs" 4 dumpInputs
        $+$ hang "Logs" 4 dumpLogs
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
    dumpLogs :: Doc
    dumpLogs = vcat . fmap go . zip [1 ..] $ logs
    go :: (Int, Text) -> Doc
    go (ix, line) = (int ix <> colon) <+> (text . show $ line)

ourStyle :: Style
ourStyle = style {lineLength = 80}

formatScriptError :: ScriptError -> String
formatScriptError =
  renderStyle ourStyle . hang "Script execution error:" 4 . ppDoc

{-# INLINEABLE reportParseFailed #-}
reportParseFailed :: BuiltinString -> ()
reportParseFailed what = report ("Parse failed: " `appendString` what)

{-# INLINEABLE reportPass #-}
reportPass :: ()
reportPass = report "Pass"

{-# INLINEABLE reportFail #-}
reportFail :: ()
reportFail = report "Fail"

{-# INLINEABLE report #-}
report :: BuiltinString -> ()
report what = trace ("tasty-plutus: " `appendString` what) ()
