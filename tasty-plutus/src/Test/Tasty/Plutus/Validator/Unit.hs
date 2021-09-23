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
  shouldValidate,
  shouldn'tValidate,

  -- * State control helpers
  withConfig,
  withTestData,
) where

import Control.Monad.RWS.Strict (RWS, evalRWS)
import Control.Monad.Reader (MonadReader, asks, local)
import Control.Monad.Writer (tell)
import Data.Bifunctor (first, second)
import Data.Kind (Type)
import Data.Sequence (Seq)
import Data.Sequence qualified as Seq
import Data.Tagged (Tagged (Tagged))
import Data.Text (Text)
import Data.Text qualified as Text
import GHC.Exts (toList)
import Ledger.Value (Value)
import Plutus.V1.Ledger.Contexts (ScriptContext)
import Plutus.V1.Ledger.Scripts (
  Context (Context),
  Datum (Datum),
  MintingPolicy,
  Redeemer (Redeemer),
  ScriptError,
  Validator,
  runMintingPolicyScript,
  runScript,
 )
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
import Test.Tasty.Plutus.Context.Internal (
  ContextBuilder,
  Purpose (ForMinting, ForSpending),
  TransactionConfig,
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
 passing to 'TestData'.

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

{- | A wrapper for minting policies. Should be used similarly to
 'toTestValidator': see the documentation for more information and examples.

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

{- | A structure housing a validator, and the data needed to execute it.

 = Important note

 If passing a 'Validator' or 'MintingPolicy', ensure that it's constructed
 _only_ using 'toTestValidator' or 'toTestMintingPolicy' as described in their
 documentation. Failing to do so will likely lead to unexpected behaviour.

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
    Validator ->
    datum ->
    redeemer ->
    Value ->
    TestData 'ForSpending
  -- | @since 3.0
  MintingTest ::
    (ToData redeemer, FromData redeemer, Show redeemer) =>
    MintingPolicy ->
    redeemer ->
    TestData 'ForMinting

{- | Provides a monadic API for composing tests against the same validator.
 While it has all the capabilities of a monad, you mostly won't need them; the
 intended usage is:

 > withValidator "Testing my validator" myConfig myTestData $ do
 >    shouldValidate "Valid case" validContext
 >    shouldn'tValidatoe "Invalid case" invalidContext
 >    ...

 For convenience, we also let you access the 'TestData' and 'TransactionConfig'
 in the environment using 'ask' and \'hot modify\' using 'local', as per
 'MonadReader'.

 @since 3.0
-}
newtype WithValidator (p :: Purpose) (a :: Type)
  = WithValidator (RWS (TransactionConfig, TestData p) (Seq TestTree) () a)
  deriving
    ( -- | @since 1.0
      Functor
    , -- | @since 1.0
      Applicative
    , -- | @since 1.0
      Monad
    , -- | @since 3.0
      MonadReader (TransactionConfig, TestData p)
    )
    via (RWS (TransactionConfig, TestData p) (Seq TestTree) ())

{- | A specialized form of 'local', modifying only the 'TransactionConfig'.

 @since 3.0
-}
withConfig ::
  forall (p :: Purpose) (a :: Type).
  (TransactionConfig -> TransactionConfig) ->
  WithValidator p a ->
  WithValidator p a
withConfig f = local (first f)

{- | A specialized form of 'local', modifying only the 'TestData'.

 @since 3.0
-}
withTestData ::
  forall (p :: Purpose) (a :: Type).
  (TestData p -> TestData p) ->
  WithValidator p a ->
  WithValidator p a
withTestData f = local (second f)

{- | Given the name for the tests, a configuration, and test data, execute all
 specified tests in the 'WithValidator' as a 'TestTree'.

 = Usage

 > myTests :: TestTree
 > myTests = withValidator "Testing my validator" myConfig myTestData $ do
 >    shouldValidate "Valid case" validContext
 >    shouldn'tValidate "Invalid case" invalidContext
 >    ...

 @since 3.0
-}
withValidator ::
  forall (p :: Purpose).
  String ->
  TransactionConfig ->
  TestData p ->
  WithValidator p () ->
  TestTree
withValidator name conf td (WithValidator comp) =
  case evalRWS comp (conf, td) () of
    ((), tests) -> testGroup name . toList $ tests

{- | Specify that, in the given context, the validation should succeed.

 @since 3.0
-}
shouldValidate ::
  forall (p :: Purpose).
  (Typeable p) =>
  String ->
  ContextBuilder p ->
  WithValidator p ()
shouldValidate name cb = WithValidator $ do
  tt <- asks (singleTest name . ValidatorTest Pass cb)
  tell . Seq.singleton $ tt

{- | Specify that, in the given context, the inputs should parse, but the
 validation should fail.

 @since 3.0
-}
shouldn'tValidate ::
  forall (p :: Purpose).
  (Typeable p) =>
  String ->
  ContextBuilder p ->
  WithValidator p ()
shouldn'tValidate name cb = WithValidator $ do
  tt <- asks (singleTest name . ValidatorTest Fail cb)
  tell . Seq.singleton $ tt

-- Helpers

data Outcome = Fail | Pass

data ValidatorTest (p :: Purpose)
  = ValidatorTest Outcome (ContextBuilder p) (TransactionConfig, TestData p)

instance (Typeable p) => IsTest (ValidatorTest p) where
  run _ (ValidatorTest expected cb (conf, td)) _ = pure $ case td of
    SpendingTest validator d r val ->
      let context = compileSpending conf cb d val
          context' = Context . toBuiltinData $ context
          d' = Datum . toBuiltinData $ d
          r' = Redeemer . toBuiltinData $ r
       in case runScript context' validator d' r' of
            Left err -> testFailed . formatScriptError $ err
            Right (_, logs) -> deliverResult expected logs conf context td
    MintingTest mp r ->
      let context = compileMinting conf cb
          context' = Context . toBuiltinData $ context
          r' = Redeemer . toBuiltinData $ r
       in case runMintingPolicyScript context' mp r' of
            Left err -> testFailed . formatScriptError $ err
            Right (_, logs) -> deliverResult expected logs conf context td
  testOptions = Tagged []

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
      SpendingTest _ d r v ->
        "Datum"
          $+$ ppDoc d
          $+$ "Redeemer"
          $+$ ppDoc r
          $+$ "Value"
          $+$ ppDoc v
      MintingTest _ r ->
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
