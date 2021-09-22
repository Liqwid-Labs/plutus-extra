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

  -- * Testing API
  withValidator,
  shouldValidate,
  shouldn'tValidate,
) where

import Control.Monad.RWS.Strict (RWS, evalRWS)
import Control.Monad.Reader (MonadReader, asks)
import Control.Monad.Writer (tell)
import Data.Kind (Type)
import Data.Sequence (Seq)
import Data.Sequence qualified as Seq
import Data.Tagged (Tagged (Tagged))
import GHC.Exts (toList)
import Ledger.Value (Value)
import Plutus.V1.Ledger.Contexts (ScriptContext)
import PlutusTx.IsData.Class (
  FromData,
  ToData,
 )
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
  hang,
  renderStyle,
  style,
  ($+$),
 )
import Text.Show.Pretty (ppDoc)
import Type.Reflection (Typeable)

{- | A structure housing a validator, and the data needed to execute it.

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
    (datum -> redeemer -> ScriptContext -> Bool) ->
    datum ->
    redeemer ->
    Value ->
    TestData 'ForSpending
  -- | @since 3.0
  MintingTest ::
    (ToData redeemer, FromData redeemer, Show redeemer) =>
    (redeemer -> ScriptContext -> Bool) ->
    redeemer ->
    TestData 'ForMinting

{- | Provides a monadic API for composing tests against the same validator.
 While it has all the capabilities of a monad, you mostly won't need them; the
 intended usage is:

 For convenience, we also let you access the 'TestData' in the environment
 using 'ask' and \'hot modify\' using 'local', as per 'MonadReader'.

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

-- | @since 3.0
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
          actual = validator d r context
       in deliverResult expected actual conf context td
    MintingTest mp r ->
      let context = compileMinting conf cb
          actual = mp r context
       in deliverResult expected actual conf context td
  testOptions = Tagged []

deliverResult ::
  forall (p :: Purpose).
  Outcome ->
  Bool ->
  TransactionConfig ->
  ScriptContext ->
  TestData p ->
  Result
deliverResult expected actual conf sc td = case (expected, actual) of
  (Fail, False) -> testPassed ""
  (Fail, True) -> testFailed unexpectedSuccess
  (Pass, False) -> testFailed unexpectedFailure
  (Pass, True) -> testPassed ""
  where
    unexpectedSuccess :: String
    unexpectedSuccess =
      renderStyle ourStyle $
        "Unexpected success" $+$ dumpState
    unexpectedFailure :: String
    unexpectedFailure =
      renderStyle ourStyle $
        "Unexpected failure" $+$ dumpState
    dumpState :: Doc
    dumpState =
      ""
        $+$ hang "Context" 4 (ppDoc sc)
        $+$ hang "Configuration" 4 (ppDoc conf)
        $+$ hang "Inputs" 4 dumpInputs
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

ourStyle :: Style
ourStyle = style {lineLength = 80}
