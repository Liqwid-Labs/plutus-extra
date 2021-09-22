{-# LANGUAGE TemplateHaskell #-}

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
  mkMintingPolicyScript,
  mkValidatorScript,
  runMintingPolicyScript,
  runScript,
 )
import PlutusTx.Builtins (BuiltinData, trace)
import PlutusTx.Code (CompiledCode, applyCode)
import PlutusTx.IsData.Class (
  FromData (fromBuiltinData),
  ToData (toBuiltinData),
 )
import PlutusTx.TH (compile)
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
    CompiledCode (datum -> redeemer -> ScriptContext -> Bool) ->
    datum ->
    redeemer ->
    Value ->
    TestData 'ForSpending
  -- | @since 3.0
  MintingTest ::
    (ToData redeemer, FromData redeemer, Show redeemer) =>
    CompiledCode (redeemer -> ScriptContext -> Bool) ->
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
    SpendingTest cc d r val ->
      let context = compileSpending conf cb d val
          context' = Context . toBuiltinData $ context
          d' = Datum . toBuiltinData $ d
          r' = Redeemer . toBuiltinData $ r
          validator = compileValidator cc
       in case runScript context' validator d' r' of
            Left err -> testFailed . formatScriptError $ err
            Right (_, logs) -> deliverResult expected logs conf context td
    MintingTest cc r ->
      let context = compileMinting conf cb
          context' = Context . toBuiltinData $ context
          r' = Redeemer . toBuiltinData $ r
          mp = compileMintingPolicy cc
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
        "No outcome from run" $+$ dumpState
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

compileValidator ::
  forall (datum :: Type) (redeemer :: Type).
  (FromData datum, FromData redeemer) =>
  CompiledCode (datum -> redeemer -> ScriptContext -> Bool) ->
  Validator
compileValidator =
  mkValidatorScript . applyCode $$(compile [||wrap||])
  where
    -- This is needed for, as the Plutus docs say it, 'technical reasons'. More
    -- specifically, without 'trapping' the type class constraints on 'datum'
    -- and 'redeemer', the compiler can't assure us that what it compiles can
    -- 'fit', as 'applyCode' doesn't guarantee any constraints on 'datum' or
    -- 'redeemer'. This throws GHC into a tailspin at compile time, as it finds
    -- itself having to prove that type class dictionaries exist before it can
    -- be sure of that.
    --
    -- Doing this 'local trapping' allows us to avoid this problem, as this
    -- 'burns in' the type class dictionaries 'atop' of 'wrap'.
    --
    -- Koz
    wrap ::
      (datum -> redeemer -> ScriptContext -> Bool) ->
      BuiltinData ->
      BuiltinData ->
      BuiltinData ->
      ()
    wrap f d r p = case fromBuiltinData d of
      Nothing -> trace "tasty-plutus: Parse failed: Datum" ()
      Just d' -> case fromBuiltinData r of
        Nothing -> trace "tasty-plutus: Parse failed: Redeemer" ()
        Just r' -> case fromBuiltinData p of
          Nothing -> trace "tasty-plutus: Parse failed: ScriptContext" ()
          Just p' ->
            if f d' r' p'
              then trace "tasty-plutus: Pass" ()
              else trace "tasty-plutus: Fail" ()

compileMintingPolicy ::
  forall (redeemer :: Type).
  (FromData redeemer) =>
  CompiledCode (redeemer -> ScriptContext -> Bool) ->
  MintingPolicy
compileMintingPolicy =
  mkMintingPolicyScript . applyCode $$(compile [||wrap||])
  where
    -- See the note on the 'wrap' function for 'compileValidator' for why this
    -- is needed.
    --
    -- Koz
    wrap ::
      (redeemer -> ScriptContext -> Bool) ->
      BuiltinData ->
      BuiltinData ->
      ()
    wrap f r p = case fromBuiltinData r of
      Nothing -> trace "tasty-plutus: Parse failed: Redeemer" ()
      Just r' -> case fromBuiltinData p of
        Nothing -> trace "tasty-plutus: Parse failed: ScriptContext" ()
        Just p' ->
          if f r' p'
            then trace "tasty-plutus: Pass" ()
            else trace "tasty-plutus: Fail" ()

formatScriptError :: ScriptError -> String
formatScriptError =
  renderStyle ourStyle . hang "Script execution error:" 4 . ppDoc
