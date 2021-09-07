{-# LANGUAGE AllowAmbiguousTypes #-}

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
  -- * Validator context type
  WithValidator,

  -- * Testing API
  withValidator,
  shouldn'tValidateSpending,
  shouldValidateSpending,
) where

import Control.Monad (guard)
import Control.Monad.RWS.Strict (RWS, evalRWS, tell)
import Control.Monad.Reader (MonadReader, asks)
import Data.Functor (($>))
import Data.Kind (Type)
import Data.Map.Strict qualified as Map
import Data.Sequence (Seq)
import Data.Sequence qualified as Seq
import Data.Tagged (Tagged (Tagged))
import Data.Validation (Validation (Failure, Success))
import GHC.Exts (toList)
import Plutus.V1.Ledger.Contexts (ScriptContext)
import PlutusTx.IsData.Class (FromData)
import Prettyprinter (
  Doc,
  defaultLayoutOptions,
  hang,
  hardline,
  layoutPretty,
  pretty,
  viaShow,
  vsep,
  (<+>),
 )
import Prettyprinter.Render.String (renderString)
import Test.Tasty (testGroup)
import Test.Tasty.Plutus.Context (
  ContextBuilder,
  DecodeFailure,
  Purpose (ForSpending),
  compile,
 )
import Test.Tasty.Providers (
  IsTest (run, testOptions),
  TestTree,
  singleTest,
  testFailed,
  testPassed,
 )
import Type.Reflection (Typeable, tyConName, typeRep, typeRepTyCon)
import Witherable (mapMaybe)
import Prelude hiding (map)

{- | Provides a monadic API for composing tests against the same validator.
 While it has all the capabilities of a monad, you mostly won't need them; the
 intended usage is:

 > withValidator "My lovely validator" fooValidator $ do
 >    shouldValidate "Normal" ctxBuilderGood
 >    shouldn'tValidate "Missing input" ctxBuilderNoInput
 >    ...

 For convenience, we also let you access the validator in the environment
 using 'ask', and \'hot modify\' using 'local', as per 'MonadReader'.

 @since 1.0
-}
newtype WithValidator (datum :: Type) (redeemer :: Type) (a :: Type)
  = WithValidator (RWS (datum -> redeemer -> ScriptContext -> Bool) (Seq TestTree) () a)
  deriving
    ( -- | @since 1.0
      Functor
    , -- | @since 1.0
      Applicative
    , -- | @since 1.0
      Monad
    , -- | @since 1.0
      MonadReader (datum -> redeemer -> ScriptContext -> Bool)
    )
    via (RWS (datum -> redeemer -> ScriptContext -> Bool) (Seq TestTree) ())

{- | Given a validator and a sequence of tests to execute, produce a 'TestTree'
 that will execute them. See 'WithValidator' for an example of use.

 @since 1.0
-}
withValidator ::
  forall (datum :: Type) (redeemer :: Type).
  String ->
  (datum -> redeemer -> ScriptContext -> Bool) ->
  WithValidator datum redeemer () ->
  TestTree
withValidator name val (WithValidator comp) = case evalRWS comp val () of
  ((), tests) -> testGroup name . toList $ tests

{- | Specify that, in the given context, the datum and redeemer should parse,
 but that validation should fail.

 @since 1.0
-}
shouldn'tValidateSpending ::
  forall (datum :: Type) (redeemer :: Type).
  ( FromData datum
  , FromData redeemer
  , Typeable datum
  , Typeable redeemer
  , Show datum
  , Show redeemer
  ) =>
  String ->
  ContextBuilder 'ForSpending ->
  WithValidator datum redeemer ()
shouldn'tValidateSpending name cb = WithValidator $ do
  tt <- asks (singleTest name . SpendingTest Fail cb)
  tell . Seq.singleton $ tt

{- | Specify that, in the given context, the validation should succeed.

 @since 1.0
-}
shouldValidateSpending ::
  forall (datum :: Type) (redeemer :: Type).
  ( FromData datum
  , FromData redeemer
  , Typeable datum
  , Typeable redeemer
  , Show datum
  , Show redeemer
  ) =>
  String ->
  ContextBuilder 'ForSpending ->
  WithValidator datum redeemer ()
shouldValidateSpending name cb = WithValidator $ do
  tt <- asks (singleTest name . SpendingTest Pass cb)
  tell . Seq.singleton $ tt

-- Helpers

data Outcome = Fail | Pass

data SpendingTest (datum :: Type) (redeemer :: Type)
  = SpendingTest
      Outcome
      (ContextBuilder 'ForSpending)
      (datum -> redeemer -> ScriptContext -> Bool)

instance
  ( Typeable datum
  , Typeable redeemer
  , FromData datum
  , FromData redeemer
  , Show datum
  , Show redeemer
  ) =>
  IsTest (SpendingTest datum redeemer)
  where
  run _ (SpendingTest expected cb val) _ =
    pure $ case compile @datum @redeemer cb of
      Failure errs ->
        testFailed . renderDecodeFailures @datum @redeemer $ errs
      Success map ->
        let result = mapMaybe go map
         in case Map.toList result of
              [] -> testPassed ""
              xs -> testFailed . renderTestFailures expected $ xs
    where
      go ::
        (datum, redeemer, ScriptContext) ->
        Maybe (datum, redeemer, ScriptContext)
      go args@(dat, red, context) =
        let result = val dat red context
         in case expected of
              Fail -> guard result $> args -- throw away failures
              Pass -> guard (not result) $> args -- throw away passes
              -- None for now.

  testOptions = Tagged []

renderDecodeFailures ::
  forall (datum :: Type) (redeemer :: Type).
  (Typeable datum, Typeable redeemer) =>
  [DecodeFailure] ->
  String
renderDecodeFailures = renderString . layoutPretty defaultLayoutOptions . go
  where
    go :: forall (ann :: Type). [DecodeFailure] -> Doc ann
    go errs =
      "Some inputs failed to decode."
        <> hardline
        <> "Datum type:" <+> prettyTypeName @datum
        <> hardline
        <> "Redeemer type:" <+> prettyTypeName @redeemer
        <> hardline
        <> (hang 4 . vsep . fmap pretty $ errs)

renderTestFailures ::
  forall (datum :: Type) (redeemer :: Type).
  (Typeable datum, Show datum, Typeable redeemer, Show redeemer) =>
  Outcome ->
  [(Integer, (datum, redeemer, ScriptContext))] ->
  String
renderTestFailures expected unexpecteds =
  renderString . layoutPretty defaultLayoutOptions $ go
  where
    go :: forall (ann :: Type). Doc ann
    go =
      "Unexpected" <+> pretty what <> ":"
        <> hardline
        <> (hang 4 . vsep . fmap prettyUnexpected $ unexpecteds)
    what :: String
    what = case expected of
      Fail -> "success(es)"
      Pass -> "failure(s)"
    prettyUnexpected ::
      forall (ann :: Type).
      (Integer, (datum, redeemer, ScriptContext)) ->
      Doc ann
    prettyUnexpected (ix, (dat, red, sc)) =
      "Input" <+> pretty ix <> hardline
        <> "Datum:"
        <> hardline
        <> hang 4 (prettyDatum dat)
        <> hardline
        <> "Redeemer:"
        <> hardline
        <> hang 4 (prettyRedeemer red)
        <> hardline
        <> "ScriptContext:"
        <> hardline
        <> hang 4 (viaShow sc)

prettyDatum ::
  forall (datum :: Type) (ann :: Type).
  (Typeable datum, Show datum) =>
  datum ->
  Doc ann
prettyDatum dat =
  "Type:" <+> prettyTypeName @datum <> hardline
    <> "Representation:" <+> viaShow dat

prettyRedeemer ::
  forall (redeemer :: Type) (ann :: Type).
  (Typeable redeemer, Show redeemer) =>
  redeemer ->
  Doc ann
prettyRedeemer red =
  "Type:" <+> prettyTypeName @redeemer <> hardline
    <> "Representation:" <+> viaShow red

prettyTypeName ::
  forall (a :: Type) (ann :: Type).
  (Typeable a) =>
  Doc ann
prettyTypeName = pretty . tyConName . typeRepTyCon $ typeRep @a
