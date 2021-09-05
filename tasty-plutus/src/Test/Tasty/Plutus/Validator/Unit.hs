{-# LANGUAGE AllowAmbiguousTypes #-}

module Test.Tasty.Plutus.Validator.Unit (
  -- * Testing API
  shouldn'tValidateSpending,
  shouldValidateSpending,
) where

import Control.Monad (guard)
import Data.Functor (($>))
import Data.Kind (Type)
import Data.Map.Strict qualified as Map
import Data.Tagged (Tagged (Tagged))
import Data.Validation (Validation (Failure, Success))
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
  ContextBuilder 'ForSpending ->
  String ->
  (datum -> redeemer -> ScriptContext -> Bool) ->
  TestTree
shouldn'tValidateSpending cb name = singleTest name . SpendingTest Fail cb

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
  ContextBuilder 'ForSpending ->
  String ->
  (datum -> redeemer -> ScriptContext -> Bool) ->
  TestTree
shouldValidateSpending cb name = singleTest name . SpendingTest Pass cb

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
