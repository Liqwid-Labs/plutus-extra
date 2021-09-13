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
import Data.Portray (GPortray (gportray), portrayType)
import Data.Portray.Plutus (portrayScriptContext)
import Data.Portray.Pretty (portrayalToDoc)
import Data.Sequence (Seq)
import Data.Sequence qualified as Seq
import Data.Tagged (Tagged (Tagged))
import Data.Validation (Validation (Failure, Success))
import GHC.Exts (toList)
import GHC.Generics (Generic (Rep, from))
import Plutus.V1.Ledger.Contexts (ScriptContext)
import PlutusTx.IsData.Class (FromData)
import Test.Tasty (testGroup)
import Test.Tasty.Plutus.Context (
  ContextBuilder,
  DecodeFailure,
  Purpose (ForSpending),
  compile,
  renderDecodeFailure,
 )
import Test.Tasty.Providers (
  IsTest (run, testOptions),
  TestTree,
  singleTest,
  testFailed,
  testPassed,
 )
import Text.PrettyPrint (
  Doc,
  Style (lineLength),
  hang,
  integer,
  renderStyle,
  style,
  vcat,
  ($+$),
  (<+>),
 )
import Type.Reflection (Typeable, typeRep)
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
  , Generic datum
  , Generic redeemer
  , GPortray (Rep datum)
  , GPortray (Rep redeemer)
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
  , Generic datum
  , Generic redeemer
  , GPortray (Rep datum)
  , GPortray (Rep redeemer)
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
  , Generic datum
  , Generic redeemer
  , GPortray (Rep datum)
  , GPortray (Rep redeemer)
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
  testOptions = Tagged []

renderDecodeFailures ::
  forall (datum :: Type) (redeemer :: Type).
  (Typeable datum, Typeable redeemer) =>
  [DecodeFailure] ->
  String
renderDecodeFailures = renderStyle ourStyle . go
  where
    go :: [DecodeFailure] -> Doc
    go errs =
      hang "Some inputs failed to decode." 4 $
        ("Datum type:" <+> typeRepDoc @datum)
          $+$ ("Redeemer type:" <+> typeRepDoc @redeemer)
          $+$ (vcat . fmap renderDecodeFailure $ errs)

renderTestFailures ::
  forall (datum :: Type) (redeemer :: Type).
  ( Generic datum
  , Generic redeemer
  , GPortray (Rep datum)
  , GPortray (Rep redeemer)
  ) =>
  Outcome ->
  [(Integer, (datum, redeemer, ScriptContext))] ->
  String
renderTestFailures expected = renderStyle ourStyle . go
  where
    go :: [(Integer, (datum, redeemer, ScriptContext))] -> Doc
    go = hang ("Unexpected" <+> what) 4 . vcat . fmap prettyUnexpected
    what :: Doc
    what = case expected of
      Fail -> "success(es)"
      Pass -> "failure(s)"
    prettyUnexpected ::
      (Integer, (datum, redeemer, ScriptContext)) ->
      Doc
    prettyUnexpected (ix, (dat, red, sc)) =
      hang ("Input" <+> integer ix) 4 $
        hang "Datum:" 4 (genericDoc dat)
          $+$ hang "Redeemer:" 4 (genericDoc red)
          $+$ hang "ScriptContext:" 4 (scDoc sc)

genericDoc ::
  forall (a :: Type).
  (Generic a, GPortray (Rep a)) =>
  a ->
  Doc
genericDoc = portrayalToDoc . gportray . from

scDoc :: ScriptContext -> Doc
scDoc = portrayalToDoc . portrayScriptContext

{-
prettyDatum ::
  forall (datum :: Type) .
  (Typeable datum, Show datum) =>
  datum ->
  Doc
prettyDatum dat = _
  "Type:" <+> prettyTypeName @datum <> hardline
    <> "Representation:" <+> viaShow dat

prettyRedeemer ::
  forall (redeemer :: Type).
  (Typeable redeemer, Show redeemer) =>
  redeemer ->
  Doc
prettyRedeemer red = _
  "Type:" <+> prettyTypeName @redeemer <> hardline
    <> "Representation:" <+> viaShow red
-}

typeRepDoc ::
  forall (a :: Type).
  (Typeable a) =>
  Doc
typeRepDoc = portrayalToDoc . portrayType $ typeRep @a

ourStyle :: Style
ourStyle = style {lineLength = 80}
