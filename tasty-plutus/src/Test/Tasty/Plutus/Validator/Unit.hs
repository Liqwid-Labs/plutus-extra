module Test.Tasty.Plutus.Validator.Unit (
  -- * Testing API

  -- shouldn'tParse,
  -- shouldn'tValidate,
  -- shouldValidate,
) where

import Data.Foldable (traverse_)
import Data.Kind (Type)
import Data.Tagged (Tagged (Tagged))
import PlutusTx.Builtins (BuiltinData)
import PlutusTx.IsData.Class (FromData (fromBuiltinData))
import Test.Tasty.Providers (
  IsTest (run, testOptions),
  testFailed,
  testPassed,
 )
import Type.Reflection (Typeable)

{-
-- | Specify that, in the given context, the datum or the redeemer are invalid,
-- and should not parse at all.
--
-- @since 1.0
shouldn'tParse ::
  forall (datum :: Type) (redeemer :: Type) (a :: Type) .
  (FromData datum, FromData redeemer, Typeable datum, Typeable redeemer) =>
  ContextBuilder a ->
  String ->
  (datum -> redeemer -> ScriptContext -> Bool) ->
  TestTree
shouldn'tParse cb name _ =
  singleTest name . NoParse @datum @redeemer . compileOwnInputs $ cb

-- | Specify that, in the given context, the datum and redeemer should parse,
-- but that validation should fail.
--
-- @since 1.0
shouldn'tValidate ::
  forall (datum :: Type) (redeemer :: Type) (a :: Type) .
  (FromData datum, FromData redeemer, Typeable datum, Typeable redeemer) =>
  ContextBuilder a ->
  String ->
  (datum -> redeemer -> ScriptContext -> Bool) ->
  TestTree
shouldn'tValidate cb name val = _

-- | @since 1.0
shouldValidate ::
  ContextBuilder ->
  String ->
  (ScriptContext -> Bool) ->
  TestTree
shouldValidate cb name =
  singleTest name . ValidatorTest ValidationSuccess
-}

-- Helpers

newtype NoParse (datum :: Type) (redeemer :: Type)
  = NoParse [(BuiltinData, BuiltinData)]

instance
  (Typeable datum, Typeable redeemer, FromData datum, FromData redeemer) =>
  IsTest (NoParse datum redeemer)
  where
  run _ (NoParse ds) _ = pure $ case traverse_ (uncurry go) ds of
    Nothing -> testPassed "Parse failure occurred."
    Just () -> testFailed "Everything parsed successfully."
    where
      go :: BuiltinData -> BuiltinData -> Maybe ()
      go dat red = do
        _ <- fromBuiltinData @datum dat
        _ <- fromBuiltinData @redeemer red
        pure ()

  -- None for now.
  testOptions = Tagged []

{-
compileOwnInputs ::
  forall (a :: Type) .
  ContextBuilder a ->
  [(BuiltinData, BuiltinData)]
compileOwnInputs (ContextBuilder (inputs, _, _, _)) = foldMap go inputs
  where
    go :: Input -> [(BuiltinData, BuiltinData)]
    go (Input t _) = case t of
      OwnInput dat red -> [(dat, red)]
      _ -> []
-}
