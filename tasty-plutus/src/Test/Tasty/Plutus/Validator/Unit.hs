module Test.Tasty.Plutus.Validator.Unit (
  -- * Testing API
  shouldn'tValidateSpending,
  shouldValidateSpending,
) where

import Data.Functor (($>))
import Control.Monad (guard)
import Data.Map.Strict qualified as Map
import Witherable (mapMaybe)
import Data.Foldable (traverse_)
import Data.Kind (Type)
import Data.Tagged (Tagged (Tagged))
import PlutusTx.Builtins (BuiltinData)
import PlutusTx.IsData.Class (FromData (fromBuiltinData))
import Test.Tasty.Providers (
  IsTest (run, testOptions),
  testFailed,
  testPassed,
  TestTree,
  singleTest,
  Result,
 )
import Type.Reflection (Typeable)
import Test.Tasty.Plutus.Context (Purpose (ForSpending), ContextBuilder, 
  compile, DecodeFailure)
import Plutus.V1.Ledger.Contexts (ScriptContext)
import Data.Validation (Validation(Failure, Success))

-- | Specify that, in the given context, the datum and redeemer should parse,
-- but that validation should fail.
--
-- @since 1.0
shouldn'tValidateSpending ::
  forall (datum :: Type) (redeemer :: Type) .
  (FromData datum, FromData redeemer, Typeable datum, Typeable redeemer) =>
  ContextBuilder 'ForSpending ->
  String ->
  (datum -> redeemer -> ScriptContext -> Bool) ->
  TestTree
shouldn'tValidateSpending cb name = singleTest name . SpendingTest Fail cb

-- | Specify that, in the given context, the validation should succeed.
--
-- @since 1.0
shouldValidateSpending :: 
  forall (datum :: Type) (redeemer :: Type) . 
  (FromData datum, FromData redeemer, Typeable datum, Typeable redeemer) => 
  ContextBuilder 'ForSpending -> 
  String -> 
  (datum -> redeemer -> ScriptContext -> Bool) -> 
  TestTree
shouldValidateSpending cb name = singleTest name . SpendingTest Pass cb

-- Helpers

data Outcome = Fail | Pass

data SpendingTest (datum :: Type) (redeemer :: Type) = 
  SpendingTest Outcome
               (ContextBuilder 'ForSpending) 
               (datum -> redeemer -> ScriptContext -> Bool)

instance (Typeable datum, Typeable redeemer, FromData datum, FromData redeemer) => 
  IsTest (SpendingTest datum redeemer) where
  run _ (SpendingTest expected cb val) _ = 
    pure $ case compile @datum @redeemer cb of 
      Failure errs -> reportParseFailure errs
      Success map -> let result = mapMaybe (go expected val) map in
        case Map.toList result of 
          [] -> testPassed ""
          xs -> testFailed . renderTestFailures expected $ xs
    where
      go :: 
        Outcome -> 
        (datum -> redeemer -> ScriptContext -> Bool) -> 
        (datum, redeemer, ScriptContext) -> 
        Maybe (datum, redeemer, ScriptContext)
      go expected val args@(dat, red, context) = 
        let result = val dat red context in
          case expected of 
            Fail -> guard result $> args -- throw away failures
            Pass -> guard (not result) $> args -- throw away passes
  -- None for now.
  testOptions = Tagged []

reportParseFailure :: [DecodeFailure] -> Result
reportParseFailure errs = _

renderTestFailures :: 
  forall (datum :: Type) (redeemer :: Type) . 
  Outcome -> 
  [(Integer, (datum, redeemer, ScriptContext))] -> 
  String
renderTestFailures expected unexpecteds = _
