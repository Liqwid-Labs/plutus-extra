module Test.Tasty.Plutus.Internal.Estimate (
  spenderEstimate,
  minterEstimate,
) where

import Data.Bifunctor (second)
import Data.Kind (Type)
import Plutus.V1.Ledger.Api (ExBudget, toBuiltinData)
import Plutus.V1.Ledger.Scripts (
  Context (Context),
  Datum (Datum),
  Redeemer (Redeemer),
  ScriptError,
  runMintingPolicyScript,
  runScript,
 )
import Test.Plutus.ContextBuilder (
  Naming,
  Purpose (ForMinting, ForSpending),
  TestUTXO (TestUTXO),
  mintingScriptContext,
  spendingScriptContext,
 )
import Test.Tasty.Options (OptionSet)
import Test.Tasty.Plutus.Internal.Env (prepareConf)
import Test.Tasty.Plutus.Internal.TestScript (
  TestScript (TestMintingPolicy, TestValidator),
 )
import Test.Tasty.Plutus.TestData (
  TestItems (ItemsForMinting, ItemsForSpending),
 )

spenderEstimate ::
  forall (d :: Type) (r :: Type) (n :: Naming).
  OptionSet ->
  TestScript ( 'ForSpending d r) ->
  TestItems ( 'ForSpending d r) n ->
  Either ScriptError ExBudget
spenderEstimate opts ts ti = case ti of
  ItemsForSpending dat red v cb _ -> case ts of
    TestValidator _ val ->
      let conf = prepareConf id opts
          context = spendingScriptContext conf cb (TestUTXO dat v)
          dat' = Datum . toBuiltinData $ dat
          red' = Redeemer . toBuiltinData $ red
          context' = Context . toBuiltinData $ context
       in second fst . runScript context' val dat' $ red'

minterEstimate ::
  forall (r :: Type) (n :: Naming).
  OptionSet ->
  TestScript ( 'ForMinting r) ->
  TestItems ( 'ForMinting r) n ->
  Either ScriptError ExBudget
minterEstimate opts ts ti = case ti of
  ItemsForMinting red tasks cb _ -> case ts of
    TestMintingPolicy _ mp ->
      let conf = prepareConf id opts
          context = mintingScriptContext conf cb tasks
          red' = Redeemer . toBuiltinData $ red
          context' = Context . toBuiltinData $ context
       in second fst . runMintingPolicyScript context' mp $ red'
