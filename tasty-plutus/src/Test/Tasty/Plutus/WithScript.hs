{-# LANGUAGE Trustworthy #-}

{- |
 Module: Test.Tasty.Plutus.WithScript
 Copyright: (C) MLabs 2021
 License: Apache 2.0
 Maintainer: Koz Ross <koz@mlabs.city>
 Portability: GHC only
 Stability: Experimental
-}
module Test.Tasty.Plutus.WithScript (
  -- * Environment monad
  WithScript,

  -- * Helper functions
  withValidator,
  withMintingPolicy,
) where

import Control.Monad.RWS.Strict (evalRWS)
import Data.Kind (Type)
import GHC.Exts (toList)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Plutus.Internal.Context (
  Purpose (ForMinting, ForSpending),
 )
import Test.Tasty.Plutus.Internal.TestScript (TestScript)
import Test.Tasty.Plutus.Internal.WithScript (
  WithScript (WithMinting, WithSpending),
 )
import Prelude

{- | Given the name for the tests, a 'TestValidator', and a collection of
 spending-related tests, execute all of them as a 'TestTree'.

 = Usage

 > myTests :: TestTree
 > myTests = withValidator "Testing my spending" myTestValidator $ do
 >    shouldValidate "Valid case" validData validContext
 >    shouldn'tValidate "Invalid context" validData invalidContext
 >    shouldn'tValidate "Invalid data" invalidData validContext
 >    shouldn'tValidate "Everything is bad" invalidData invalidContext
 >    shouldValidateTracing "Gotta get good messages" tracePred validData validContext
 >    shouldn'tValidateTracing "Oh damn" tracePred invalidData validContext
 >    scriptProperty "Some property" myGenerator mkContext
 >    ...

 = Important note

 Unless your 'TestValidator' has been prepared using 'toTestValidator', this will
 likely not behave as intended. We use 'WrappedValidator' to prevent
 other wrapper functions from being used.

 @since 6.0
-}
withValidator ::
  forall (d :: Type) (r :: Type).
  String ->
  TestScript ( 'ForSpending d r) ->
  WithScript ( 'ForSpending d r) () ->
  TestTree
withValidator name val (WithSpending comp) =
  case evalRWS comp val () of
    ((), tests) -> testGroup name . toList $ tests

{- | Given the name for the tests, a 'TestMintingPolicy', and a collection of
 minting-related tests, execute all of them as a 'TestTree'.

 = Usage

 > myTests :: TestTree
 > myTests = withMintingPolicy "Testing my minting" myTestMintingPolicy $ do
 >    shouldValidate "Valid case" validData validContext
 >    shouldn'tValidate "Invalid context" validData invalidContext
 >    shouldn'tValidate "Invalid data" invalidData validContext
 >    shouldn'tValidate "Everything is bad" invalidData invalidContext
 >    scriptProperty "Some property" myGenerator mkContext
 >    ...

 = Important note

 Unless your 'TestMintingPolicy' has been prepared using 'toTestMintingPolicy',
 this will likely not behave as intended. We use 'WrappedMintingPolicy' to prevent
 other wrapper functions from being used.

 @since 6.0
-}
withMintingPolicy ::
  forall (r :: Type).
  String ->
  TestScript ( 'ForMinting r) ->
  WithScript ( 'ForMinting r) () ->
  TestTree
withMintingPolicy name mp (WithMinting comp) =
  case evalRWS comp mp () of
    ((), tests) -> testGroup name . toList $ tests
