{-# LANGUAGE AllowAmbiguousTypes #-}
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
  withTestScript,
) where

import Control.Monad.RWS.Strict (RWS, evalRWS)
import Data.Sequence (Seq)
import GHC.Exts (toList)
import Test.Tasty (TestTree, testGroup)
import Test.Plutus.ScriptContext.Internal.Context (Purpose)
import Test.Tasty.Plutus.Internal.DumpScript (dumpScript)
import Test.Tasty.Plutus.Internal.TestScript (TestScript)
import Test.Tasty.Plutus.Internal.WithScript (
  WithScript (WithMinting, WithSpending),
 )
import Prelude

{- | Given the name for the tests, a 'TestScript', and a collection of
 tests, execute all of them as a 'TestTree'.
 = Usage
 > validatorTests :: TestTree
 > validatorTests = withTestScript "Testing my validator" myTestValidator $ do
 >    shouldValidate "Valid case" validData validContext
 >    shouldn'tValidate "Invalid context" validData invalidContext
 >    shouldn'tValidate "Invalid data" invalidData validContext
 >    shouldn'tValidate "Everything is bad" invalidData invalidContext
 >    shouldValidateTracing "Gotta get good messages" tracePred validData validContext
 >    shouldn'tValidateTracing "Oh damn" tracePred invalidData validContext
 >    scriptProperty "Some property" myGenerator mkContext
 >    ...
 >  mintingPolicyTests :: TestTree
 >  mintingPolicyTests = withTestScript "Testing my minting" myTestMintingPolicy $ do
 >    shouldValidate "Valid case" validData validContext
 >    shouldn'tValidate "Invalid context" validData invalidContext
 >    shouldn'tValidate "Invalid data" invalidData validContext
 >    shouldn'tValidate "Everything is bad" invalidData invalidContext
 >    scriptProperty "Some property" myGenerator mkContext
 >    ...
 = Important note
 Unless your 'TestScript' has been prepared using 'toTestValidator'
 or 'toTestMintingPolicy', this will likely not behave as intended.
 We use 'WrappedValidator' to prevent other wrapper functions from being used.
 @since 6.0
-}
withTestScript ::
  forall (p :: Purpose).
  String ->
  TestScript p ->
  WithScript p () ->
  TestTree
withTestScript name ts = \case
  WithSpending comp -> go comp
  WithMinting comp -> go comp
  where
    go :: RWS (TestScript p) (Seq TestTree) () () -> TestTree
    go rws =
      let ((), tests) = evalRWS rws ts ()
       in testGroup name . (dumpScript name ts :) . toList $ tests
