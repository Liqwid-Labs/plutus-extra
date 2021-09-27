module Test.Tasty.Plutus.Internal (
  WithScript (..),
) where

import Control.Monad.RWS.Strict (RWS)
import Control.Monad.Reader (MonadReader (ask, local))
import Data.Kind (Type)
import Data.Sequence (Seq)
import Plutus.V1.Ledger.Scripts (MintingPolicy, Validator)
import Test.Tasty (TestTree)
import Test.Tasty.Plutus.Context.Internal (Purpose (ForMinting, ForSpending))
import Prelude

{- | Provides a monadic API for composing tests against the same validator or
 minting policy. While it has all the capabilities of a monad, you mostly
 won't need them. An example of the intended usage is:

 > withValidator "Testing my validator" myValidator $ do
 >    shouldValidate "Valid case" validData validContext
 >    shouldn'tValidate "Invalid context" validData invalidContext
 >    shouldn'tValidate "Invalid data" invalidData validContext
 >    shouldn'tValidate "Everything is bad" invalidData invalidContext
 >    ...

 'withMintingPolicy' works similarly.

 @since 3.0
-}
data WithScript (p :: Purpose) (a :: Type) where
  WithSpending ::
    RWS Validator (Seq TestTree) () a ->
    WithScript 'ForSpending a
  WithMinting ::
    RWS MintingPolicy (Seq TestTree) () a ->
    WithScript 'ForMinting a

-- | @since 1.0
deriving stock instance Functor (WithScript p)

-- | @since 1.0
instance Applicative (WithScript 'ForSpending) where
  {-# INLINEABLE pure #-}
  pure = WithSpending . pure
  {-# INLINEABLE (<*>) #-}
  WithSpending fs <*> WithSpending xs = WithSpending (fs <*> xs)

-- | @since 1.0
instance Applicative (WithScript 'ForMinting) where
  {-# INLINEABLE pure #-}
  pure = WithMinting . pure
  {-# INLINEABLE (<*>) #-}
  WithMinting fs <*> WithMinting xs = WithMinting (fs <*> xs)

-- | @since 1.0
instance Monad (WithScript 'ForSpending) where
  {-# INLINEABLE (>>=) #-}
  WithSpending xs >>= f = WithSpending $ do
    x <- xs
    let (WithSpending ys) = f x
    ys

-- | @since 1.0
instance Monad (WithScript 'ForMinting) where
  {-# INLINEABLE (>>=) #-}
  WithMinting xs >>= f = WithMinting $ do
    x <- xs
    let (WithMinting ys) = f x
    ys

-- | @since 3.0
instance MonadReader Validator (WithScript 'ForSpending) where
  {-# INLINEABLE ask #-}
  ask = WithSpending ask
  {-# INLINEABLE local #-}
  local f (WithSpending comp) = WithSpending . local f $ comp

-- | @since 3.0
instance MonadReader MintingPolicy (WithScript 'ForMinting) where
  {-# INLINEABLE ask #-}
  ask = WithMinting ask
  {-# INLINEABLE local #-}
  local f (WithMinting comp) = WithMinting . local f $ comp
