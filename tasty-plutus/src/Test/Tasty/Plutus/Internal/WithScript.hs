module Test.Tasty.Plutus.Internal.WithScript (
  WithScript (..),
) where

import Control.Monad.RWS.Strict (MonadReader (ask, local), RWS)
import Data.Kind (Type)
import Data.Sequence (Seq)
import Test.Plutus.ScriptContext.Internal.Context (
  Purpose (ForMinting, ForSpending),
 )
import Test.Tasty.Plutus.Internal.TestScript (TestScript)
import Test.Tasty.Providers (TestTree)

{- | Provides a monadic API for composing tests against the same validator or
 minting policy. While it has all the capabilities of a monad, you mostly
 won't need them. An example of the intended usage is:

 > withTestScript "Testing my validator" myValidator $ do
 >    shouldValidate "Valid case" validData validContext
 >    shouldn'tValidate "Invalid context" validData invalidContext
 >    shouldn'tValidate "Invalid data" invalidData validContext
 >    shouldn'tValidate "Everything is bad" invalidData invalidContext
 >    scriptProperty "Some property" myGenerator mkContext
 >    ...

 @since 3.0
-}
data WithScript (p :: Purpose) (a :: Type) where
  WithSpending ::
    forall (a :: Type) (d :: Type) (r :: Type).
    RWS (TestScript ( 'ForSpending d r)) (Seq TestTree) () a ->
    WithScript ( 'ForSpending d r) a
  WithMinting ::
    forall (a :: Type) (r :: Type).
    RWS (TestScript ( 'ForMinting r)) (Seq TestTree) () a ->
    WithScript ( 'ForMinting r) a

-- | @since 1.0
deriving stock instance Functor (WithScript p)

-- | @since 1.0
instance Applicative (WithScript ( 'ForSpending d r)) where
  {-# INLINEABLE pure #-}
  pure = WithSpending . pure
  {-# INLINEABLE (<*>) #-}
  WithSpending fs <*> WithSpending xs = WithSpending (fs <*> xs)

-- | @since 1.0
instance Applicative (WithScript ( 'ForMinting r)) where
  {-# INLINEABLE pure #-}
  pure = WithMinting . pure
  {-# INLINEABLE (<*>) #-}
  WithMinting fs <*> WithMinting xs = WithMinting (fs <*> xs)

-- | @since 1.0
instance Monad (WithScript ( 'ForSpending d r)) where
  {-# INLINEABLE (>>=) #-}
  WithSpending xs >>= f = WithSpending $ do
    x <- xs
    let (WithSpending ys) = f x
    ys

-- | @since 1.0
instance Monad (WithScript ( 'ForMinting r)) where
  {-# INLINEABLE (>>=) #-}
  WithMinting xs >>= f = WithMinting $ do
    x <- xs
    let (WithMinting ys) = f x
    ys

-- | @since 3.0
instance MonadReader (TestScript ( 'ForSpending d r)) (WithScript ( 'ForSpending d r)) where
  {-# INLINEABLE ask #-}
  ask = WithSpending ask
  {-# INLINEABLE local #-}
  local f (WithSpending comp) = WithSpending . local f $ comp

-- | @since 3.0
instance MonadReader (TestScript ( 'ForMinting r)) (WithScript ( 'ForMinting r)) where
  {-# INLINEABLE ask #-}
  ask = WithMinting ask
  {-# INLINEABLE local #-}
  local f (WithMinting comp) = WithMinting . local f $ comp
