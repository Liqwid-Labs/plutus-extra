{-# LANGUAGE RankNTypes #-}

module Test.Tasty.Plutus.Generator (
  Generator (..),
) where

import Data.Kind (Type)
import System.Random.Stateful (StatefulGen)

newtype Generator (a :: Type) = Generator
  { runGenerator :: forall (g :: Type) (m :: Type -> Type). (StatefulGen g m) => g -> m a
  }
  deriving stock (Functor)

instance Applicative Generator where
  {-# INLINEABLE pure #-}
  pure x = Generator (pure . const x)
  {-# INLINEABLE (<*>) #-}
  Generator fs <*> Generator xs = Generator $ \rng -> fs rng <*> xs rng

instance Monad Generator where
  {-# INLINEABLE (>>=) #-}
  Generator xs >>= f = Generator $ \rng -> xs rng >>= (($ rng) . runGenerator . f)
