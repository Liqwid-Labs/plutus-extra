{-# OPTIONS_GHC -fno-omit-interface-pragmas #-}

module PlutusTx.Either.Extra (
  maybeToEither,
  left,
  right,
) where

--------------------------------------------------------------------------------

import Data.Kind (Type)

--------------------------------------------------------------------------------

import PlutusTx.Bifunctor (first, second)
import PlutusTx.Prelude (Either (Left, Right), Maybe (Just, Nothing))

--------------------------------------------------------------------------------

-- | Convert a `Maybe` to an @`Either` e@, given an error @e@ for the `Nothing` case
{-# INLINEABLE maybeToEither #-}
maybeToEither :: forall (e :: Type) (a :: Type). e -> Maybe a -> Either e a
maybeToEither e Nothing = Left e
maybeToEither _ (Just a) = Right a

--------------------------------------------------------------------------------

-- | Map a function over the `Left` value of an `Either`
{-# INLINEABLE left #-}
left :: forall (a :: Type) (b :: Type) (c :: Type). (a -> b) -> Either a c -> Either b c
left = first

-- | Map a function over the `Right` value of an `Either`
{-# INLINEABLE right #-}
right :: forall (a :: Type) (b :: Type) (c :: Type). (b -> c) -> Either a b -> Either a c
right = second
