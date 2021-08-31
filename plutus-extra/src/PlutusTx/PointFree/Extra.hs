-- | Useful point-free functions missing from PlutusTx
module PlutusTx.PointFree.Extra (
  (&),
  (>>>),
  (<<<),
  (<&>),
) where

--------------------------------------------------------------------------------

import Data.Kind (Type)

--------------------------------------------------------------------------------

import PlutusTx.Prelude

--------------------------------------------------------------------------------

infixl 1 &
{-# INLINEABLE (&) #-}

-- | Reverse application operator, equivalent to `flip $`
(&) :: forall (a :: Type) (b :: Type). a -> (a -> b) -> b
x & f = f x

infixr 1 >>>, <<<
{-# INLINEABLE (>>>) #-}

-- | Left-to-right function composition
(>>>) :: forall (a :: Type) (b :: Type) (c :: Type). (a -> b) -> (b -> c) -> a -> c
f >>> g = g . f

{-# INLINEABLE (<<<) #-}

-- | Right-to-left function composition
(<<<) :: forall (a :: Type) (b :: Type) (c :: Type). (b -> c) -> (a -> b) -> a -> c
(<<<) = (.)

infixl 1 <&>
{-# INLINEABLE (<&>) #-}

-- | Flipped version of `<$>`
(<&>) ::
  forall (f :: Type -> Type) (a :: Type) (b :: Type).
  Functor f =>
  f a ->
  (a -> b) ->
  f b
xs <&> f = f <$> xs
