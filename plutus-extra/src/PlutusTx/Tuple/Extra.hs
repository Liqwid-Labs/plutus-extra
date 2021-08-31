{-# OPTIONS_GHC -fno-omit-interface-pragmas #-}

module PlutusTx.Tuple.Extra (uncurry, uncurry3) where

--------------------------------------------------------------------------------

import Data.Kind (Type)

--------------------------------------------------------------------------------

-- | Converts a curried function to a function on pairs.
{-# INLINEABLE uncurry #-}
uncurry :: forall (a :: Type) (b :: Type) (c :: Type). (a -> b -> c) -> (a, b) -> c
uncurry f (x, y) = f x y

-- | Converts a curried function to a function on a triple.
{-# INLINEABLE uncurry3 #-}
uncurry3 ::
  forall (a :: Type) (b :: Type) (c :: Type) (d :: Type).
  (a -> b -> c -> d) ->
  (a, b, c) ->
  d
uncurry3 f (x, y, z) = f x y z
