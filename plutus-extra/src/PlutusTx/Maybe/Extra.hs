{-# OPTIONS_GHC -fno-omit-interface-pragmas #-}

module PlutusTx.Maybe.Extra (maybeMap) where

--------------------------------------------------------------------------------

import Data.Kind (Type)

--------------------------------------------------------------------------------

import PlutusTx.Maybe (Maybe, mapMaybe)
import PlutusTx.Prelude (flip)

--------------------------------------------------------------------------------

-- | Flipped version of `mapMaybe`, to correspond with `>>=`
{-# INLINEABLE maybeMap #-}
maybeMap :: forall (a :: Type) (b :: Type). [a] -> (a -> Maybe b) -> [b]
maybeMap = flip mapMaybe
