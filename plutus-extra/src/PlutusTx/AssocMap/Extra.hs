{-# OPTIONS_GHC -fno-omit-interface-pragmas #-}

module PlutusTx.AssocMap.Extra (alter) where

--------------------------------------------------------------------------------

import Data.Kind (Type)

--------------------------------------------------------------------------------

import PlutusTx.AssocMap (Map)
import PlutusTx.AssocMap qualified as AssocMap
import PlutusTx.Prelude (Eq, Maybe (Just, Nothing))

--------------------------------------------------------------------------------

-- It would be nice if PlutusTx.AssocMap exposed unionWith.

-- | Alter a value that may or may not already exist in a map given a key
{-# INLINEABLE alter #-}
alter ::
  forall (k :: Type) (v :: Type).
  Eq k =>
  k ->
  (Maybe v -> Maybe v) ->
  Map k v ->
  Map k v
alter k f m =
  case f (AssocMap.lookup k m) of
    Nothing ->
      AssocMap.delete k m
    Just v ->
      AssocMap.insert k v m
