{-# OPTIONS_GHC -fno-omit-interface-pragmas #-}

module PlutusTx.List.Extra (zip3, unzip, unzip3) where

--------------------------------------------------------------------------------

import Data.Kind (Type)

--------------------------------------------------------------------------------

import PlutusTx.Prelude (Foldable (foldMap), foldr)

--------------------------------------------------------------------------------

{-# INLINEABLE zip3 #-}

-- | 'zip3' takes three lists and returns a list of triples, analogous to 'zip'.
zip3 ::
  forall (a :: Type) (b :: Type) (c :: Type).
  [a] ->
  [b] ->
  [c] ->
  [(a, b, c)]
zip3 (x : xs) (y : ys) (z : zs) = (x, y, z) : zip3 xs ys zs
zip3 _ _ _ = []

{-# INLINEABLE unzip #-}

-- | unzip transforms a list of pairs into a list of first components and a list of second components.
unzip :: forall (a :: Type) (b :: Type). [(a, b)] -> ([a], [b])
unzip = foldMap singletons
  where
    singletons :: (a, b) -> ([a], [b])
    singletons (x, y) = ([x], [y])

{-# INLINEABLE unzip3 #-}

{- | The 'unzip3' function takes a list of triples and returns three
lists, analogous to 'unzip'.
==== __Examples__

>>> unzip3 []
([],[],[])

>>> unzip3 [(1, 'a', True), (2, 'b', False)]
([1,2],"ab",[True,False])
-}
unzip3 ::
  forall (a :: Type) (b :: Type) (c :: Type).
  [(a, b, c)] ->
  ([a], [b], [c])
unzip3 = foldr (\(a, b, c) ~(as, bs, cs) -> (a : as, b : bs, c : cs)) ([], [], [])
