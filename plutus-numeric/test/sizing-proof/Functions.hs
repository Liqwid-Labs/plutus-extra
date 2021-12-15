{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Functions (
  round1,
  round2,
  round3,
  round4,
  fOurs,
  fTheirs,
) where

import PlutusTx.Prelude
import PlutusTx.Ratio qualified as Theirs
import PlutusTx.Rational qualified as Ours

{-# INLINEABLE round1 #-}
round1 :: [Rational] -> [Integer]
round1 (x1 : _ : _ : _ : _) = [round x1, 1, 1, 1]

{-# INLINEABLE round2 #-}
round2 :: [Rational] -> [Integer]
round2 (x1 : x2 : _ : _ : _) = [round x1, round x2, 1, 1]

{-# INLINEABLE round3 #-}
round3 :: [Rational] -> [Integer]
round3 (x1 : x2 : x3 : _ : _) = [round x1, round x2, round x3, 1]

{-# INLINEABLE round4 #-}
round4 :: [Rational] -> [Integer]
round4 (x1 : x2 : x3 : x4 : _) = [round x1, round x2, round x3, round x4]

{-# INLINEABLE fOurs #-}
fOurs :: Ours.Rational -> Ours.Rational -> Integer
fOurs x y = Ours.round . Ours.negate $ cube (Ours.abs x) + cube (Ours.abs y)
  where
    cube :: Ours.Rational -> Ours.Rational
    cube z = z * z * z

{-# INLINEABLE fTheirs #-}
fTheirs :: Rational -> Rational -> Integer
fTheirs x y = round . negate $ cube (Theirs.abs x) + cube (Theirs.abs y)
  where
    cube :: Rational -> Rational
    cube z = z * z * z
