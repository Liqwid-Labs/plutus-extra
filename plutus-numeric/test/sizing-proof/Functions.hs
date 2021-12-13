{-# LANGUAGE NoImplicitPrelude #-}

module Functions (
  round1,
  round2,
  round3,
  round4
  ) where

import PlutusTx.Prelude
import PlutusTx.Ratio (Rational, round)

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
