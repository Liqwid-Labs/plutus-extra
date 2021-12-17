{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE NoImplicitPrelude #-}

{- | Module: PlutusTx.Rational
 Copyright: (C) MLabs 2021
 License: Apache 2.0
 Maintainer: Koz Ross <koz@mlabs.city>
 Portability: GHC only
 Stability: Experimental

 An optimized rational number representation.
-}
module PlutusTx.Rational (
  -- * Type
  Rational,

  -- * Construction
  Internal.ratio,
  QQ.frac,
  QQ.dec,

  -- * Constants
  half,

  -- * Conversion
  fromInteger,
  fromGHC,
  Internal.toGHC,

  -- ** RatioSchema
  Internal.RatioSchema (RatioSchema),

  -- * Other functionality
  negate,
  Internal.numerator,
  Internal.denominator,
  abs,
  properFraction,
  recip,
  truncate,
  round,
) where

import Data.Ratio qualified as GHC
import PlutusTx.Prelude hiding (Rational, fromInteger, negate, round)
import PlutusTx.Prelude qualified as Plutus
import PlutusTx.Rational.Internal (Rational (Rational))
import PlutusTx.Rational.Internal qualified as Internal
import PlutusTx.Rational.QQ qualified as QQ

{- | 0.5. Provided for compatibility.

 @since 4.0
-}
{-# INLINEABLE half #-}
half :: Rational
half = Rational 1 2

{- | Converts an 'Integer' into the equivalent 'Rational'.

 @since 4.0
-}
{-# INLINEABLE fromInteger #-}
fromInteger :: Integer -> Rational
fromInteger num = Rational num one

{- | Converts a GHC 'GHC.Rational' to a 'Rational', preserving value. Does not
 work on-chain.

 @since 4.0
-}
fromGHC :: GHC.Rational -> Rational
fromGHC r = GHC.numerator r Internal.% GHC.denominator r

{- | Produces the additive inverse of its argument.

 @since 4.0
-}
{-# INLINEABLE negate #-}
negate :: Rational -> Rational
negate (Rational n d) = Rational (Plutus.negate n) d

{- | Returns the absolute value of its argument. Specialized for 'Rational's.

 @since 4.0
-}
{-# INLINEABLE abs #-}
abs :: Rational -> Rational
abs rat@(Rational n d)
  | n < zero = Rational (Plutus.negate n) d
  | otherwise = rat

{- | @'properFraction' r@ returns the pair @(n, f)@, such that all of the
 following hold:

 * @'fromInteger' n + f = r@;
 * @n@ and @f@ both have the same sign as @r@; and
 * @'abs' f '<' 'one'@.

 @since 4.0
-}
{-# INLINEABLE properFraction #-}
properFraction :: Rational -> (Integer, Rational)
properFraction (Rational n d) =
  (n `quotient` d, Rational (n `remainder` d) d)

{- | Gives the reciprocal of the argument; specifically, for @r '/=' 'zero'@, @r
 '*' 'recip' r = 'one'@.

 = Note

 The reciprocal of zero is mathematically undefined; thus, @'recip' 'zero'@
 will error. Use with care.

 @since 4.0
-}
{-# INLINEABLE recip #-}
recip :: Rational -> Rational
recip (Rational n d)
  | n == zero = error ()
  | n < zero = Rational (Plutus.negate d) (Plutus.negate n)
  | otherwise = Rational d n

{- | Returns the whole-number part of its argument, dropping any leftover
 fractional part. More precisely, @'truncate' r = n@, where @(n, _) =
 'properFraction' r@, but much more efficiently.

 @since 4.0
-}
{-# INLINEABLE truncate #-}
truncate :: Rational -> Integer
truncate (Rational n d) = n `quotient` d

{- | @'round' r@ returns the nearest 'Integer' value to @r@. If @r@ is
 equidistant between two values, the even value will be given.

 @since 4.0
-}
{-# INLINEABLE round #-}
round :: Rational -> Integer
round x =
  let (n, r) = properFraction x
      m = if r < zero then n - one else n + one
      flag = abs r - half
   in if
          | flag < zero -> n
          | flag == zero -> if even n then n else m
          | otherwise -> m
