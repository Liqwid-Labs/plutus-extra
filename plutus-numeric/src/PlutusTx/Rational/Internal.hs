{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NoImplicitPrelude #-}

module PlutusTx.Rational.Internal (
  Rational (..),
  mkRational,
  negate,
  fromInteger,
  numerator,
  denominator,
  properFraction,
  recip,
  abs,
  truncate,
  round,
) where

import Control.Monad (guard)
import PlutusTx.Prelude hiding (Rational, fromInteger, negate, round)
import PlutusTx.Prelude qualified as Plutus
import Test.QuickCheck.Arbitrary (
  Arbitrary (arbitrary, shrink),
  CoArbitrary (coarbitrary),
 )
import Test.QuickCheck.Function (Function (function), functionMap)
import Test.QuickCheck.Modifiers (Positive (Positive))
import Prelude qualified

-- Numerator, denominator
-- Numerator is free, denominator always > 0
data Rational = Rational Integer Integer
  deriving stock (Prelude.Eq, Prelude.Show)

instance Eq Rational where
  {-# INLINEABLE (==) #-}
  Rational n d == Rational n' d' = n == n' && d == d'

instance Ord Rational where
  {-# INLINEABLE compare #-}
  compare (Rational n d) (Rational n' d') = compare (n * d') (n' * d)
  {-# INLINEABLE (<=) #-}
  Rational n d <= Rational n' d' = (n * d') <= (n' * d)
  {-# INLINEABLE (>=) #-}
  Rational n d >= Rational n' d' = (n * d') >= (n' * d)
  {-# INLINEABLE (<) #-}
  Rational n d < Rational n' d' = (n * d') < (n' * d)
  {-# INLINEABLE (>) #-}
  Rational n d > Rational n' d' = (n * d') > (n' * d)

instance Prelude.Ord Rational where
  compare (Rational n d) (Rational n' d') =
    Prelude.compare (n Prelude.* d') (n' Prelude.* d)
  Rational n d <= Rational n' d' =
    (n Prelude.* d') Prelude.<= (n' Prelude.* d)
  Rational n d >= Rational n' d' =
    (n Prelude.* d') Prelude.>= (n' Prelude.* d)
  Rational n d < Rational n' d' =
    (n Prelude.* d') Prelude.< (n' Prelude.* d)
  Rational n d > Rational n' d' =
    (n Prelude.* d') Prelude.> (n' Prelude.* d)

instance AdditiveSemigroup Rational where
  {-# INLINEABLE (+) #-}
  Rational n d + Rational n' d' =
    let newNum = (n * d') + (n' * d)
        newDen = d * d'
        gcd = euclid newNum newDen
     in Rational (newNum `quotient` gcd) (newDen `quotient` gcd)

instance AdditiveMonoid Rational where
  {-# INLINEABLE zero #-}
  zero = Rational zero one

instance AdditiveGroup Rational where
  {-# INLINEABLE (-) #-}
  Rational n d - Rational n' d' =
    let newNum = (n * d') - (n' * d)
        newDen = d * d'
        gcd = euclid newNum newDen
     in Rational (newNum `quotient` gcd) (newDen `quotient` gcd)

instance MultiplicativeSemigroup Rational where
  {-# INLINEABLE (*) #-}
  Rational n d * Rational n' d' =
    let newNum = n * n'
        newDen = d * d'
        gcd = euclid newNum newDen
     in Rational (newNum `quotient` gcd) (newDen `quotient` gcd)

instance MultiplicativeMonoid Rational where
  {-# INLINEABLE one #-}
  one = Rational one one

instance Arbitrary Rational where
  arbitrary = do
    num <- arbitrary
    Positive den <- arbitrary
    Prelude.pure . mkRational num $ den
  shrink r@(Rational num den) = do
    num' <- shrink num
    Positive den' <- shrink . Positive $ den
    let res = mkRational num' den'
    guard (res Prelude.< r)
    Prelude.pure res

instance CoArbitrary Rational where
  coarbitrary (Rational n d) = coarbitrary n . coarbitrary d

instance Function Rational where
  function = functionMap into outOf
    where
      into :: Rational -> (Integer, Integer)
      into (Rational n d) = (n, d)
      outOf :: (Integer, Integer) -> Rational
      outOf (n, d) = Rational n d

{-# INLINEABLE mkRational #-}
mkRational :: Integer -> Integer -> Rational
mkRational n d
  | d == zero = error ()
  | d < zero = mkRational (Plutus.negate n) (Plutus.negate d)
  | otherwise =
    let gcd = euclid n d
     in Rational (n `quotient` gcd) (d `quotient` gcd)

{-# INLINEABLE negate #-}
negate :: Rational -> Rational
negate (Rational n d) = Rational (Plutus.negate n) d

{-# INLINEABLE fromInteger #-}
fromInteger :: Integer -> Rational
fromInteger num = Rational num one

{-# INLINEABLE numerator #-}
numerator :: Rational -> Integer
numerator (Rational n _) = n

{-# INLINEABLE denominator #-}
denominator :: Rational -> Integer
denominator (Rational _ d) = d

{-# INLINEABLE round #-}
round :: Rational -> Integer
round x =
  let (n, r) = properFraction x
      m = if r < zero then n - one else n + one
      flag = abs r - Rational one 2
   in if
          | flag < zero -> n
          | flag == zero -> if even n then n else m
          | otherwise -> m

{-# INLINEABLE truncate #-}
truncate :: Rational -> Integer
truncate (Rational n d) = n `quotient` d

{-# INLINEABLE properFraction #-}
properFraction :: Rational -> (Integer, Rational)
properFraction (Rational n d) = (n `quotient` d, Rational (n `remainder` d) d)

{-# INLINEABLE recip #-}
recip :: Rational -> Rational
recip (Rational n d)
  | n == zero = error ()
  | n < zero = Rational (Plutus.negate d) (Plutus.negate n)
  | otherwise = Rational d n

-- TODO: half, fromGHC, toGHC

{-# INLINEABLE abs #-}
abs :: Rational -> Rational
abs rat@(Rational n d)
  | n < zero = Rational (Plutus.negate n) d
  | otherwise = rat

-- Helpers

-- Euclid's algorithm
{-# INLINEABLE euclid #-}
euclid :: Integer -> Integer -> Integer
euclid x y
  | y == zero = x
  | otherwise = euclid y (x `modulo` y)
