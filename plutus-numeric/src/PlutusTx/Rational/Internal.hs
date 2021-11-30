{-# LANGUAGE NoImplicitPrelude #-}

module PlutusTx.Rational.Internal (
  Rational (..),
  negate,
  fromInteger,
  numerator,
  denominator,
  properFraction,
  recip,
  abs,
) where

import PlutusTx.Prelude hiding (Rational, fromInteger, negate)
import PlutusTx.Prelude qualified as Plutus

-- Numerator, denominator
-- Numerator is free, denominator always > 0
data Rational = Rational Integer Integer

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

-- TODO: round, truncate

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
