{-# LANGUAGE NoImplicitPrelude #-}

module PlutusTx.Rational.Internal (
  Rational (..),
  (%),
  ratio,
  toGHC,
) where

import Control.Monad (guard)
import Data.Aeson (
  FromJSON (parseJSON),
  ToJSON (toJSON),
  object,
  withObject,
  (.:),
 )
import Data.Ratio qualified as GHC
import PlutusTx.IsData.Class (
  FromData (fromBuiltinData),
  ToData (toBuiltinData),
  UnsafeFromData (unsafeFromBuiltinData),
 )
import PlutusTx.Prelude hiding (
  Rational,
  fromInteger,
  negate,
  round,
  (%),
 )
import PlutusTx.Prelude qualified as Plutus
import Test.QuickCheck.Arbitrary (
  Arbitrary (arbitrary, shrink),
  CoArbitrary (coarbitrary),
 )
import Test.QuickCheck.Function (Function (function), functionMap)
import Test.QuickCheck.Modifiers (Positive (Positive))
import Text.Show.Pretty (PrettyVal (prettyVal))
import Prelude qualified

{- | A data type representing an arbitrary-precision ratio.

 @since 4.0
-}
data Rational = Rational Integer Integer
  deriving stock
    ( -- | @since 4.0
      Prelude.Eq
    , -- | @since 4.0
      Prelude.Show
    )

-- | @since 4.0
instance Eq Rational where
  {-# INLINEABLE (==) #-}
  Rational n d == Rational n' d' = n == n' && d == d'

-- | @since 4.0
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

-- | @since 4.0
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

-- | @since 4.0
instance AdditiveSemigroup Rational where
  {-# INLINEABLE (+) #-}
  Rational n d + Rational n' d' =
    let newNum = (n * d') + (n' * d)
        newDen = d * d'
        gcd = euclid newNum newDen
     in Rational (newNum `quotient` gcd) (newDen `quotient` gcd)

-- | @since 4.0
instance AdditiveMonoid Rational where
  {-# INLINEABLE zero #-}
  zero = Rational zero one

-- | @since 4.0
instance AdditiveGroup Rational where
  {-# INLINEABLE (-) #-}
  Rational n d - Rational n' d' =
    let newNum = (n * d') - (n' * d)
        newDen = d * d'
        gcd = euclid newNum newDen
     in Rational (newNum `quotient` gcd) (newDen `quotient` gcd)

-- | @since 4.0
instance MultiplicativeSemigroup Rational where
  {-# INLINEABLE (*) #-}
  Rational n d * Rational n' d' =
    let newNum = n * n'
        newDen = d * d'
        gcd = euclid newNum newDen
     in Rational (newNum `quotient` gcd) (newDen `quotient` gcd)

-- | @since 4.0
instance MultiplicativeMonoid Rational where
  {-# INLINEABLE one #-}
  one = Rational one one

-- | @since 4.0
instance Arbitrary Rational where
  arbitrary = do
    num <- arbitrary
    Positive den <- arbitrary
    Prelude.pure $ num % den
  shrink r@(Rational num den) = do
    num' <- shrink num
    Positive den' <- shrink . Positive $ den
    let res = num' % den'
    guard (res Prelude.< r)
    Prelude.pure res

-- | @since 4.0
instance CoArbitrary Rational where
  coarbitrary (Rational n d) = coarbitrary n . coarbitrary d

-- | @since 4.0
instance Function Rational where
  function = functionMap into outOf
    where
      into :: Rational -> (Integer, Integer)
      into (Rational n d) = (n, d)
      outOf :: (Integer, Integer) -> Rational
      outOf (n, d) = Rational n d

-- | @since 4.0
instance ToData Rational where
  {-# INLINEABLE toBuiltinData #-}
  toBuiltinData (Rational n d) = toBuiltinData (n, d)

-- | @since 4.0
instance FromData Rational where
  {-# INLINEABLE fromBuiltinData #-}
  fromBuiltinData dat = case fromBuiltinData dat of
    Nothing -> Nothing
    Just (n, d) -> if d == zero then error () else Just (n % d)

-- | @since 4.0
instance UnsafeFromData Rational where
  {-# INLINEABLE unsafeFromBuiltinData #-}
  unsafeFromBuiltinData dat = case unsafeFromBuiltinData dat of
    (n, d) -> if d == zero then error () else n % d

-- | @since 4.0
instance ToJSON Rational where
  toJSON (Rational n d) =
    object
      [ ("numerator", toJSON n)
      , ("denominator", toJSON d)
      ]

-- | @since 4.0
instance FromJSON Rational where
  parseJSON = withObject "Rational" $ \obj -> do
    n <- obj .: "numerator"
    d <- obj .: "denominator"
    if d == 0
      then Prelude.fail "Zero denominator is invalid."
      else Prelude.pure (n % d)

-- | @since 4.0
instance PrettyVal Rational where
  prettyVal = prettyVal . toGHC

-- TODO: ToArgument, ToSchema (both flavours), PrettyVal

{-# INLINEABLE (%) #-}
(%) :: Integer -> Integer -> Rational
n % d
  | d == zero = error ()
  | d < zero = Plutus.negate n % Plutus.negate d
  | otherwise =
    let gcd = euclid n d
     in Rational (n `quotient` gcd) (d `quotient` gcd)

infixl 7 %

{- | Safely constructs a 'Rational' from a numerator and denominator. Returns
 'Nothing' if given a zero denominator.

 @since 4.0
-}
{-# INLINEABLE ratio #-}
ratio :: Integer -> Integer -> Maybe Rational
ratio n d
  | d == zero = Nothing
  | d < zero = Just (Plutus.negate n % Plutus.negate d)
  | otherwise =
    let gcd = euclid n d
     in Just . Rational (n `quotient` gcd) $ d `quotient` gcd

{- | Converts a 'Rational' to a GHC 'GHC.Rational', preserving value. Does not
 work on-chain.

 @since 4.0
-}
toGHC :: Rational -> GHC.Rational
toGHC (Rational n d) = n GHC.% d

-- Helpers

-- Euclid's algorithm
{-# INLINEABLE euclid #-}
euclid :: Integer -> Integer -> Integer
euclid x y
  | y == zero = x
  | otherwise = euclid y (x `modulo` y)
