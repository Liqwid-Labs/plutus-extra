{-# LANGUAGE TemplateHaskell #-}

{- | An on-chain numeric type representing a ratio of non-negative numbers.
 = Warning
 This is an internal module; as such, it exposes the 'NatRatio'
 implementation, which can be used to violate constraints if used without
 care. This module primarily exists to support @testlib@, or for some internal
 APIs; if at all possible, use 'PlutusTx.NatRatio' instead.
-}
module PlutusTx.NatRatio.Internal (
  NatRatio (..),
  natRatio,
  fromNatural,
  numerator,
  denominator,
  truncate,
  ceiling,
  PlutusTx.NatRatio.Internal.round,
  properFraction,
  recip,
  toRational,
) where

import Control.Monad (guard)
import Data.Aeson (FromJSON (parseJSON), ToJSON)
import PlutusTx.IsData.Class (
  FromData (fromBuiltinData),
  ToData,
  UnsafeFromData (unsafeFromBuiltinData),
 )
import PlutusTx.Lift (makeLift)
import PlutusTx.Natural.Internal (Natural (Natural))
import PlutusTx.Prelude
import PlutusTx.Ratio qualified as Ratio
import Test.QuickCheck.Arbitrary (Arbitrary (arbitrary, shrink))
import Test.QuickCheck.Gen (suchThat)
import Prelude qualified

{- | A ratio of 'Natural's. Similar to 'Rational', but with the numerator and
 denominator guaranteed non-negative.

 @since 1.0
-}
newtype NatRatio = NatRatio Rational
  deriving stock
    ( -- | @since 1.0
      Prelude.Show
    )
  deriving
    ( -- | @since 1.0
      Prelude.Eq
    , -- | @since 1.0
      Eq
    , -- | @since 1.0
      Ord
    , -- | @since 1.0
      AdditiveSemigroup
    , -- | @since 1.0
      AdditiveMonoid
    , -- | @since 1.0
      MultiplicativeSemigroup
    , -- | @since 1.0
      MultiplicativeMonoid
    , -- | @since 1.0
      ToData
    , -- | @since 1.0
      ToJSON
    )
    via Rational

-- | @since 1.0
instance FromJSON NatRatio where
  parseJSON v = do
    r <- parseJSON v
    guard (r >= zero)
    Prelude.pure . NatRatio $ r

-- | @since 1.0
instance FromData NatRatio where
  {-# INLINEABLE fromBuiltinData #-}
  fromBuiltinData dat = case fromBuiltinData dat of
    Nothing -> Nothing
    Just r ->
      if Ratio.abs r == r
        then Just (NatRatio r)
        else Nothing

-- | @since 1.0
instance UnsafeFromData NatRatio where
  {-# INLINEABLE unsafeFromBuiltinData #-}
  unsafeFromBuiltinData dat =
    let r = unsafeFromBuiltinData dat
     in if Ratio.abs r == r
          then NatRatio r
          else error . trace "Negative fractions cannot be NatRatio" $ ()

-- | @since 1.0
instance Arbitrary NatRatio where
  arbitrary = do
    Natural num <- arbitrary
    Natural den <- suchThat arbitrary (> zero)
    Prelude.pure . NatRatio $ num Ratio.% den
  shrink nr = do
    let Natural num = numerator nr
    let Natural den = denominator nr
    num' <- Prelude.filter (> 0) . shrink $ num
    den' <- Prelude.filter (> 0) . shrink $ den
    Prelude.pure . NatRatio $ num' Ratio.% den'

{- | Safely construct a 'NatRatio'. Checks for a zero denominator.

 @since 1.0
-}
{-# INLINEABLE natRatio #-}
natRatio :: Natural -> Natural -> Maybe NatRatio
natRatio (Natural n) (Natural m) =
  if m == 0
    then Nothing
    else Just . NatRatio $ n Ratio.% m

{- | Convert a 'Natural' into a 'NatRatio' with the same value.

 @since 1.0
-}
{-# INLINEABLE fromNatural #-}
fromNatural :: Natural -> NatRatio
fromNatural (Natural n) = NatRatio $ n % 1

{- | Retrieve the numerator of a 'NatRatio'.

 @since 1.0
-}
{-# INLINEABLE numerator #-}
numerator :: NatRatio -> Natural
numerator (NatRatio r) = Natural . Ratio.numerator $ r

{- | Retrieve the denominator of a 'NatRatio'. This is guaranteed non-zero,
 though the result type doesn't specify this.

 @since 1.0
-}
{-# INLINEABLE denominator #-}
denominator :: NatRatio -> Natural
denominator (NatRatio r) = Natural . Ratio.denominator $ r

{- | Round the 'NatRatio' down.

 @since 1.0
-}
{-# INLINEABLE truncate #-}
truncate :: NatRatio -> Natural
truncate (NatRatio r) = Natural . Ratio.truncate $ r

{- | Round the 'NatRatio' arithmetically.

 @since 1.0
-}
{-# INLINEABLE round #-}
round :: NatRatio -> Natural
round (NatRatio r) = Natural . Ratio.round $ r

{- | Take the reciprocal of the 'NatRatio'.

 @since 1.0
-}
{-# INLINEABLE recip #-}
recip :: NatRatio -> NatRatio
recip (NatRatio r) = NatRatio . Ratio.recip $ r

{- | Round the 'NatRatio' up.

 @since 1.0
-}
{-# INLINEABLE ceiling #-}
ceiling :: NatRatio -> Natural
ceiling x =
  case properFraction x of
    (floor, leftover)
      | leftover == zero -> floor
      | otherwise -> succ floor

{- | Separate a 'NatRatio' into a whole and a fractional part, such that the
 fractional part is guaranteed to be less than @1@.

 @since 1.0
-}
{-# INLINEABLE properFraction #-}
properFraction :: NatRatio -> (Natural, NatRatio)
properFraction (NatRatio r) =
  let (n, r') = Ratio.properFraction r
   in (Natural n, NatRatio r')

{- | Convert a 'NatRatio' to the underlying 'Rational'.

 @since 1.0
-}
toRational :: NatRatio -> Rational
toRational (NatRatio r) = r

makeLift ''NatRatio
