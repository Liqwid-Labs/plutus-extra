{- |
 Module: PlutusTx.Numeric.Gen
 Copyright: (C) MLabs 2021
 License: Apache 2.0
 Maintainer: Koz Ross <koz@mlabs.city>
 Portability: GHC only
 Stability: Experimental

 Support for generating 'Natural' and 'NatRatio' with Hedgehog.
-}
module PlutusTx.Numeric.Gen (
  natural,
  natural_,
  natRatio_,
) where

import Data.Kind (Type)
import Hedgehog (MonadGen, Range)
import Hedgehog.Gen qualified as Gen
import PlutusTx.NatRatio.Internal (NatRatio (NatRatio))
import PlutusTx.Natural.Internal (Natural (Natural))
import PlutusTx.Numeric.Extra (addExtend)
import PlutusTx.Ratio qualified as Ratio

{- | Generates a random 'Natural' in the given @[inclusive, inclusive]@ range.

 When the generator tries to shrink, it will shrink towards the @origin@ of
 the specified 'Range'. For example, the following generator will produce a
 'Natural' between 1970 and 2020, but will shrink toward 2000:

 > natural (Range.constantFrom 2000 1970 2020)

 @since 1.1
-}
natural ::
  forall (m :: Type -> Type).
  (MonadGen m) =>
  Range Natural ->
  m Natural
natural r = do
  let r' = addExtend <$> r
  res <- Gen.integral r'
  pure . Natural $ res

{- | Generates a random 'Natural' in the given @[inclusive, inclusive]@ range.
 Does /not/ shrink.

 @since 1.1
-}
natural_ ::
  forall (m :: Type -> Type).
  (MonadGen m) =>
  Range Natural ->
  m Natural
natural_ r = do
  let r' = addExtend <$> r
  res <- Gen.integral_ r'
  pure . Natural $ res

{- | Generates a random 'NatRatio' in the given @[inclusive, inclusive]@ range.
 Does /not/ shrink.

 @since 1.1
-}
natRatio_ ::
  forall (m :: Type -> Type).
  (MonadGen m) =>
  Range NatRatio ->
  m NatRatio
natRatio_ r = do
  let r' = Ratio.toGHC . addExtend <$> r
  res <- Gen.realFrac_ r'
  pure . NatRatio . Ratio.fromGHC $ res
