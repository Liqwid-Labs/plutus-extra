module PlutusTx.Numeric.Laws (
  additiveHemigroupLaws,
  euclideanClosedLaws,
  euclideanClosedSignedLaws,
) where

import Data.Kind (Type)
import PlutusTx.Numeric.Extra (
  AdditiveHemigroup ((^-)),
  EuclideanClosed (divMod),
  IntegralDomain (abs),
 )
import PlutusTx.Prelude qualified as PTx
import Test.QuickCheck (
  Property,
  discard,
  forAllShrinkShow,
  property,
  (.&&.),
  (===),
 )
import Test.QuickCheck.Arbitrary (liftArbitrary, liftShrink)
import Test.QuickCheck.Gen (Gen)
import Test.Tasty.Plutus.Arbitrary (Pair (Pair), Triple (Triple))
import Test.Tasty.Plutus.Laws (Laws, law)
import Text.Show.Pretty (ppShow)
import Prelude hiding (abs, divMod)

-- | @since 4.0
additiveHemigroupLaws ::
  forall (a :: Type).
  (AdditiveHemigroup a, Show a, Eq a) =>
  Laws a
additiveHemigroupLaws =
  law "x + (y ^- x) = y + (x ^- y)" monusInterchange
    <> law "(x ^- y) ^- z = x ^- (y + z)" monusAbsorb
    <> law "x ^- x = 0" selfAnnihilation
    <> law "0 ^- x = 0" zeroClip
  where
    monusInterchange :: Gen a -> (a -> [a]) -> Property
    monusInterchange gen shr =
      forAllShrinkShow (liftArbitrary gen) (liftShrink shr) ppShow $
        \(Pair x y) ->
          let lhs = x PTx.+ (y ^- x)
              rhs = y PTx.+ (x ^- y)
           in lhs === rhs
    monusAbsorb :: Gen a -> (a -> [a]) -> Property
    monusAbsorb gen shr =
      forAllShrinkShow (liftArbitrary gen) (liftShrink shr) ppShow $
        \(Triple x y z) ->
          let lhs = (x ^- y) ^- z
              rhs = x ^- (y PTx.+ z)
           in lhs === rhs
    selfAnnihilation :: Gen a -> (a -> [a]) -> Property
    selfAnnihilation gen shr = forAllShrinkShow gen shr ppShow $
      \x -> x ^- x === PTx.zero
    zeroClip :: Gen a -> (a -> [a]) -> Property
    zeroClip gen shr = forAllShrinkShow gen shr ppShow $
      \x -> PTx.zero ^- x === PTx.zero

{- | This is for types /without/ an additive inverse. If such a thing is
 possible, use 'euclideanClosedSignedLaws' instead.

 @since 4.0
-}
euclideanClosedLaws ::
  forall (a :: Type).
  (Show a, EuclideanClosed a, Eq a) =>
  Laws a
euclideanClosedLaws =
  ecInversionLaw
    <> ecZeroDivLaw
    <> law
      ( "if x `divMod` y = (d, r) and y /= 0, "
          <> "then 0 <= r < y"
      )
      ecNonZeroDiv
  where
    ecNonZeroDiv :: Gen a -> (a -> [a]) -> Property
    ecNonZeroDiv gen shr =
      forAllShrinkShow (liftArbitrary gen) (liftShrink shr) ppShow $
        \(Pair x y) ->
          if y PTx.== PTx.zero
            then discard
            else
              let (_, r) = x `divMod` y
               in property (PTx.zero PTx.<= r) .&&. property (r PTx.< y)

{- | This is for types /with/ an additive inverse, hence the additional
 constraints. If your type lacks additive inverses, use 'euclideanClosedLaws'
 instead.

 @since 4.0
-}
euclideanClosedSignedLaws ::
  forall (a :: Type) (b :: Type).
  (Show a, EuclideanClosed a, Eq a, IntegralDomain a b) =>
  Laws a
euclideanClosedSignedLaws =
  ecInversionLaw
    <> ecZeroDivLaw
    <> law
      ( "if x `divMod` y = (d, r) and y /= 0, "
          <> "then 0 <= abs r < abs y"
      )
      ecNonZeroDiv
  where
    ecNonZeroDiv :: Gen a -> (a -> [a]) -> Property
    ecNonZeroDiv gen shr =
      forAllShrinkShow (liftArbitrary gen) (liftShrink shr) ppShow $
        \(Pair x y) ->
          if y PTx.== PTx.zero
            then discard
            else
              let (_, r) = x `divMod` y
                  r' = abs r
               in property (PTx.zero PTx.<= r') .&&. property (r' PTx.< abs y)

-- Helpers

ecInversionLaw ::
  forall (a :: Type).
  (Show a, EuclideanClosed a, Eq a) =>
  Laws a
ecInversionLaw = law "if x `divMod` y = (d, r), then (d * y) + r = x" go
  where
    go :: Gen a -> (a -> [a]) -> Property
    go gen shr = forAllShrinkShow (liftArbitrary gen) (liftShrink shr) ppShow $
      \(Pair x y) ->
        let (d, r) = x `divMod` y
         in (d PTx.* y) PTx.+ r === x

ecZeroDivLaw ::
  forall (a :: Type).
  (Show a, Eq a, EuclideanClosed a) =>
  Laws a
ecZeroDivLaw = law "x `divMod` 0 = (0, x)" go
  where
    go :: Gen a -> (a -> [a]) -> Property
    go gen shr = forAllShrinkShow gen shr ppShow $
      \x -> x `divMod` PTx.zero === (PTx.zero, x)
