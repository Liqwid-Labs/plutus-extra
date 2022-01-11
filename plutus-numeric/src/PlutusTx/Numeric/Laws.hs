module PlutusTx.Numeric.Laws (
  additiveHemigroupLaws,
  euclideanClosedLaws,
  euclideanClosedSignedLaws,
  multiplicativeGroupLaws,
  integralDomainLaws,
  scaleNatLaws,
) where

import Data.Kind (Type)
import PlutusTx.Natural (Natural)
import PlutusTx.Numeric.Extra (
  AdditiveHemigroup ((^-)),
  EuclideanClosed (divMod),
  IntegralDomain (abs, addExtend, projectAbs, restrictMay, signum),
  MultiplicativeGroup (powInteger, reciprocal, (/)),
  scaleNat,
 )
import PlutusTx.Prelude qualified as PTx
import Test.QuickCheck (
  Property,
  discard,
  forAllShrinkShow,
  property,
  (.&&.),
  (=/=),
  (===),
 )
import Test.QuickCheck.Arbitrary (arbitrary, liftArbitrary, liftShrink, shrink)
import Test.QuickCheck.Gen (Gen)
import Test.QuickCheck.Modifiers (
  Negative (Negative),
  Positive (Positive),
 )
import Test.Tasty.Plutus.Arbitrary (Pair (Pair), Triple (Triple))
import Test.Tasty.Plutus.Laws (Laws, law)
import Text.Show.Pretty (ppShow)
import Prelude hiding (abs, divMod, signum, (/))

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

{- | The 'PTx.AdditiveMonoid' constraint is required to ensure non-zeroes in the
 tests, as the laws assume this. All instances of @y@ denote a non-zero value.

 @since 4.0
-}
multiplicativeGroupLaws ::
  forall (a :: Type).
  (Show a, MultiplicativeGroup a, Eq a, PTx.AdditiveMonoid a) =>
  Laws a
multiplicativeGroupLaws =
  law "if x / y = z, then y * z = x" mgInversion
    <> law "x / y = x * reciprocal y" mgDivRecip
    <> law "powInteger x 0 = 1" mgPowZero
    <> law "powInteger x 1 = x" mgPowOne
    <> law
      ( "if i < 0, "
          <> "then powInteger y i = reciprocal . powInteger y . abs $ i"
      )
      mgPowNeg
    <> law "if i > 0, then powInteger x i = x * powInteger x (i - 1)" mgPowPos
  where
    mgInversion :: Gen a -> (a -> [a]) -> Property
    mgInversion gen shr =
      forAllShrinkShow (liftArbitrary gen) (liftShrink shr) ppShow $
        \(Pair x y) ->
          if y == PTx.zero
            then discard
            else
              let z = x / y
               in y PTx.* z === x
    mgDivRecip :: Gen a -> (a -> [a]) -> Property
    mgDivRecip gen shr =
      forAllShrinkShow (liftArbitrary gen) (liftShrink shr) ppShow $
        \(Pair x y) ->
          if y == PTx.zero
            then discard
            else (x / y) === (x PTx.* reciprocal y)
    mgPowZero :: Gen a -> (a -> [a]) -> Property
    mgPowZero gen shr = forAllShrinkShow gen shr ppShow $
      \x -> powInteger x PTx.zero === PTx.one
    mgPowOne :: Gen a -> (a -> [a]) -> Property
    mgPowOne gen shr = forAllShrinkShow gen shr ppShow $
      \x -> powInteger x PTx.one === x
    mgPowNeg :: Gen a -> (a -> [a]) -> Property
    mgPowNeg gen shr =
      forAllShrinkShow (liftArbitrary gen) (liftShrink shr) ppShow $
        \(Negative i, x) ->
          if x == PTx.zero
            then discard
            else powInteger x i === (reciprocal . powInteger x . abs $ i)
    mgPowPos :: Gen a -> (a -> [a]) -> Property
    mgPowPos gen shr =
      forAllShrinkShow (liftArbitrary gen) (liftShrink shr) ppShow $
        \(Positive i, x) -> powInteger x i === x PTx.* powInteger x (i - 1)

-- | @since 4.0
integralDomainLaws ::
  forall (a :: Type) (b :: Type).
  (Show a, Eq a, IntegralDomain a b) =>
  Laws a
integralDomainLaws =
  law "abs x >= 0" absGEZero
    <> law "x <= abs x" absGESelf
    <> law "abs (x * y) = abs x * abs y" absMulDist
    <> law "abs x * signum x = x" absSignumAgreement
    <> law "addExtend . projectAbs $ x = abs x" projectExtend
    <> law "restrictMay x = Just y if and only if abs x = x" restrictMayAbs
  where
    absGEZero :: Gen a -> (a -> [a]) -> Property
    absGEZero gen shr = forAllShrinkShow gen shr ppShow $
      \x -> property (abs x PTx.>= PTx.zero)
    absMulDist :: Gen a -> (a -> [a]) -> Property
    absMulDist gen shr =
      forAllShrinkShow (liftArbitrary gen) (liftShrink shr) ppShow $
        \(Pair x y) ->
          let lhs = abs (x PTx.* y)
              rhs = abs x PTx.* abs y
           in lhs === rhs
    absGESelf :: Gen a -> (a -> [a]) -> Property
    absGESelf gen shr = forAllShrinkShow gen shr ppShow $
      \x -> property (x PTx.<= abs x)
    absSignumAgreement :: Gen a -> (a -> [a]) -> Property
    absSignumAgreement gen shr = forAllShrinkShow gen shr ppShow $
      \x -> abs x PTx.* signum x === x
    projectExtend :: Gen a -> (a -> [a]) -> Property
    projectExtend gen shr = forAllShrinkShow gen shr ppShow $
      \x ->
        let lhs = addExtend . projectAbs $ x
            rhs = abs x
         in lhs === rhs
    restrictMayAbs :: Gen a -> (a -> [a]) -> Property
    restrictMayAbs gen shr = forAllShrinkShow gen shr ppShow $
      \x -> case restrictMay x of
        Nothing -> abs x =/= x
        Just _ -> abs x === x

-- | @since 4.2
scaleNatLaws ::
  forall (a :: Type).
  (Show a, Eq a, PTx.AdditiveMonoid a) =>
  Laws a
scaleNatLaws =
  law "scaleNat n (r1 + r2) = scaleNat n r1 + scaleNat n r2" scaleNatDist
    <> law "scaleNat n1 (scaleNat n2 r) = scaleNat (n1 * n2) r" scaleNatComp
    <> law "scaleNat one r = r" scaleNatOne
    <> law "scaleNat zero r = zero" scaleNatZero
  where
    scaleNatDist :: Gen a -> (a -> [a]) -> Property
    scaleNatDist gen shr =
      forAllShrinkShow gen' shr' ppShow $
        \(n, r1, r2) -> scaleNat n (r1 PTx.+ r2) === scaleNat n r1 PTx.+ scaleNat n r2
      where
        gen' :: Gen (Natural, a, a)
        gen' = (,,) <$> arbitrary <*> gen <*> gen
        shr' :: (Natural, a, a) -> [(Natural, a, a)]
        shr' (n, a1, a2) = (,,) <$> shrink n <*> shr a1 <*> shr a2

    scaleNatComp :: Gen a -> (a -> [a]) -> Property
    scaleNatComp gen shr =
      forAllShrinkShow gen' shr' ppShow $
        \(n1, n2, r) -> scaleNat n1 (scaleNat n2 r) === scaleNat (n1 PTx.* n2) r
      where
        gen' :: Gen (Natural, Natural, a)
        gen' = (,,) <$> arbitrary <*> arbitrary <*> gen
        shr' :: (Natural, Natural, a) -> [(Natural, Natural, a)]
        shr' (n1, n2, a) = (,,) <$> shrink n1 <*> shrink n2 <*> shr a

    scaleNatOne :: Gen a -> (a -> [a]) -> Property
    scaleNatOne gen shr =
      forAllShrinkShow gen shr ppShow $
        \r -> scaleNat PTx.one r === r

    scaleNatZero :: Gen a -> (a -> [a]) -> Property
    scaleNatZero gen shr =
      forAllShrinkShow gen shr ppShow $
        \r -> scaleNat PTx.zero r === PTx.zero

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
