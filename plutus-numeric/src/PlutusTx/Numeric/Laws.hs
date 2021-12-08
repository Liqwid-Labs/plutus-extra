module PlutusTx.Numeric.Laws (
  additiveHemigroupLaws,
) where

import Data.Kind (Type)
import PlutusTx.Numeric.Extra (AdditiveHemigroup ((^-)))
import PlutusTx.Prelude qualified as PTx
import Test.QuickCheck (
  Property,
  forAllShrinkShow,
  (===),
 )
import Test.QuickCheck.Arbitrary (liftArbitrary, liftShrink)
import Test.QuickCheck.Gen (Gen)
import Test.Tasty.Plutus.Arbitrary (Pair (Pair), Triple (Triple))
import Test.Tasty.Plutus.Laws (Laws, law)
import Text.Show.Pretty (ppShow)

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
