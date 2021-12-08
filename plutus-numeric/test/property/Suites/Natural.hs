{-# LANGUAGE QuasiQuotes #-}

module Suites.Natural (tests) where

import PlutusTx.Natural (
  Natural,
  Parity (Even, Odd),
  nat,
  parity,
 )
import PlutusTx.Numeric.Extra (divMod, powNat, rem, (^-))
import PlutusTx.Prelude qualified as Plutus
import Test.QuickCheck (
  Property,
  forAllShrink,
  (.&&.),
  (.||.),
  (=/=),
  (===),
 )
import Test.QuickCheck.Arbitrary (arbitrary, shrink)
import Test.Tasty (TestTree, localOption, testGroup)
import Test.Tasty.QuickCheck (QuickCheckTests, testProperty)
import Prelude hiding (divMod, product, rem)

tests :: [TestTree]
tests =
  [ localOption go . testGroup "EuclideanClosed" $
      [ testProperty "if divMod x y = (d, r), then (d * y) + r = x" ecProp1
      , testProperty "divMod x 0 = (0, x)" ecProp2
      , testProperty "if divMod x y = (d, r) and y /= 0, then 0 <= |r| < |y|" ecProp3
      ]
  , localOption go . testProperty "Parity" $ parityProp
  , localOption go . testGroup "Exponentiation" $
      [ testProperty "x `powNat` 0 = mempty" expProp1
      , testProperty "x `powNat` 1 = x" expProp2
      , testProperty "x `powNat` n = fold . repeat n $ x" expProp3
      ]
  ]
  where
    go :: QuickCheckTests
    go = 1000000

expProp1 :: Property
expProp1 = forAllShrink arbitrary shrink go
  where
    go :: Natural -> Property
    go x = x `powNat` Plutus.zero === Plutus.one

expProp2 :: Property
expProp2 = forAllShrink arbitrary shrink go
  where
    go :: Natural -> Property
    go x = x `powNat` Plutus.one === x

expProp3 :: Property
expProp3 = forAllShrink arbitrary shrink go
  where
    go :: (Natural, Natural) -> Property
    go (x, n) = x `powNat` n === (product . clone n $ x)
    clone :: Natural -> Natural -> [Natural]
    clone n x =
      if n == Plutus.zero
        then []
        else x : clone (n ^- Plutus.one) x
    product :: [Natural] -> Natural
    product = \case
      [] -> Plutus.one
      (n : ns) -> n Plutus.* product ns

ecProp1 :: Property
ecProp1 = forAllShrink arbitrary shrink go
  where
    go :: (Natural, Natural) -> Property
    go (x, y) =
      let (d, r) = divMod x y
       in (d Plutus.* y) Plutus.+ r === x

ecProp2 :: Property
ecProp2 = forAllShrink arbitrary shrink go
  where
    go :: Natural -> Property
    go x = divMod x Plutus.zero === (Plutus.zero, x)

ecProp3 :: Property
ecProp3 = forAllShrink arbitrary shrink go
  where
    go :: (Natural, Natural) -> Property
    go (x, y) =
      let (_, r) = divMod x y
       in (y === Plutus.zero)
            .||. ( (Plutus.compare Plutus.zero r =/= GT)
                    .&&. (Plutus.compare r y === LT)
                 )

parityProp :: Property
parityProp = forAllShrink arbitrary shrink go
  where
    go :: Natural -> Property
    go x = case x `rem` [nat| 2 |] of
      [nat| 0 |] -> parity x === Even
      _ -> parity x === Odd
