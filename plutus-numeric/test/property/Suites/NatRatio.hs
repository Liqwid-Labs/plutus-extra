module Suites.NatRatio (tests) where

import Helpers (NonZero (NonZero))
import PlutusTx.NatRatio (NatRatio)
import PlutusTx.NatRatio qualified as NR
import PlutusTx.Natural (Natural)
import PlutusTx.Numeric.Extra (powNat, reciprocal, (/), (^), (^-))
import PlutusTx.Prelude qualified as Plutus
import Test.QuickCheck (Property, forAllShrink, (===))
import Test.QuickCheck.Arbitrary (arbitrary, shrink)
import Test.QuickCheck.Modifiers (Negative (Negative), Positive (Positive))
import Test.Tasty (TestTree, localOption, testGroup)
import Test.Tasty.QuickCheck (QuickCheckTests, testProperty)
import Prelude hiding (divMod, (/), (^))

tests :: [TestTree]
tests =
  [ localOption go . testGroup "MultiplicativeGroup" $
      [ testProperty "if x / y = z, then y * z = x" mgProp1
      , testProperty "x / y = x * reciprocal y" mgProp2
      , testProperty "x ^ 0 = 1" mgProp3
      , testProperty "x ^ 1 = x" mgProp4
      , localOption goSmall
          . testProperty "If i < 0, x ^ i = recip (x ^ |i|)"
          $ mgProp5
      , localOption goSmall
          . testProperty "If i > 1, then x ^ i = x * x ^ (i - 1)"
          $ mgProp6
      ]
  , localOption goSmall . testGroup "Exponentiation" $
      [ testProperty "x `powNat` 0 = 1" expProp1
      , testProperty "x `powNat` 1 = x" expProp2
      , testProperty "If i > 1, then x `powNat` i = x * x `powNat` (i - 1)" expProp3
      ]
  , localOption goSmall . testGroup "Ceiling" $
      [ testProperty "ceiling n >= n" ceilingProp1
      , testProperty "ceiling n ^- truncate n <= 1" ceilingProp2
      ]
  ]
  where
    go :: QuickCheckTests
    go = 1000000
    goSmall :: QuickCheckTests
    goSmall = 100000

expProp1 :: Property
expProp1 = forAllShrink arbitrary shrink go
  where
    go :: NatRatio -> Property
    go x = x `powNat` Plutus.zero === Plutus.one

expProp2 :: Property
expProp2 = forAllShrink arbitrary shrink go
  where
    go :: NatRatio -> Property
    go x = x `powNat` Plutus.one === x

expProp3 :: Property
expProp3 = forAllShrink arbitrary shrink go
  where
    go :: (NatRatio, NonZero Natural) -> Property
    go (x, NonZero i) =
      x `powNat` i === x Plutus.* (x `powNat` (i ^- Plutus.one))

mgProp1 :: Property
mgProp1 = forAllShrink arbitrary shrink go
  where
    go :: (NatRatio, NonZero NatRatio) -> Property
    go (x, NonZero y) =
      let z = x / y
       in x === y Plutus.* z

mgProp2 :: Property
mgProp2 = forAllShrink arbitrary shrink go
  where
    go :: (NatRatio, NonZero NatRatio) -> Property
    go (x, NonZero y) = x / y === x Plutus.* reciprocal y

mgProp3 :: Property
mgProp3 = forAllShrink arbitrary shrink go
  where
    go :: NatRatio -> Property
    go x = x ^ Plutus.zero === Plutus.one

mgProp4 :: Property
mgProp4 = forAllShrink arbitrary shrink go
  where
    go :: NatRatio -> Property
    go x = x ^ Plutus.one === x

mgProp5 :: Property
mgProp5 = forAllShrink arbitrary shrink go
  where
    go :: (NonZero NatRatio, Negative Integer) -> Property
    go (NonZero x, Negative i) =
      x ^ i === reciprocal (x ^ abs i)

mgProp6 :: Property
mgProp6 = forAllShrink arbitrary shrink go
  where
    go :: (NatRatio, Positive Integer) -> Property
    go (x, Positive i) =
      x ^ i === x Plutus.* (x ^ (i - 1))

ceilingProp1 :: NatRatio -> Bool
ceilingProp1 nr = NR.fromNatural (NR.ceiling nr) Plutus.>= nr

ceilingProp2 :: NatRatio -> Bool
ceilingProp2 nr = NR.ceiling nr ^- NR.truncate nr Plutus.<= Plutus.one
