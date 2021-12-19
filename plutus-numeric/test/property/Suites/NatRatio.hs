module Suites.NatRatio (tests) where

import Helpers (NonZero (NonZero))
import PlutusTx.NatRatio (NatRatio)
import PlutusTx.NatRatio qualified as NR
import PlutusTx.Natural (Natural)
import PlutusTx.Numeric.Extra (powNat, (^-))
import PlutusTx.Prelude qualified as Plutus
import Test.QuickCheck (Property, forAllShrink, (===))
import Test.QuickCheck.Arbitrary (arbitrary, shrink)
import Test.Tasty (TestTree, localOption, testGroup)
import Test.Tasty.QuickCheck (QuickCheckTests, testProperty)
import Prelude hiding (divMod, (/), (^))

tests :: [TestTree]
tests =
  [ localOption goSmall . testGroup "Exponentiation" $
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

ceilingProp1 :: NatRatio -> Bool
ceilingProp1 nr = NR.fromNatural (NR.ceiling nr) Plutus.>= nr

ceilingProp2 :: NatRatio -> Bool
ceilingProp2 nr = NR.ceiling nr ^- NR.truncate nr Plutus.<= Plutus.one
