module Suites.Consistency (tests) where

import PlutusTx.NatRatio (NatRatio, natRatioToRational)
import PlutusTx.Natural (Natural, natToInteger)
import PlutusTx.Numeric.Extra (addExtend)
import Test.QuickCheck (Property, forAllShrink, (===))
import Test.QuickCheck.Arbitrary (arbitrary, shrink)
import Test.Tasty (TestTree, adjustOption, testGroup)
import Test.Tasty.QuickCheck (QuickCheckTests, testProperty)

tests :: [TestTree]
tests =
  [ adjustOption go . testGroup "Wrapped operations must be consistent" $
      [ testProperty "natToInteger must be consistent with addExtend" natToIntegerProp
      , testProperty "natRatioToRatio must be consistent with addExtend" natRatioToRationalProp
      ]
  ]
  where
    go :: QuickCheckTests -> QuickCheckTests
    go = max 100000

-- Helpers

natToIntegerProp :: Property
natToIntegerProp = forAllShrink arbitrary shrink go
  where
    go :: Natural -> Property
    go n = addExtend n === natToInteger n

natRatioToRationalProp :: Property
natRatioToRationalProp = forAllShrink arbitrary shrink go
  where
    go :: NatRatio -> Property
    go nr = addExtend nr === natRatioToRational nr
