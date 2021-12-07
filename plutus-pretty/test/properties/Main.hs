module Main (main) where

import Test.QuickCheck.Plutus.Instances ()
import PlutusTx.Prelude (BuiltinString)
import PlutusTx.Prelude qualified as PTx
import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.QuickCheck (testProperty)
import Test.QuickCheck (Property, forAllShrinkShow)
import Test.QuickCheck.Arbitrary (arbitrary, shrink)
import Text.Show.Pretty (ppShow)

main :: IO ()
main = defaultMain . testGroup "Skeletal" $ [
  testProperty ("showSkeletal s <> showSkeletal s' = " <> 
                "showSkeletal (s <> s')") stringSkeletalProp
  ]

-- Helpers

stringSkeletalProp :: Property
stringSkeletalProp = forAllShrinkShow arbitrary shrink ppShow go
  where
    go :: (BuiltinString, BuiltinString) -> Property
    go (s, s') = _
