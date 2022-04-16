module Main (main) where

import ScaleNat (benchScaleNat)
import Test.Tasty.Bench (defaultMain)

main :: IO ()
main = defaultMain [benchScaleNat]
