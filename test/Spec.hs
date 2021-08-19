--------------------------------------------------------------------------------

import Prelude

--------------------------------------------------------------------------------

import Test.Tasty (defaultMain, testGroup)

--------------------------------------------------------------------------------

import Suites.PlutusTx.Bimap qualified as Bimap
import Suites.PlutusTx.Set qualified as Set
import Suites.Plutus.PAB.CurrencyForge qualified as CurrencyForge

--------------------------------------------------------------------------------

main :: IO ()
main =
  defaultMain $
    testGroup
      "Suites"
      [ testGroup "PlutusTx.Bimap" Bimap.tests
      , testGroup "PlutusTx.Set" Set.tests
      , testGroup "Plutus.PAB.CurrencyForge" CurrencyForge.tests
      ]
