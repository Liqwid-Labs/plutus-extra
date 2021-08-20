--------------------------------------------------------------------------------

import Prelude

--------------------------------------------------------------------------------

import Test.Tasty (defaultMain, testGroup)

--------------------------------------------------------------------------------

import Suites.Plutus.PAB.CurrencyForge qualified as CurrencyForge
import Suites.PlutusTx.Bimap qualified as Bimap
import Suites.PlutusTx.Data.Extra qualified as DataExtra
import Suites.PlutusTx.Set qualified as Set
import Suites.PlutusTx.NonEmpty qualified as NonEmpty

--------------------------------------------------------------------------------

main :: IO ()
main =
  defaultMain $
    testGroup
      "Suites"
      [ testGroup "PlutusTx.Bimap" Bimap.tests
      , testGroup "PlutusTx.Set" Set.tests
      , testGroup "PlutusTx.NonEmpty" NonEmpty.tests
      , testGroup "PlutusTx.Data.Extra" DataExtra.tests
      , testGroup "Plutus.PAB.CurrencyForge" CurrencyForge.tests
      ]
