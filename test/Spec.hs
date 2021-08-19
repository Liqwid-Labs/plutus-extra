--------------------------------------------------------------------------------

import Prelude

--------------------------------------------------------------------------------

import Test.Tasty (defaultMain, testGroup)

--------------------------------------------------------------------------------

import Suites.PlutusTx.Bimap qualified as Bimap
import Suites.PlutusTx.Set qualified as Set

--------------------------------------------------------------------------------

main :: IO ()
main =
  defaultMain $
    testGroup
      "Suites"
      [ testGroup "Bimap" Bimap.tests
      , testGroup "Set" Set.tests
      ]
