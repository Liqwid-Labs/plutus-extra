--------------------------------------------------------------------------------

import Prelude

--------------------------------------------------------------------------------

import Test.Tasty (defaultMain, testGroup)

--------------------------------------------------------------------------------

import Suites.Bimap qualified as Bimap
import Suites.Set qualified as Set

--------------------------------------------------------------------------------

main :: IO ()
main =
  defaultMain $
    testGroup
      "Suites"
      [ testGroup "Bimap" Bimap.tests
      , testGroup "Set" Set.tests
      ]
