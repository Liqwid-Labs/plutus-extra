{-# LANGUAGE TemplateHaskell #-}

module Main (main) where

import GHC.Generics qualified as GHC
import PlutusTx.IsCoexistingData (makeCoexistingIsData)
import PlutusTx.Data qualified as Data
import PlutusTx.IsData.Class (IsData (fromData, toData))
import PlutusTx.Prelude
import Test.QuickCheck (Property, forAllShrink, (===))
import Test.QuickCheck.Arbitrary (Arbitrary (arbitrary, shrink))
import Test.QuickCheck.Gen qualified as Gen
import Test.QuickCheck.Instances.ByteString ()
import Test.Tasty (TestTree, defaultMain, localOption, testGroup)
import Test.Tasty.HUnit (Assertion, assertEqual, testCase)
import Test.Tasty.QuickCheck (QuickCheckTests (QuickCheckTests), testProperty)
import Prelude qualified

main :: Prelude.IO ()
main = defaultMain allTests
  where
    allTests :: TestTree
    allTests =
      testGroup
        "IsData generators"
        [ basicStructure
        , roundTripping
        ]

-- Helpers

data Foo = Bar | Baz {-# UNPACK #-} !ByteString | Quux Integer Integer
  deriving stock (GHC.Generic, Prelude.Show, Prelude.Eq)

instance Arbitrary Foo where
  arbitrary =
    Gen.oneof
      [ Prelude.pure Bar
      , Baz Prelude.<$> arbitrary
      , Quux Prelude.<$> arbitrary Prelude.<*> arbitrary
      ]
  shrink = \case
    Bar -> []
    Baz bs -> (Bar :) $ Baz Prelude.<$> shrink bs
    Quux i j -> (Bar :) $ Quux Prelude.<$> shrink i Prelude.<*> shrink j

basicStructure :: TestTree
basicStructure =
  testGroup
    "Structure of output"
    [ testCase "Representation, no arguments" barStructure
    , testCase "Representation, one argument" bazStructure
    , testCase "Representation, two arguments" quuxStructure
    ]
  where
    barStructure :: Assertion
    barStructure =
      assertEqual
        ""
        (Data.Constr 0 [Data.I 8029713825859723597])
        (toData Bar)
    bazStructure :: Assertion
    bazStructure =
      assertEqual
        ""
        (Data.Constr 1 [Data.I 8029713825859723597, Data.B "xyz"])
        (toData . Baz $ "xyz")
    quuxStructure :: Assertion
    quuxStructure =
      assertEqual
        ""
        (Data.Constr 2 [Data.I 8029713825859723597, Data.I 1, Data.I 2])
        (toData . Quux 1 $ 2)

roundTripping :: TestTree
roundTripping =
  localOption (QuickCheckTests 100000)
    . testGroup
      "Round-tripping"
    $ [ testProperty "toData >>> fromData"
          . forAllShrink arbitrary shrink
          $ toFrom
      , testProperty "fromData >>> toData"
          . forAllShrink arbitrary shrink
          $ fromTo
      ]
  where
    toFrom :: Foo -> Property
    toFrom x = Just x === (fromData . toData $ x)
    fromTo :: Foo -> Property
    fromTo x =
      (Just . toData $ x) === (toData @Foo <$> (fromData . toData $ x))

makeCoexistingIsData ''Foo
