{-# LANGUAGE TemplateHaskell #-}

module Main (main) where

import PlutusTx.Prelude (BuiltinString)
import PlutusTx.Prelude qualified as PTx
import PlutusTx.Skeleton (makeSkeletal, showSkeletal)
import Test.QuickCheck (Property, forAllShrinkShow, (===))
import Test.QuickCheck.Arbitrary (Arbitrary (arbitrary, shrink))
import Test.QuickCheck.Plutus.Instances ()
import Test.Tasty (defaultMain, localOption, testGroup)
import Test.Tasty.QuickCheck (
  QuickCheckTests,
  testProperty,
 )
import Text.Show.Pretty (ppShow)

main :: IO ()
main =
  defaultMain . localOption go . testGroup "Skeletal" $
    [ testProperty "BuiltinString pretty-print" stringSkeletalProp
    , testProperty "Tuple pretty-print" tupleSkeletalProp
    , testProperty "Record pretty-print" recordSkeletalProp
    ]
  where
    go :: QuickCheckTests
    go = 100_000

-- Helpers

stringSkeletalProp :: Property
stringSkeletalProp = forAllShrinkShow arbitrary shrink ppShow go
  where
    go :: BuiltinString -> Property
    go s =
      let lhs = "\"" PTx.<> s PTx.<> "\""
       in lhs === showSkeletal s

tupleSkeletalProp :: Property
tupleSkeletalProp = forAllShrinkShow arbitrary shrink ppShow go
  where
    go :: (BuiltinString, Integer) -> Property
    go tup@(s, i) =
      let lhs =
            "Tuple: "
              PTx.<> showSkeletal s
              PTx.<> ", "
              PTx.<> showSkeletal i
          rhs = showSkeletal tup
       in lhs === rhs

recordSkeletalProp :: Property
recordSkeletalProp = forAllShrinkShow arbitrary shrink ppShow go
  where
    go :: Foo -> Property
    go f@(Foo x y) =
      let lhs =
            "Record Foo:\n"
              PTx.<> "\n, bar: "
              PTx.<> showSkeletal x
              PTx.<> "\n, baz: "
              PTx.<> showSkeletal y
       in lhs === showSkeletal f

data Foo = Foo
  { bar :: BuiltinString
  , baz :: Integer
  }
  deriving stock (Eq, Show)

instance PTx.Eq Foo where
  Foo x y == Foo x' y' = x PTx.== x' PTx.&& y PTx.== y'

$(makeSkeletal ''Foo)

instance Arbitrary Foo where
  arbitrary = Foo <$> arbitrary <*> arbitrary
  shrink (Foo x y) = Foo <$> shrink x <*> shrink y
