{-# LANGUAGE ViewPatterns #-}

import Data.Function (on)
import Data.Kind (Type)
import Data.List (nub, sort, sortOn)
import PlutusTx.List.Natural qualified as Nat
import PlutusTx.List.Ord (
  isSorted,
  isSortedAscending,
  isSortedAscendingOn,
  isSortedOn,
  ordNub,
  ordNubBy,
 )
import PlutusTx.List.Ord qualified as List
import PlutusTx.Natural (Natural)
import PlutusTx.Prelude qualified as PTx
import Test.Tasty (TestTree, defaultMain, localOption, testGroup)
import Test.Tasty.QuickCheck (
  Arbitrary (arbitrary, shrink),
  Fun (Fun),
  Gen,
  Property,
  QuickCheckTests,
  Testable (property),
  checkCoverage,
  cover,
  forAllShrink,
  oneof,
  testProperty,
 )

main :: IO ()
main = defaultMain $ testGroup "Tests" tests

tests :: [TestTree]
tests =
  [ localOption go
      . testGroup "List functions with Natural"
      $ [ testProperty "length == PlutusTx.length" propLengthAgree
        , testProperty "take == PlutusTx.take" propTakeAgree
        , testProperty "take n xs ++ drop n xs == xs" propTakeDrop
        , testProperty "splitAt n xs == (take n xs, drop n xs)" propSplitAt
        , testProperty "length (replicate n x) == n" propReplicateLength
        , testProperty "forall y in (replicate n x), x == y" propReplicateElem
        ]
  , localOption go
      . testGroup "Sort and ordNub"
      $ [ testProperty "sort == Prelude.sort" propSort
        , testProperty "ordNub == Prelude.nub . Prelude.sort" propOrdNub
        ]
  , testGroup
      "List sortedness"
      [ testProperty "isSorted xs == (sort xs == xs)" propIsSorted
      , testProperty "isSortedAscending xs == (ordNub xs == xs)" propIsSortedAscending
      , testProperty "isSortedOn f xs == isSorted (f <$> xs)" propIsSortedOn
      , testProperty "isSortedAscendingOn f xs == isSortedAscending (f <$> xs)" propIsSortedAscendingOn
      ]
  ]
  where
    go :: QuickCheckTests
    go = 100000

-- This ensures at least 50% sorted lists.
data SomeSorted a = NotSorted [a] | IsSorted [a]
  deriving stock (Show, Eq)

unsort :: forall (a :: Type). SomeSorted a -> [a]
unsort (NotSorted xs) = xs
unsort (IsSorted xs) = xs

arbitrarySortedOn ::
  forall (a :: Type) (b :: Type).
  (Arbitrary a, Ord b) =>
  (a -> b) ->
  Gen (SomeSorted a)
arbitrarySortedOn f =
  oneof
    [ NotSorted <$> arbitrary
    , IsSorted . sortOn f <$> arbitrary
    ]

shrinkSortedOn ::
  forall (a :: Type) (b :: Type).
  (Arbitrary a, Ord b) =>
  (a -> b) ->
  SomeSorted a ->
  [SomeSorted a]
shrinkSortedOn f = \case
  NotSorted xs -> NotSorted <$> shrink xs
  IsSorted xs -> IsSorted . sortOn f <$> shrink xs

instance
  forall (a :: Type).
  (Arbitrary a, Ord a) =>
  Arbitrary (SomeSorted a)
  where
  arbitrary = arbitrarySortedOn id
  shrink = shrinkSortedOn id

-- This ensures at least 50% lists in strict ascending order.
data SomeAscending a = NotAscending [a] | IsAscending [a]
  deriving stock (Show, Eq)

unasc :: forall (a :: Type). SomeAscending a -> [a]
unasc (NotAscending xs) = xs
unasc (IsAscending xs) = xs

arbitraryAscendingOn ::
  forall (a :: Type) (b :: Type).
  (Arbitrary a, Ord b) =>
  (a -> b) ->
  Gen (SomeAscending a)
arbitraryAscendingOn f =
  oneof
    [ NotAscending <$> arbitrary
    , IsAscending . ordNubBy (compare `on` f) . sortOn f <$> arbitrary
    ]

shrinkAscendingOn ::
  forall (a :: Type) (b :: Type).
  (Arbitrary a, Ord b) =>
  (a -> b) ->
  SomeAscending a ->
  [SomeAscending a]
shrinkAscendingOn f = \case
  NotAscending xs -> NotAscending <$> shrink xs
  IsAscending xs ->
    IsAscending . ordNubBy (compare `on` f) . sortOn f <$> shrink xs

instance
  forall (a :: Type).
  (Arbitrary a, Ord a) =>
  Arbitrary (SomeAscending a)
  where
  arbitrary = arbitraryAscendingOn id
  shrink = shrinkAscendingOn id

propLengthAgree :: Property
propLengthAgree = property $ \(xs :: [Int]) ->
  PTx.fromEnum (Nat.length xs) == PTx.fromEnum (PTx.length xs)

propTakeAgree :: Property
propTakeAgree = property $ \(n :: Natural) (xs :: [Int]) ->
  Nat.take n xs == PTx.take (PTx.toEnum $ PTx.fromEnum n) xs

propTakeDrop :: Property
propTakeDrop = property $ \(n :: Natural) (xs :: [Int]) ->
  Nat.take n xs <> Nat.drop n xs == xs

propSplitAt :: Property
propSplitAt = property $ \(n :: Natural) (xs :: [Int]) ->
  Nat.splitAt n xs == (Nat.take n xs, Nat.drop n xs)

propReplicateLength :: Property
propReplicateLength = property $ \(n :: Natural) (x :: Int) ->
  Nat.length (Nat.replicate n x) == n

propReplicateElem :: Property
propReplicateElem = property $ \(n :: Natural) (x :: Int) ->
  all (== x) $ Nat.replicate n x

propSort :: Property
propSort = property $ \(xs :: [PTx.Integer]) -> sort xs == List.sort xs

propOrdNub :: Property
propOrdNub = property $ \(xs :: [PTx.Integer]) ->
  nub (sort xs) == ordNub xs

propIsSorted :: Property
propIsSorted = property $ \(unsort -> xs :: [PTx.Integer]) ->
  checkCoverage
    . cover 50.0 (isSorted xs) "precondition known satisfied"
    $ isSorted xs == (List.sort xs == xs)

propIsSortedAscending :: Property
propIsSortedAscending = property $ \(unasc -> xs :: [PTx.Integer]) ->
  checkCoverage
    . cover 50.0 (isSortedAscending xs) "precondition known satisfied"
    $ isSortedAscending xs == (ordNub xs == xs)

propIsSortedOn :: Property
propIsSortedOn = property $ \(Fun _ f :: Fun PTx.Integer PTx.Integer) ->
  forAllShrink (arbitrarySortedOn f) (shrinkSortedOn f) $ \(unsort -> xs) ->
    checkCoverage
      . cover 50.0 (isSortedOn f xs) "precondition known satisfied"
      $ isSortedOn f xs == isSorted (f <$> xs)

propIsSortedAscendingOn :: Property
propIsSortedAscendingOn = property $
  \(Fun _ f :: Fun PTx.Integer PTx.Integer) ->
    forAllShrink (arbitraryAscendingOn f) (shrinkAscendingOn f) $
      \(unasc -> xs) ->
        checkCoverage
          . cover 50.0 (isSortedAscendingOn f xs) "precondition known satisfied"
          $ isSortedAscendingOn f xs == isSortedAscending (f <$> xs)
