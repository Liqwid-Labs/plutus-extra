{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-omit-interface-pragmas #-}

{- | PlutusTx version of NonEmpty, suitable for onchain usage.
The original version of this type is contained on Hackage in Data.List.NonEmpty.
The module contains many function that overlap with PlutusTx.
You will almost certainly want to import this module qualified.
-}
module PlutusTx.NonEmpty (
  -- * Datatype definition
  NonEmpty ((:|)),

  -- * Basic util functions

  -- ** Converting to and from a list
  toList,
  nonEmpty,

  -- ** Constructing and deconstruction
  singleton,
  (<|),
  cons,
  uncons,

  -- ** Extracting some part
  head,
  tail,
  init,
  last,
  take,
  filter,
  nub,
  nubBy,

  -- ** Zipping and unzipping
  zip,
  zip3,
  unzip,
  unzip3,

  -- ** Transforming
  reverse,
) where

--------------------------------------------------------------------------------

import Data.Kind (Type)
import Test.QuickCheck (
  Arbitrary (arbitrary, shrink),
  CoArbitrary (coarbitrary),
  Function (function),
  functionMap,
 )
import Prelude qualified

--------------------------------------------------------------------------------

import PlutusTx (makeLift)
import PlutusTx.Builtins qualified as Builtins
import PlutusTx.IsData.Class (
  FromData (fromBuiltinData),
  ToData (toBuiltinData),
  UnsafeFromData (unsafeFromBuiltinData),
 )
import PlutusTx.List.Extra qualified as PlutusTx (unzip3, zip3)
import PlutusTx.Prelude hiding (filter, head, nub, nubBy, reverse, tail, take, toList, zip)
import PlutusTx.Prelude qualified as PlutusTx (filter, nubBy, reverse, take, zip)

--------------------------------------------------------------------------------

infixr 5 :|

-- | A NonEmpty list is one which always has at least one element.
data NonEmpty (a :: Type) = a :| [a]
  deriving stock
    ( -- | @since 3.1
      Prelude.Eq
    , -- | @since 3.1
      Prelude.Ord
    )

instance Prelude.Show a => Prelude.Show (NonEmpty a) where
  {-# INLINEABLE show #-}
  show xs = "NonEmpty " ++ Prelude.show (toList xs)

instance Eq a => Eq (NonEmpty a) where
  {-# INLINEABLE (==) #-}
  (==) :: NonEmpty a -> NonEmpty a -> Bool
  (x :| xs) == (y :| ys) = x == y && xs == ys

instance Ord a => Ord (NonEmpty a) where
  {-# INLINEABLE compare #-}
  compare :: NonEmpty a -> NonEmpty a -> Ordering
  compare (x :| xs) (y :| ys) = compare x y <> compare xs ys

instance Semigroup (NonEmpty a) where
  {-# INLINEABLE (<>) #-}
  (<>) :: NonEmpty a -> NonEmpty a -> NonEmpty a
  (a :| as) <> ~(b :| bs) = a :| (as ++ b : bs)

instance Functor NonEmpty where
  {-# INLINEABLE fmap #-}
  fmap ::
    forall (a :: Type) (b :: Type).
    (a -> b) ->
    NonEmpty a ->
    NonEmpty b
  fmap f ~(a :| as) = f a :| fmap f as

instance Applicative NonEmpty where
  {-# INLINEABLE pure #-}
  pure :: forall (a :: Type). a -> NonEmpty a
  pure a = a :| []

  {-# INLINEABLE (<*>) #-}
  (<*>) ::
    forall (a :: Type) (b :: Type).
    NonEmpty (a -> b) ->
    NonEmpty a ->
    NonEmpty b
  ~(f :| fs) <*> as =
    let (b :| bs) = fmap f as
        bs' = [f' a' | f' <- fs, a' <- toList as]
     in b :| (bs ++ bs')

instance Foldable NonEmpty where
  {-# INLINEABLE foldMap #-}
  foldMap ::
    forall (m :: Type) (a :: Type).
    Monoid m =>
    (a -> m) ->
    NonEmpty a ->
    m
  foldMap f ~(a :| as) = f a <> foldMap f as

instance Traversable NonEmpty where
  {-# INLINEABLE traverse #-}
  traverse ::
    forall (a :: Type) (f :: Type -> Type) (b :: Type).
    Applicative f =>
    (a -> f b) ->
    NonEmpty a ->
    f (NonEmpty b)
  traverse f ~(a :| as) = liftA2 (:|) (f a) (traverse f as)

instance ToData a => ToData (NonEmpty a) where
  {-# INLINEABLE toBuiltinData #-}
  toBuiltinData = toBuiltinData . toList

instance FromData a => FromData (NonEmpty a) where
  {-# INLINEABLE fromBuiltinData #-}
  fromBuiltinData = maybe Nothing nonEmpty . fromBuiltinData

instance UnsafeFromData a => UnsafeFromData (NonEmpty a) where
  {-# INLINEABLE unsafeFromBuiltinData #-}
  unsafeFromBuiltinData d = case unsafeFromBuiltinData d of
    (a : as) -> a :| as
    _ -> Builtins.error ()

-- | @since 3.1
instance (Arbitrary a) => Arbitrary (NonEmpty a) where
  arbitrary = (:|) Prelude.<$> arbitrary Prelude.<*> arbitrary
  shrink (a :| as) = do
    a' <- shrink a
    as' <- shrink as
    Prelude.pure (a' :| as')

-- | @since 3.1
instance (CoArbitrary a) => CoArbitrary (NonEmpty a) where
  coarbitrary (a :| as) = coarbitrary a . coarbitrary as

-- | @since 3.1
instance (Function a) => Function (NonEmpty a) where
  function = functionMap to from
    where
      to :: NonEmpty a -> (a, [a])
      to (a :| as) = (a, as)
      from :: (a, [a]) -> NonEmpty a
      from (a, as) = a :| as

{-# INLINEABLE toList #-}

{- | Convert a stream to a normal list efficiently.

==== __Examples__

>>> toList (1 :| [2,3])
[1,2,3]
-}
toList :: forall (a :: Type). NonEmpty a -> [a]
toList ~(a :| as) = a : as

{-# INLINEABLE nonEmpty #-}

{- | 'nonEmpty' efficiently turns a normal list into a 'NonEmpty' stream,
 producing 'Nothing' if the input is empty.

==== __Examples__

Empty list case:
>>> nonEmpty ([] :: [Integer])
Nothing

Non-empty list case:
>>> nonEmpty (1 : 2 : 3 : [])
Just NonEmpty [1,2,3]
-}
nonEmpty :: forall (a :: Type). [a] -> Maybe (NonEmpty a)
nonEmpty [] = Nothing
nonEmpty (a : as) = Just (a :| as)

{-# INLINEABLE singleton #-}

{- | Construct a 'NonEmpty' list from a single element.

==== __Examples__

>>> singleton True
NonEmpty [True]
-}
singleton :: forall (a :: Type). a -> NonEmpty a
singleton a = a :| []

infixr 5 <|

{-# INLINEABLE (<|) #-}

{- | Prepend an element to the stream.

==== __Examples__

>>> 1 <| (2 :| 3 : [])
NonEmpty [1,2,3]
-}
(<|) :: forall (a :: Type). a -> NonEmpty a -> NonEmpty a
a <| ~(b :| bs) = a :| b : bs

{-# INLINEABLE cons #-}

{- | Synonym for '<|'.

==== __Examples__

>>> cons 1  (2 :| 3 : [])
NonEmpty [1,2,3]
-}
cons :: forall (a :: Type). a -> NonEmpty a -> NonEmpty a
cons = (<|)

{-# INLINEABLE uncons #-}

{- | 'uncons' produces the first element of the stream, and a stream of the
 remaining elements, if any.

==== __Examples__

>>> uncons (1 :| [])
(1,Nothing)

>>> uncons (1 :| 2 : [])
(1,Just NonEmpty [2])
-}
uncons :: forall (a :: Type). NonEmpty a -> (a, Maybe (NonEmpty a))
uncons ~(a :| as) = (a, nonEmpty as)

{-# INLINEABLE head #-}

{- | Extract the first element of the stream.

==== __Examples__

>>> head (1 :| [])
1
-}
head :: forall (a :: Type). NonEmpty a -> a
head ~(a :| _) = a

{-# INLINEABLE tail #-}

{- | Extract the possibly-empty tail of the stream.

==== __Examples__

>>> tail (1 :| [])
[]

>>> tail (1 :| 2 : 3 : [])
[2,3]
-}
tail :: forall (a :: Type). NonEmpty a -> [a]
tail ~(_ :| as) = as

{-# INLINEABLE init #-}

{- | Extract everything except the last element of the stream.

==== __Examples__

>>> tail (1 :| [])
[]

>>> tail (1 :| 2 : 3 : [])
[2,3]
-}
init :: forall (a :: Type). NonEmpty a -> [a]
init ~(a :| as) = go a as
  where
    go :: a -> [a] -> [a]
    go _ [] = []
    go x (y : ys) = x : go y ys

{-# INLINEABLE last #-}

{- | Extract the last element of the stream.

==== __Examples__

>>> last (1 :| [])
1

>>> last (1 :| 2 : 3 : [])
3
-}
last :: forall (a :: Type). NonEmpty a -> a
last ~(a :| as) = foldl (\_ x -> x) a as

{-# INLINEABLE take #-}

{- | @'take' n xs@ returns the first @n@ elements of @xs@.

==== __Examples__

Non-positive index:
>>> take (-1) (1 :| 2 : [])
[]

Index within the NonEmpty list
>>> take 2 (1 :| 2 : 3: [])
[1,2]

Index outside the NonEmpty list
>>> take 5 (1 :| 2 : 3: [])
[1,2,3]
-}
take :: forall (a :: Type). Integer -> NonEmpty a -> [a]
take n = PlutusTx.take n . toList

{-# INLINEABLE filter #-}

{- | @'filter' p xs@ removes any elements from @xs@ that do not satisfy @p@.

==== __Examples__

>>> filter (> 3) (1 :| [2,3,5,2,4]) :: [Integer]
[5,4]
-}
filter :: forall (a :: Type). (a -> Bool) -> NonEmpty a -> [a]
filter p = PlutusTx.filter p . toList

{-# INLINEABLE nub #-}

{- | The 'nub' function removes duplicate elements from a list. In
particular, it keeps only the first occurrence of each element.
It is a special case of 'nubBy', which allows the programmer to
supply their own inequality test.

==== __Examples__

>>> nub (1 :| [2,3,3,2,4]) :: NonEmpty Integer
NonEmpty [1,2,3,4]
-}
nub :: forall (a :: Type). Eq a => NonEmpty a -> NonEmpty a
nub = nubBy (==)

{-# INLINEABLE nubBy #-}

{- | The 'nubBy' function behaves just like 'nub', except it uses a
 user-supplied equality predicate instead of the overloaded '=='
 function.

==== __Examples__

>>> nubBy (==) (1 :| [2,3,3,2,4]) :: NonEmpty Integer
NonEmpty [1,2,3,4]

>>> nubBy (<) (2 :| [1,3,3,4,1]) :: NonEmpty Integer
NonEmpty [2,1,1]
-}
nubBy :: forall (a :: Type). (a -> a -> Bool) -> NonEmpty a -> NonEmpty a
nubBy eq (x :| xs) = x :| rest
  where
    rest :: [a]
    rest = PlutusTx.nubBy eq $ PlutusTx.filter (\x' -> not $ x `eq` x') xs

{-# INLINEABLE zip #-}

{- | The 'zip' function takes two streams and returns a stream of
 corresponding pairs.

==== __Examples__

>>> zip (1 :| [2,3]) (True :| [False]) :: NonEmpty (Integer, Bool)
NonEmpty [(1,True),(2,False)]
-}
zip ::
  forall (a :: Type) (b :: Type).
  NonEmpty a ->
  NonEmpty b ->
  NonEmpty (a, b)
zip ~(x :| xs) ~(y :| ys) = (x, y) :| PlutusTx.zip xs ys

{-# INLINEABLE zip3 #-}

{- | Analogue of 'zip' function with 3 streams.

==== __Examples__

>>> zip3 (1 :| [2,3]) (True :| [False]) ('a' :| ['b', 'c', 'd']) :: NonEmpty (Integer, Bool, P.Char)
NonEmpty [(1,True,'a'),(2,False,'b')]
-}
zip3 ::
  forall (a :: Type) (b :: Type) (c :: Type).
  NonEmpty a ->
  NonEmpty b ->
  NonEmpty c ->
  NonEmpty (a, b, c)
zip3 ~(x :| xs) ~(y :| ys) ~(z :| zs) = (x, y, z) :| PlutusTx.zip3 xs ys zs

{-# INLINEABLE unzip #-}

{- | The 'unzip' function is the inverse of the 'zip' function.

==== __Examples__

>>> unzip ((1, True) :| (2, False) : [])
(NonEmpty [1,2],NonEmpty [True,False])
-}
unzip ::
  forall (a :: Type) (b :: Type).
  NonEmpty (a, b) ->
  (NonEmpty a, NonEmpty b)
unzip xs = (fst <$> xs, snd <$> xs)

{-# INLINEABLE unzip3 #-}

{- | The 'unzip3' function takes a stream of triples and returns three
streams, analogous to 'unzip'.

==== __Examples__

>>> unzip3 ((1, True, 'a') :| (2, False, 'b') : [])
(NonEmpty [1,2],NonEmpty [True,False],NonEmpty "ab")
-}
unzip3 ::
  forall (a :: Type) (b :: Type) (c :: Type).
  NonEmpty (a, b, c) ->
  (NonEmpty a, NonEmpty b, NonEmpty c)
unzip3 ~((x, y, z) :| lst) =
  let (xs, ys, zs) = PlutusTx.unzip3 lst
   in (x :| xs, y :| ys, z :| zs)

{-# INLINEABLE reverse #-}

{- | 'reverse' a finite NonEmpty stream.

==== __Examples__

>>> reverse (1 :| [])
NonEmpty [1]

>>> reverse (1 :| [2,3])
NonEmpty [3,2,1]
-}
reverse :: forall (a :: Type). NonEmpty a -> NonEmpty a
reverse (x :| xs) = case PlutusTx.reverse xs of
  [] -> x :| []
  (y : ys) -> y :| ys ++ [x]

makeLift ''NonEmpty
