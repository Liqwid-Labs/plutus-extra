{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE Trustworthy #-}
{-# OPTIONS_GHC -fno-ignore-interface-pragmas #-}
{-# OPTIONS_GHC -fno-omit-interface-pragmas #-}
{-# OPTIONS_GHC -fno-specialize #-}

{- |
 Module: PlutusTx.Numeric.Extra
 Copyright: (C) MLabs 2021
 License: Apache 2.0
 Maintainer: Koz Ross <koz@mlabs.city>
 Portability: GHC only
 Stability: Experimental

 A collection of type classes and operations which extend the Plutus numerical
 hierarchy with additional capabilities and laws.
-}
module PlutusTx.Numeric.Extra (
  -- * Type classes
  AdditiveHemigroup (..),
  EuclideanClosed (..),
  MultiplicativeGroup (..),
  IntegralDomain (..),

  -- * Helper types
  Hemiring,
  Field,
  Hemifield,

  -- * Helper functions
  monus,
  div,
  rem,
  (^),
  powNat,
) where

import Data.Kind (Type)
import PlutusTx.NatRatio.Internal (NatRatio (NatRatio), nrMonus)
import PlutusTx.Natural.Internal (Natural (Natural))
import PlutusTx.Prelude hiding (divMod, even, (%))
import PlutusTx.Rational qualified as Rational
import PlutusTx.Rational.Internal (
  Rational (Rational),
  rDiv,
  rPowInteger,
 )
import Prelude ()

{- | Raise by a 'Natural' power.

 = Note

 This really /ought/ to be a method of 'MultiplicativeMonoid', but as we can't
 modify type classes provided by Plutus, this is the next best option.

 = Laws

 1. @'powNat' x 'zero' = 'one'@
 2. @'powNat' x 'one' = x@
 3. If @n '>' 'one'@, then @'powNat' x n = x '*' 'powNat' x (n '^-' 'one')@

 @since 1.0
-}
{-# INLINEABLE powNat #-}
powNat ::
  forall (m :: Type).
  (MultiplicativeMonoid m) =>
  m ->
  Natural ->
  m
powNat x (Natural n) =
  if n == zero
    then one
    else expBySquaring x n

infixr 8 `powNat`

{- | An 'AdditiveMonoid' with a notion of monus. Provides one operation @'^-'@,
 also called \'monus\'.

 = Laws

 In addition to the standard laws of being an 'AdditiveMonoid', the following
 must also hold:

 1. @a '+' (b '^-' a) = b + (a '^-' b)@
 2. @(a '^-' b) '^-' c = a '^-' (b '+' c)@
 3. @a '^-' a = 'zero'@
 4. @'zero' '^-' a = 'zero'@

 Our use of the term \'hemigroup\' is different to its use in Gondran and
 Minoux: they use the term to mean a cancellative monoid with a canonical
 natural order. We use this terminology for symmetry with groups, as monus is
 meant to act as a \'replacement\' for additive inverse.

 /See also:/

 * Gondran, Michel and Minoux, Michel; /Graphs, Dioids and Semirings: New
   Models and Algorithms/; Springer, 2008.
 * [Monus](https://en.wikipedia.org/wiki/Monus)

 @since 1.0
-}
class (AdditiveMonoid a) => AdditiveHemigroup a where
  (^-) :: a -> a -> a

infixl 6 ^-

{- | 'Natural' monus is \'difference-or-zero\'.

 @since 1.0
-}
instance AdditiveHemigroup Natural where
  {-# INLINEABLE (^-) #-}
  Natural n ^- Natural m
    | m > n = zero
    | otherwise = Natural (n - m)

{- | 'NatRatio' monus is \'difference-or-zero\'.

 @since 1.0
-}
instance AdditiveHemigroup NatRatio where
  {-# INLINEABLE (^-) #-}
  nr ^- nr' = nrMonus nr nr'

{- | We define the combination of an 'AdditiveHemigroup' and a
 'MultiplicativeMonoid' as a \'hemiring\'. This is designed to be symmetric
 with \'ring\' (much as \'hemigroup\' is symmetric with \'group\').

 @since 1.0
-}
type Hemiring a = (AdditiveHemigroup a, MultiplicativeMonoid a)

{- | A semiring with a notion of (kind of) Euclidean division. This differs from
 the mathematical notion of this, as we do not exclude zero.

 Intuitively, we provide an operation corresponding to \'division with
 remainder\' which has invertibility properties in combination with /both/ '+'
 and '*'. This combination is lawful, total and closed, and is general enough
 to only require a semiring constraint; in particular, this means that both
 the ring and dioid \'universes\' can participate equally (though the presence
 of additive inverses complicates the laws somewhat).

 We avoid the paradoxes induced by defining zero division by \'coupling\'
 division and remainder, which avoids this problem, as we do not claim that
 either \'half\' of the operation is invertible by itself.

 = Laws

 In addition to the unstated laws of being a 'Semiring', the following must
 hold:

 1. If @'divMod' x y = (d, r)@ then @(d '*' y) '+' r = x@
 2. @'divMod' x 'zero' = ('zero', x)@
 3. If @'divMod' x y = (d, r)@ and @y '/=' 'zero'@, then @'zero' '<=' |r| '<'
    |y|@. This property is simplified to @'zero' '<=' r '<' y@ if there is no
    notion of additive inverses for the instance.

 = Note

 The 'Ord' constraint on the instance may or may not be a natural (or indeed,
 canonical) order.

 @since 1.0
-}
class (Ord a, Semiring a) => EuclideanClosed a where
  -- | \'Division with remainder\', producing both results.
  divMod :: a -> a -> (a, a)

-- | @since 1.0
instance EuclideanClosed Natural where
  {-# INLINEABLE divMod #-}
  divMod (Natural x) (Natural y) = case x `divMod` y of
    (d, r) -> (Natural d, Natural r)

-- | @since 1.0
instance EuclideanClosed Integer where
  {-# INLINEABLE divMod #-}
  divMod x y =
    if y == zero
      then (zero, x)
      else (quotient x y, remainder x y)

{- | A multiplicative monoid with a notion of multiplicative inverse (for
 non-zero values).

 We have to exclude division by zero, as it leads to paradoxical situations.
 This does mean that '/' and 'reciprocal' aren't total, but there's not much
 we can do about that.

 = Laws

 These assume @y /= 0@.

 1. If @x '/' y = z@ then @y '*' z = x@.
 2. @x '/' y = x '*' 'reciprocal' y@
 3. @'powInteger' x 'zero' = 'one'@
 4. @'powInteger' x 'one' = x@
 5. If @i '<' 0@, then @'powInteger' x i = 'reciprocal' '.' 'powInteger' x .
    'abs' '$' i@
 6. If @i '>' 1@, then @'powInteger' x i = 'x '*' 'powInteger' x (i '-' 'one)@

 @since 1.0
-}
class (MultiplicativeMonoid a) => MultiplicativeGroup a where
  {-# MINIMAL (/) | reciprocal #-}
  (/) :: a -> a -> a
  x / y = x * reciprocal y
  reciprocal :: a -> a
  reciprocal x = one / x

  -- | Raise by an 'Integer' power.
  powInteger :: a -> Integer -> a
  powInteger x i
    | i == zero = one
    | i == one = x
    | i < zero = reciprocal . expBySquaring x . abs $ i
    | otherwise = expBySquaring x i

infixr 8 `powInteger`

-- | @since 1.0
instance MultiplicativeGroup Rational.Rational where
  {-# INLINEABLE (/) #-}
  (/) = rDiv
  {-# INLINEABLE reciprocal #-}
  reciprocal = Rational.recip
  {-# INLINEABLE powInteger #-}
  powInteger = rPowInteger

-- | @since 1.0
instance MultiplicativeGroup NatRatio where
  {-# INLINEABLE (/) #-}
  NatRatio r / NatRatio r' = NatRatio (r / r')
  {-# INLINEABLE reciprocal #-}
  reciprocal (NatRatio r) = NatRatio . reciprocal $ r
  {-# INLINEABLE powInteger #-}
  powInteger (NatRatio r) = NatRatio . powInteger r

-- | @since 1.0
type Field a = (AdditiveGroup a, MultiplicativeGroup a)

-- | @since 1.0
type Hemifield a = (AdditiveHemigroup a, MultiplicativeGroup a)

{- | A ring with a notion of \'sign\' or \'direction\' separate from magnitude.
 This allows us to define notions of [absolute
 value](https://en.wikipedia.org/wiki/Absolute_value_(algebra)) and [signum
 function](https://en.wikipedia.org/wiki/Sign_function).

 We extend the notion of integral domain with the idea of an \'additive
 restriction\', which is a type representing \'strictly positive\' values. For
 example, the \'additive restriction\' of 'Integer' is 'Natural', and the
 \'additive restricton\' of 'Rational' is 'NatRatio'. This gives us the
 ability to move between these two types while preserving magnitude.

 = Laws

 For 'abs', the following laws apply:

 1. @'abs' x '>=' 'zero'@
 2. @x '<=' 'abs' x@
 3. @'abs' (x '*' y) = 'abs' x '*' 'abs' y@

 Additionally, if you define 'signum', the following law applies:

 4. @'abs' x '*' 'signum' x = x@

 For the methods relating to the additive restriction, the following law
 applies:

 1. @'addExtend' '.' 'projectAbs' '$' x = 'abs' x@

 Additionally, if you define 'restrictMay', the following law applies:

 2. @restrictMay x = Just y@ if and only if @abs x = x@.

 @since 1.0
-}
class (Ord a, Ring a) => IntegralDomain a r | a -> r, r -> a where
  abs :: a -> a
  projectAbs :: a -> r
  addExtend :: r -> a
  restrictMay :: a -> Maybe r
  restrictMay x
    | abs x == x = Just . projectAbs $ x
    | otherwise = Nothing
  signum :: a -> a
  signum x
    | x == zero = zero
    | x > zero = one
    | otherwise = negate one

-- | @since 1.0
instance IntegralDomain Integer Natural where
  {-# INLINEABLE abs #-}
  abs x
    | x < zero = negate x
    | otherwise = x
  {-# INLINEABLE projectAbs #-}
  projectAbs = Natural . abs
  {-# INLINEABLE restrictMay #-}
  restrictMay x
    | x < zero = Nothing
    | otherwise = Just . Natural $ x
  {-# INLINEABLE addExtend #-}
  addExtend (Natural i) = i

-- | @since 1.0
instance IntegralDomain Rational.Rational NatRatio where
  {-# INLINEABLE abs #-}
  abs = Rational.abs
  {-# INLINEABLE projectAbs #-}
  projectAbs = NatRatio . Rational.abs
  {-# INLINEABLE restrictMay #-}
  restrictMay x
    | x < zero = Nothing
    | otherwise = Just . NatRatio $ x
  {-# INLINEABLE addExtend #-}
  addExtend (NatRatio r) = r
  {-# INLINEABLE signum #-}
  signum (Rational n _)
    | n < zero = Rational (negate one) one
    | n == zero = zero
    | otherwise = one

{- | Non-operator version of '^-'.

 @since 1.0
-}
{-# INLINE monus #-}
monus ::
  forall (a :: Type).
  (AdditiveHemigroup a) =>
  a ->
  a ->
  a
monus = (^-)

infixl 6 `monus`

{- | Gets only the division part of a 'divMod'.

 @since 1.0
-}
{-# INLINE div #-}
div ::
  forall (a :: Type).
  (EuclideanClosed a) =>
  a ->
  a ->
  a
div x = fst . divMod x

infixl 7 `div`

{- | Gets only the remainder part of a 'divMod'.

 @since 1.0
-}
{-# INLINE rem #-}
rem ::
  forall (a :: Type).
  (EuclideanClosed a) =>
  a ->
  a ->
  a
rem x = snd . divMod x

infixl 7 `rem`

{-# INLINE (^) #-}

{- | Operator version of 'powInteger'.

 @since 1.0
-}
(^) ::
  forall (a :: Type).
  (MultiplicativeGroup a) =>
  a ->
  Integer ->
  a
(^) = powInteger

infixr 8 ^

-- Helpers

{-# INLINEABLE expBySquaring #-}
{- HLint ignore expBySquaring -}
-- We secretly know that i is always positive.
expBySquaring ::
  forall (a :: Type).
  (MultiplicativeMonoid a) =>
  a ->
  Integer ->
  a
expBySquaring acc i
  | i == one = acc
  | even' i = expBySquaring (square acc) . halve $ i
  | otherwise = (acc *) . expBySquaring (square acc) . halve $ i
  where
    square :: a -> a
    square y = y * y
    halve :: Integer -> Integer
    halve = (`divide` 2)
    even' :: Integer -> Bool
    even' x = x `rem` 2 == 0
