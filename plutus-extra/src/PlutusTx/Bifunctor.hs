{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -fno-omit-interface-pragmas #-}

{- | PlutusTX onchain realisation of the Bifunctor typeclass
with some of its usefull instances.
The original version could be found on [Hackage](https://hackage.haskell.org/package/bifunctors-5)
-}
module PlutusTx.Bifunctor (
  -- * Typeclass
  Bifunctor,
  bimap,

  -- * Helper functions
  first,
  second,
) where

--------------------------------------------------------------------------------

import Data.Kind (Type)

--------------------------------------------------------------------------------

import Ledger.Constraints.TxConstraints (
  InputConstraint,
  OutputConstraint,
  TxConstraints (TxConstraints),
  icRedeemer,
  ocDatum,
  txConstraints,
  txOwnInputs,
  txOwnOutputs,
 )
import PlutusTx.Either (Either (Left, Right))
import PlutusTx.Prelude (fmap, id)
import PlutusTx.These (These (That, These, This))

--------------------------------------------------------------------------------

{- | A type constructor @f@ of kind (Type -> Type -> Type) is a Bifunctor
if it behaves like a Functor over both of it`s type arguments.
Furthermore @f@ needs to adhere to the following:
[Identity]    @'bimap' 'id' 'id' == 'id'@
[Composition] @'bimap' (f . g) (h . k) == 'bimap' f h . 'bimap' g k@
-}
class Bifunctor (f :: Type -> Type -> Type) where
  -- | Map over both arguments at the same time.
  bimap ::
    forall (a1 :: Type) (a2 :: Type) (b1 :: Type) (b2 :: Type).
    -- | Function to map over first argument
    (a1 -> b1) ->
    -- | Function to map over second argument
    (a2 -> b2) ->
    f a1 a2 ->
    f b1 b2

instance Bifunctor Either where
  {-# INLINEABLE bimap #-}
  bimap ::
    forall (a1 :: Type) (a2 :: Type) (b1 :: Type) (b2 :: Type).
    (a1 -> b1) ->
    (a2 -> b2) ->
    Either a1 a2 ->
    Either b1 b2
  bimap f g = \case
    Left a -> Left (f a)
    Right b -> Right (g b)

instance Bifunctor (,) where
  {-# INLINEABLE bimap #-}
  bimap ::
    forall (a1 :: Type) (a2 :: Type) (b1 :: Type) (b2 :: Type).
    (a1 -> b1) ->
    (a2 -> b2) ->
    (a1, a2) ->
    (b1, b2)
  bimap f g ~(x, y) = (f x, g y)

instance Bifunctor These where
  {-# INLINEABLE bimap #-}
  bimap ::
    forall (a1 :: Type) (a2 :: Type) (b1 :: Type) (b2 :: Type).
    (a1 -> b1) ->
    (a2 -> b2) ->
    These a1 a2 ->
    These b1 b2
  bimap f g = \case
    This a -> This (f a)
    That b -> That (g b)
    These a b -> These (f a) (g b)

instance Bifunctor TxConstraints where
  {-# INLINEABLE bimap #-}
  bimap ::
    forall (a1 :: Type) (a2 :: Type) (b1 :: Type) (b2 :: Type).
    (a1 -> b1) ->
    (a2 -> b2) ->
    TxConstraints a1 a2 ->
    TxConstraints b1 b2
  bimap f g txc =
    TxConstraints
      { txConstraints = txConstraints txc
      , txOwnInputs = fmap (fmapInput f) (txOwnInputs txc)
      , txOwnOutputs = fmap (fmapOutput g) (txOwnOutputs txc)
      }
    where
      fmapInput ::
        forall (a :: Type) (b :: Type).
        (a -> b) ->
        InputConstraint a ->
        InputConstraint b
      fmapInput h con = con {icRedeemer = h (icRedeemer con)}

      fmapOutput ::
        forall (a :: Type) (b :: Type).
        (a -> b) ->
        OutputConstraint a ->
        OutputConstraint b
      fmapOutput h con = con {ocDatum = h (ocDatum con)}

{- | Map covariantly over the first argument.
==== __Example__
>>> first (+1) (4, 8)
(5, 8)
-}
{-# INLINEABLE first #-}
first ::
  forall (f :: Type -> Type -> Type) (a1 :: Type) (b1 :: Type) (a2 :: Type).
  Bifunctor f =>
  (a1 -> b1) ->
  f a1 a2 ->
  f b1 a2
first f = bimap f id

{- | Map covariantly over the second argument.
==== __Example__
>>> second (+1) (4, 8)
(4, 9)
-}
{-# INLINEABLE second #-}
second ::
  forall (f :: Type -> Type -> Type) (a2 :: Type) (b2 :: Type) (a1 :: Type).
  Bifunctor f =>
  (a2 -> b2) ->
  f a1 a2 ->
  f a1 b2
second = bimap id
