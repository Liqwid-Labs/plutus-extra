module PlutusTx.Skeleton.Internal (
  Skeleton (..),
  Skeletal (..),
) where

import PlutusTx.Prelude qualified as PTx

-- | @since 2.1
data Skeleton
  = -- | @since 2.1
    BuiltinS PTx.BuiltinData
  | -- | @since 2.1
    ConS PTx.BuiltinString [Skeleton]
  | -- | @since 2.1
    RecS (PTx.BuiltinString, Skeleton) [(PTx.BuiltinString, Skeleton)]
  | -- | @since 2.1
    TupleS Skeleton [Skeleton]
  | -- | @since 2.1
    ListS [Skeleton]
  deriving stock
    ( -- | @since 2.1
      Eq
    , -- | @since 2.1
      Show
    )

-- | @since 2.1
instance PTx.Eq Skeleton where
  {-# INLINEABLE (==) #-}
  sk == sk' = case (sk, sk') of
    (BuiltinS dat, BuiltinS dat') -> dat PTx.== dat'
    (ConS nam sks, ConS nam' sks') ->
      nam PTx.== nam' PTx.&& sks PTx.== sks'
    (RecS keyVal keyVals, RecS keyVal' keyVals') ->
      keyVal PTx.== keyVal' PTx.&& keyVals PTx.== keyVals'
    (TupleS x xs, TupleS x' xs') ->
      x PTx.== x' PTx.&& xs PTx.== xs'
    (ListS xs, ListS xs') -> xs PTx.== xs'
    _ -> False

{- | @since 2.1

 Instance must define a representable functor, that is, x == y iff skeletize
 x == skeletize y
-}
class (PTx.Eq a) => Skeletal a where
  skeletize :: a -> Skeleton

-- | @since 2.1
instance Skeletal PTx.BuiltinData where
  {-# INLINEABLE skeletize #-}
  skeletize = BuiltinS

-- | @since 2.1
instance (Skeletal a) => Skeletal [a] where
  {-# INLINEABLE skeletize #-}
  skeletize = ListS . PTx.fmap skeletize

-- | @since 2.1
instance (Skeletal a, Skeletal b) => Skeletal (a, b) where
  {-# INLINEABLE skeletize #-}
  skeletize (x, y) = TupleS (skeletize x) [skeletize y]
