module Plutus.V1.Ledger.Value.Norm (
  NormValue,
  toNormValue,
  fromNormValue,
  mapNormValue,
  unsafeMapNormValue,
) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Kind (Type)
import Plutus.V1.Ledger.Api qualified as Ledger
import Plutus.V1.Ledger.Value qualified as Value
import PlutusTx (FromData, ToData, UnsafeFromData)
import PlutusTx.AssocMap qualified as AssocMap
import PlutusTx.Prelude
import Prettyprinter (Pretty)
import Prettyprinter.Extras (PrettyShow (PrettyShow))
import Prelude qualified

newtype NormValue = NormValue Ledger.Value
  deriving stock (Prelude.Show)
  deriving (Pretty) via (PrettyShow NormValue)
  deriving
    ( ToJSON
    , FromJSON
    , ToData
    , FromData
    , UnsafeFromData
    , Prelude.Eq
    , Eq
    , Prelude.Semigroup
    , Semigroup
    , Prelude.Monoid
    , Monoid
    , Group
    , AdditiveSemigroup
    , AdditiveMonoid
    , AdditiveGroup
    , JoinSemiLattice
    , MeetSemiLattice
    )
    via Ledger.Value

deriving via Ledger.Value instance Module Integer NormValue

instance MultiplicativeSemigroup NormValue where
  {-# INLINEABLE (*) #-}
  NormValue (Ledger.Value val1) * NormValue (Ledger.Value val2) =
    NormValue $ Ledger.Value $ mapZipWith (mapZipWith (*)) val1 val2

{-# INLINEABLE mapZipWith #-}
mapZipWith ::
  forall (k :: Type) (a :: Type) (b :: Type) (c :: Type).
  Eq k =>
  (a -> b -> c) ->
  AssocMap.Map k a ->
  AssocMap.Map k b ->
  AssocMap.Map k c
mapZipWith f map0 map1 =
  AssocMap.mapMaybeWithKey (\k v -> f v <$> AssocMap.lookup k map1) map0

{-# INLINEABLE toNormValue #-}
toNormValue :: Ledger.Value -> NormValue
toNormValue = NormValue . normalizeValue

{-# INLINEABLE fromNormValue #-}
fromNormValue :: NormValue -> Ledger.Value
fromNormValue (NormValue v) = v

{-# INLINEABLE mapNormValue #-}
mapNormValue :: (Ledger.Value -> Ledger.Value) -> NormValue -> NormValue
mapNormValue f = toNormValue . f . fromNormValue

{-# INLINEABLE unsafeMapNormValue #-}
unsafeMapNormValue :: (Ledger.Value -> Ledger.Value) -> NormValue -> NormValue
unsafeMapNormValue f = NormValue . f . fromNormValue

{-# INLINEABLE normalizeValue #-}
normalizeValue :: Ledger.Value -> Ledger.Value
normalizeValue (Ledger.Value v) =
  fold
    . concatMap
      ( \(sym, xs) ->
          map (uncurry $ Value.singleton sym) $ AssocMap.toList xs
      )
    $ AssocMap.toList v