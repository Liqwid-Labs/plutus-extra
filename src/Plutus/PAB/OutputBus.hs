{-# LANGUAGE DeriveAnyClass #-}

module Plutus.PAB.OutputBus (
  OutputBus (getOutputBus),
  sendBus
) where

--------------------------------------------------------------------------------

import Data.Kind (Type)
import Data.Row (Row)
import Data.Semigroup (Last (Last))
import GHC.Generics (Generic)
import Prelude (Eq, Monoid, Semigroup, Show)

--------------------------------------------------------------------------------

import Data.Aeson (FromJSON, ToJSON)

--------------------------------------------------------------------------------

import Plutus.Contract (Contract, tell)
import PlutusTx.Prelude (Maybe (Just), (.))

--------------------------------------------------------------------------------

-- Transferring information between Contract and real world

-- | A channel for communicating the last state of a particular value to leak out of the contract monad.
newtype OutputBus a = OutputBus {getOutputBus :: Maybe (Last a)}
  deriving newtype (Show, Eq, Semigroup, Monoid)
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Write to the output bus
sendBus ::
  forall (a :: Type) (s :: Row Type) (e :: Type).
  a ->
  Contract (OutputBus a) s e ()
sendBus = tell . OutputBus . Just . Last

--------------------------------------------------------------------------------
