module Test.Tasty.Plutus.Context (
  -- * Types
  InputType (..),
  OutputType (..),
  Input (..),
  Output (..),
  ContextBuilder,

  -- * Functions

  -- ** Basic construction
  input,
  output,
  signedWith,
  meta,

  -- ** Paying
  paysToPubKey,
  paysToWallet,
  paysSelf,
  paysOther,

  -- ** Spending
  spendsFromPubKey,
  spendsFromWallet,
  spendsFromPubKeySigned,
  spendsFromWalletSigned,
  spendsFromSelf,
  spendsFromOther,
) where

import Data.Functor.Compose (Compose (Compose))
import Data.Kind (Type)
import Data.Sequence (Seq)
import Data.Sequence qualified as Seq
import Ledger.Crypto (PubKeyHash, pubKeyHash)
import Ledger.Scripts (ValidatorHash)
import Ledger.Value (Value)
import PlutusTx.Builtins (BuiltinData)
import PlutusTx.IsData.Class (ToData (toBuiltinData))
import Wallet.Emulator.Types (Wallet, walletPubKey)

-- | @since 1.0
data InputType
  = -- | @since 1.0
    PubKeyInput PubKeyHash
  | -- | @since 1.0
    ScriptInput ValidatorHash BuiltinData
  | -- | This is in the order of \'datum, redeemer\'.
    --
    --   @since 1.0
    OwnInput BuiltinData BuiltinData
  deriving stock
    ( -- | @since 1.0
      Show
    )

-- | @since 1.0
data OutputType
  = -- | @since 1.0
    PubKeyOutput PubKeyHash
  | -- | @since 1.0
    ScriptOutput ValidatorHash BuiltinData
  | -- | @since 1.0
    OwnOutput BuiltinData
  deriving stock
    ( -- | @since 1.0
      Show
    )

-- | @since 1.0
data Input
  = -- | @since 1.0
    Input InputType Value
  deriving stock
    ( -- | @since 1.0
      Show
    )

-- | @since 1.0
data Output
  = -- | @since 1.0
    Output OutputType Value
  deriving stock
    ( -- | @since 1.0
      Show
    )

{- | The validator's context. This consists of 'Input's, 'Output's,
 'PubKeyHash'es and possibly other data of your choice.

 If you don't need any additional data, use 'Void' as the type parameter.

 @since 1.0
-}
newtype ContextBuilder (a :: Type)
  = ContextBuilder (Seq Input, Seq Output, Seq PubKeyHash, Seq a)
  deriving stock
    ( -- | @since 1.0
      Show
    )
  deriving
    ( -- | @since 1.0
      Semigroup
    , -- | @since 1.0
      Monoid
    )
    via (Seq Input, Seq Output, Seq PubKeyHash, Seq a)
  deriving
    ( -- | @since 1.0
      Functor
    , -- | @since 1.0
      Applicative
    )
    via (Compose ((,,,) (Seq Input) (Seq Output) (Seq PubKeyHash)) Seq)

{- | Single-input context.

 @since 1.0
-}
input :: forall (a :: Type). Input -> ContextBuilder a
input x = ContextBuilder (Seq.singleton x, mempty, mempty, mempty)

{- | Single-output context.

 @since 1.0
-}
output :: forall (a :: Type). Output -> ContextBuilder a
output x = ContextBuilder (mempty, Seq.singleton x, mempty, mempty)

{- | Context with one signature.

 @since 1.0
-}
signedWith :: forall (a :: Type). PubKeyHash -> ContextBuilder a
signedWith pkh = ContextBuilder (mempty, mempty, Seq.singleton pkh, mempty)

{- | Context with one piece of metadata.

 @since 1.0
-}
meta :: forall (a :: Type). a -> ContextBuilder a
meta x = ContextBuilder (mempty, mempty, mempty, Seq.singleton x)

-- | @since 1.0
paysToPubKey :: forall (a :: Type). PubKeyHash -> Value -> ContextBuilder a
paysToPubKey pkh = output . Output (PubKeyOutput pkh)

-- | @since 1.0
paysToWallet :: forall (a :: Type). Wallet -> Value -> ContextBuilder a
paysToWallet wallet = paysToPubKey (walletPubKeyHash wallet)

-- | @since 1.0
paysSelf ::
  forall (a :: Type) (b :: Type).
  (ToData a) =>
  Value ->
  a ->
  ContextBuilder b
paysSelf v dt = output . Output (OwnOutput . toBuiltinData $ dt) $ v

-- | @since 1.0
paysOther ::
  forall (a :: Type) (b :: Type).
  (ToData a) =>
  ValidatorHash ->
  Value ->
  a ->
  ContextBuilder b
paysOther hash v dt =
  output . Output (ScriptOutput hash . toBuiltinData $ dt) $ v

-- | @since 1.0
spendsFromPubKey ::
  forall (a :: Type).
  PubKeyHash ->
  Value ->
  ContextBuilder a
spendsFromPubKey pkh = input . Input (PubKeyInput pkh)

-- | @since 1.0
spendsFromPubKeySigned ::
  forall (a :: Type).
  PubKeyHash ->
  Value ->
  ContextBuilder a
spendsFromPubKeySigned pkh v = spendsFromPubKey pkh v <> signedWith pkh

-- | @since 1.0
spendsFromWallet ::
  forall (a :: Type).
  Wallet ->
  Value ->
  ContextBuilder a
spendsFromWallet wallet = spendsFromPubKey (walletPubKeyHash wallet)

-- | @since 1.0
spendsFromWalletSigned ::
  forall (a :: Type).
  Wallet ->
  Value ->
  ContextBuilder a
spendsFromWalletSigned wallet = spendsFromPubKeySigned (walletPubKeyHash wallet)

-- | @since 1.0
spendsFromSelf ::
  forall (datum :: Type) (redeemer :: Type) (a :: Type).
  (ToData datum, ToData redeemer) =>
  Value ->
  datum ->
  redeemer ->
  ContextBuilder a
spendsFromSelf v d r =
  input . Input (OwnInput (toBuiltinData d) . toBuiltinData $ r) $ v

-- | @since 1.0
spendsFromOther ::
  forall (datum :: Type) (a :: Type).
  (ToData datum) =>
  ValidatorHash ->
  Value ->
  datum ->
  ContextBuilder a
spendsFromOther hash v d =
  input . Input (ScriptInput hash . toBuiltinData $ d) $ v

-- Helpers

walletPubKeyHash :: Wallet -> PubKeyHash
walletPubKeyHash = pubKeyHash . walletPubKey
