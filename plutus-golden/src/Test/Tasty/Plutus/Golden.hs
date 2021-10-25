{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE RankNTypes #-}

module Test.Tasty.Plutus.Golden (
  -- * Testing API
  goldenJSON,
  goldenData,
  goldenUnsafeData,
  goldenToSchema,
  goldenToArgument,

  -- * Options
  GoldenSeed (..),
  GoldenPath (..),
  GoldenSampleSize,
) where

import Control.Monad.Trans.Reader (runReaderT)
import Data.Aeson (FromJSON, ToJSON)
import Data.Kind (Type)
import Data.Proxy (Proxy (Proxy))
import Data.Tagged (Tagged (Tagged))
import PlutusTx.IsData.Class (FromData, ToData, UnsafeFromData)
import Schema qualified as Plutus
import System.Random.Stateful (mkStdGen, newIOGenM)
import Test.Tasty (TestTree)
import Test.Tasty.Options (
  IsOption (
    defaultValue,
    optionHelp,
    optionName,
    parseValue,
    showDefaultValue
  ),
  OptionDescription (Option),
  lookupOption,
 )
import Test.Tasty.Plutus.Generator (Generator)
import Test.Tasty.Plutus.Golden.Config (Config (Config))
import Test.Tasty.Plutus.Golden.JSON (doGoldenJSON)
import Test.Tasty.Providers (
  IsTest (run, testOptions),
  singleTest,
  testFailed,
 )
import Text.Read (readMaybe)
import Type.Reflection (
  Typeable,
  tyConName,
  typeRep,
  typeRepTyCon,
 )

goldenJSON ::
  forall (a :: Type).
  (Typeable a, ToJSON a, FromJSON a) =>
  Generator a ->
  TestTree
goldenJSON = singleTest ("Golden JSON: " <> tyName) . GoldenJSON tyName
  where
    tyName :: String
    tyName = typeName @a

goldenData ::
  forall (a :: Type).
  (Typeable a, FromData a, ToData a) =>
  Generator a ->
  TestTree
goldenData = singleTest ("Golden Data: " <> tyName) . GoldenData tyName
  where
    tyName :: String
    tyName = typeName @a

goldenUnsafeData ::
  forall (a :: Type).
  (Typeable a, UnsafeFromData a, ToData a) =>
  Generator a ->
  TestTree
goldenUnsafeData =
  singleTest ("Golden UnsafeData: " <> tyName) . GoldenUnsafeData tyName
  where
    tyName :: String
    tyName = typeName @a

goldenToSchema ::
  forall (a :: Type).
  (Typeable a, Plutus.ToSchema a) =>
  Generator a ->
  TestTree
goldenToSchema =
  singleTest ("Golden ToSchema: " <> tyName) . GoldenToSchema tyName
  where
    tyName :: String
    tyName = typeName @a

goldenToArgument ::
  forall (a :: Type).
  (Typeable a, Plutus.ToArgument a) =>
  Generator a ->
  TestTree
goldenToArgument =
  singleTest ("Golden ToArgument: " <> tyName) . GoldenToArgument tyName
  where
    tyName :: String
    tyName = typeName @a

{- | The seed used for generating samples.

 Defaults to @123456@.

 @since 1.0
-}
newtype GoldenSeed = GoldenSeed Int
  deriving
    ( -- | @since 1.0
      Eq
    )
    via Int
  deriving stock
    ( -- | @since 1.0
      Show
    )

-- | @since 1.0
instance IsOption GoldenSeed where
  defaultValue = GoldenSeed 123456
  parseValue = fmap GoldenSeed . readMaybe
  optionName = Tagged "golden-seed"
  optionHelp = Tagged "The seed to use for golden tests."
  showDefaultValue (GoldenSeed i) = pure . show $ i

{- | The directory in which you want the golden files placed.

 Defaults to @golden@. This directory will /always/ be relative @$CWD@.

 @since 1.0
-}
newtype GoldenPath = GoldenPath FilePath
  deriving
    ( -- | @since 1.0
      Eq
    )
    via FilePath
  deriving stock
    ( -- | @since 1.0
      Show
    )

-- | @since 1.0
instance IsOption GoldenPath where
  defaultValue = GoldenPath "golden"
  parseValue = pure . GoldenPath
  optionName = Tagged "golden-path"
  optionHelp =
    Tagged "The (relative to $CWD) directory where golden files will go."
  showDefaultValue (GoldenPath p) = pure p

{- | The number of samples for each test.

 Defaults to 20. Values below 1 will not parse.

 @since 1.0
-}
newtype GoldenSampleSize = GoldenSampleSize Int
  deriving
    ( -- | @since 1.0
      Eq
    )
    via Int
  deriving stock
    ( -- | @since 1.0
      Show
    )

-- | @since 1.0
instance IsOption GoldenSampleSize where
  defaultValue = GoldenSampleSize 20
  parseValue s = readMaybe s >>= go
    where
      go :: Int -> Maybe GoldenSampleSize
      go i = case signum i of
        1 -> Just . GoldenSampleSize $ i
        _ -> Nothing
  optionName = Tagged "golden-sample-size"
  optionHelp = Tagged "The number of samples for each test (min 1)."
  showDefaultValue (GoldenSampleSize i) = Just . show $ i

-- Helpers

typeName :: forall (a :: Type). (Typeable a) => String
typeName = tyConName . typeRepTyCon $ typeRep @a

data GoldenTest (a :: Type) where
  GoldenJSON :: (ToJSON a, FromJSON a) => String -> Generator a -> GoldenTest a
  GoldenData :: (FromData a, ToData a) => String -> Generator a -> GoldenTest a
  GoldenUnsafeData :: (UnsafeFromData a, ToData a) => String -> Generator a -> GoldenTest a
  GoldenToSchema :: (Plutus.ToSchema a) => String -> Generator a -> GoldenTest a
  GoldenToArgument :: (Plutus.ToArgument a) => String -> Generator a -> GoldenTest a

instance (Typeable a) => IsTest (GoldenTest a) where
  run opts gt _ = do
    rng <- newIOGenM . mkStdGen $ seed
    case gt of
      GoldenJSON tyName gen -> do
        let conf = Config tyName seed rng goldenPath sampleSize
        runReaderT doGoldenJSON conf
      _ -> pure . testFailed $ "Unsupported for now"
    where
      seed :: Int
      GoldenSeed seed = lookupOption opts
      goldenPath :: FilePath
      GoldenPath goldenPath = lookupOption opts
      sampleSize :: Int
      GoldenSampleSize sampleSize = lookupOption opts

  {-
    case gt of
      GoldenJSON tyName gen -> _ -- doGoldenJSON tyName gen
      GoldenData _ _ -> pure . testFailed $ "Unsupported for now"
      GoldenUnsafeData _ _ -> pure . testFailed $ "Unsupported for now"
      GoldenToSchema _ _ -> pure . testFailed $ "Unsupported for now"
      GoldenToArgument _ _ -> pure . testFailed $ "Unsupported for now"
  -}
  testOptions =
    Tagged
      [ Option (Proxy @GoldenSeed)
      , Option (Proxy @GoldenPath)
      , Option (Proxy @GoldenSampleSize)
      ]
