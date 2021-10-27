{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE RankNTypes #-}

module Test.Tasty.Plutus.Golden (
  -- * Testing API
  goldenJSON,
  goldenData,

  -- * Options
  GoldenSeed (..),
  GoldenPath (..),
  GoldenSampleSize,
) where

import Test.Tasty.Plutus.Golden.Data (doGoldenData)
import Control.Monad.Trans.Reader (runReaderT)
import Data.Aeson (ToJSON)
import Data.Kind (Type)
import Data.Proxy (Proxy (Proxy))
import Data.Tagged (Tagged (Tagged))
import PlutusTx.IsData.Class (ToData)
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
import Test.Tasty.Plutus.Golden.Internal (Config (Config))
import Test.Tasty.Plutus.Golden.JSON (doGoldenJSON)
import Test.Tasty.Providers (
  IsTest (run, testOptions),
  singleTest,
 )
import Text.Read (readMaybe)
import Type.Reflection (
  Typeable,
  tyConName,
  typeRep,
  typeRepTyCon,
 )

-- | Golden test the JSON serialization of @a@ via its 'ToJSON' instance.
--
-- @since 1.0
goldenJSON ::
  forall (a :: Type).
  (Typeable a, ToJSON a) =>
  Generator a ->
  TestTree
goldenJSON = singleTest ("Golden JSON: " <> tyName) . GoldenJSON tyName
  where
    tyName :: String
    tyName = typeName @a

-- | Golden test the Plutus 'Data' serialization of @a@ via its 'ToData'
-- instance.
--
-- @since 1.0
goldenData ::
  forall (a :: Type).
  (Typeable a, ToData a) =>
  Generator a ->
  TestTree
goldenData = singleTest ("Golden Data: " <> tyName) . GoldenData tyName
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
  GoldenJSON :: (ToJSON a) => String -> Generator a -> GoldenTest a
  GoldenData :: (ToData a) => String -> Generator a -> GoldenTest a

instance (Typeable a) => IsTest (GoldenTest a) where
  run opts gt _ = do
    rng <- newIOGenM . mkStdGen $ seed
    case gt of
      GoldenJSON tyName gen -> do
        let conf = Config tyName seed rng gen goldenPath sampleSize
        runReaderT doGoldenJSON conf
      GoldenData tyName gen -> do
        let conf = Config tyName seed rng gen goldenPath sampleSize
        runReaderT doGoldenData conf
      GoldenToSchema tyName gen -> do
        let conf = Config tyName seed rng gen goldenPath sampleSize
        runReaderT doGoldenToSchema conf
    where
      seed :: Int
      GoldenSeed seed = lookupOption opts
      goldenPath :: FilePath
      GoldenPath goldenPath = lookupOption opts
      sampleSize :: Int
      GoldenSampleSize sampleSize = lookupOption opts
  testOptions =
    Tagged
      [ Option (Proxy @GoldenSeed)
      , Option (Proxy @GoldenPath)
      , Option (Proxy @GoldenSampleSize)
      ]
