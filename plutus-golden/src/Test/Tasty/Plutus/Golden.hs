{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE RankNTypes #-}

module Test.Tasty.Plutus.Golden (
  -- * Testing API
  goldenJSON,
  goldenJSONWith,
  goldenData,
  goldenDataWith,
  goldenToSchema,

  -- * Options
  GoldenSeed (..),
  GoldenPath (..),
  GoldenSampleSize,
) where

import Control.Monad.Extra (ifM)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader (ReaderT, ask, asks, runReaderT)
import Data.Aeson (ToJSON (toJSON), Value)
import Data.Kind (Type)
import Data.Maybe (mapMaybe)
import Data.OpenApi.Schema (ToSchema)
import Data.Proxy (Proxy (Proxy))
import Data.Tagged (Tagged (Tagged))
import PlutusTx (Data)
import PlutusTx.IsData.Class (ToData, toData)
import System.Directory (
  createDirectoryIfMissing,
  doesFileExist,
  getCurrentDirectory,
 )
import System.FilePath ((<.>), (</>))
import System.Random.Stateful (mkStdGen, newIOGenM)
import Test.QuickCheck.Arbitrary (Arbitrary (arbitrary))
import Test.QuickCheck.Gen (Gen)
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
import Test.Tasty.Plutus.Golden.Internal (
  Config (Config, configGoldenPath),
 )
import Test.Tasty.Providers (
  IsTest (run, testOptions),
  Result,
  singleTest,
 )
import Text.Read (readMaybe)
import Type.Reflection (
  Typeable,
  tyConModule,
  tyConName,
  typeRep,
  typeRepTyCon,
 )

{- | Golden test the JSON serialization of @a@ via its 'ToJSON' instance.

 @since 1.0
-}
goldenJSON ::
  forall (a :: Type).
  (Typeable a, Arbitrary a, ToJSON a) =>
  TestTree
goldenJSON = goldenJSONWith @a arbitrary

{- | As 'goldenJSON', but with explicit generator.

 @since 1.0
-}
goldenJSONWith ::
  forall (a :: Type).
  (Typeable a, ToJSON a) =>
  Gen a ->
  TestTree
goldenJSONWith =
  singleTest ("Golden JSON: " <> typeName @a)
    . GoldenJSON (fullyQualifiedName @a)

{- | Golden test the Plutus 'Data' serialization of @a@ via its 'ToData'
 instance.

 @since 1.0
-}
goldenData ::
  forall (a :: Type).
  (Typeable a, Arbitrary a, ToData a) =>
  TestTree
goldenData = goldenDataWith @a arbitrary

{- | As 'goldenData', but with explicit generator.

 @since 1.0
-}
goldenDataWith ::
  forall (a :: Type).
  (Typeable a, ToData a) =>
  Gen a ->
  TestTree
goldenDataWith =
  singleTest ("Golden Data: " <> typeName @a)
    . GoldenData (fullyQualifiedName @a)

{- | Golden test the OpenAPI schematization of @a@ via its 'ToSchema' instance.

 @since 1.0
-}
goldenToSchema ::
  forall (a :: Type).
  (ToSchema a) =>
  TestTree
goldenToSchema =
  singleTest ("Golden ToSchema: " <> typeName @a)
    . GoldenToSchema @a
    $ fullyQualifiedName @a

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

fullyQualifiedName :: forall (a :: Type). (Typeable a) => String
fullyQualifiedName = mapMaybe go $ moduleName <> "." <> typeName @a
  where
    go :: Char -> Maybe Char
    go =
      pure . \case
        '.' -> '-'
        c -> c
    moduleName :: String
    moduleName = tyConModule . typeRepTyCon $ typeRep @a

data GoldenTest (a :: Type) where
  GoldenJSON :: (ToJSON a) => String -> Gen a -> GoldenTest a
  GoldenData :: (ToData a) => String -> Gen a -> GoldenTest a
  GoldenToSchema :: (ToSchema a) => String -> GoldenTest a

instance (Typeable a) => IsTest (GoldenTest a) where
  run opts gt _ = do
    cwd <- getCurrentDirectory
    let folderPath = cwd </> goldenPath
    case gt of
      GoldenJSON tyName g -> do
        let folderPath' = folderPath </> "json"
        createDirectoryIfMissing True folderPath'
        let filePath = folderPath' </> tyName <.> "json"
        runReaderT (goJSON g) . Config seed filePath $ sampleSize
      GoldenData tyName g -> do
        let folderPath' = folderPath </> "data"
        createDirectoryIfMissing True folderPath'
        let filePath = folderPath' </> tyName <.> "json"
        runReaderT (goData g) . Config seed filePath $ sampleSize
      GoldenToSchema tyName -> do
        let folderPath' = folderPath </> "to-schema"
        createDirectoryIfMissing True folderPath'
        let filePath = folderPath' </> tyName <.> "json"
        runReaderT (goToSchema @a) filePath
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

goJSON ::
  forall (a :: Type).
  (ToJSON a) =>
  Gen a ->
  ReaderT Config IO Result
goJSON g = do
  fp <- asks configGoldenPath
  ifM
    (liftIO . doesFileExist $ fp)
    (loadAndCheckJSON g)
    (genAndWriteSample toJSON g)

goData ::
  forall (a :: Type).
  (ToData a) =>
  Gen a ->
  ReaderT Config IO Result
goData g = do
  fp <- asks configGoldenPath
  ifM
    (liftIO . doesFileExist $ fp)
    (loadAndCheckData g)
    (genAndWriteSample (dataToValue . toData) g)

goToSchema ::
  forall (a :: Type).
  (ToSchema a) =>
  ReaderT FilePath IO Result
goToSchema = do
  fp <- ask
  ifM
    (liftIO . doesFileExist $ fp)
    (loadAndCheckSchema @a)
    (writeSchema @a)

loadAndCheckJSON ::
  forall (a :: Type).
  (ToJSON a) =>
  Gen a ->
  ReaderT Config IO Result
loadAndCheckJSON g = _

loadAndCheckData ::
  forall (a :: Type).
  (ToData a) =>
  Gen a ->
  ReaderT Config IO Result
loadAndCheckData g = _

genAndWriteSample ::
  forall (a :: Type).
  (a -> Value) ->
  Gen a ->
  ReaderT Config IO Result
genAndWriteSample f gen = _

parseData :: _
parseData = _

dataToValue :: Data -> Value
dataToValue = _

loadAndCheckSchema ::
  forall (a :: Type).
  (ToSchema a) =>
  ReaderT FilePath IO Result
loadAndCheckSchema = _

writeSchema ::
  forall (a :: Type).
  (ToSchema a) =>
  ReaderT FilePath IO Result
writeSchema = _
