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
import Data.Aeson (
  ToJSON (toJSON),
  Value,
  eitherDecodeFileStrict',
  object,
  withObject,
  (.:),
 )
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.Aeson.Types (Parser)
import Data.Bifunctor (bimap)
import Data.Bitraversable (bitraverse)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as Lazy
import Data.Kind (Type)
import Data.Maybe (mapMaybe)
import Data.OpenApi (Schema)
import Data.OpenApi.Schema (ToSchema, toSchema)
import Data.Proxy (Proxy (Proxy))
import Data.Tagged (Tagged (Tagged))
import Data.Text (unpack)
import Data.Text.Encoding (decodeUtf8)
import Data.Vector qualified as Vector
import PlutusTx (
  Data (
    B,
    Constr,
    I,
    List,
    Map
  ),
 )
import PlutusTx.IsData.Class (ToData, toData)
import System.Directory (
  createDirectoryIfMissing,
  doesFileExist,
  getCurrentDirectory,
 )
import System.FilePath ((<.>), (</>))
import System.Random.SplitMix (SMGen)
import System.Random.Stateful (
  IOGenM,
  applyRandomGenM,
  newIOGenM,
 )
import Test.QuickCheck.Arbitrary (Arbitrary (arbitrary))
import Test.QuickCheck.Gen (Gen)
import Test.QuickCheck.Random (QCGen (QCGen), mkQCGen)
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
  Config (
    Config,
    configGenerator,
    configGoldenPath,
    configSampleSize,
    configSeed
  ),
  Sample (Sample, sampleData, sampleSeed),
  SampleError (NotJSON, NotSample),
  deserializeSample,
  genToGenerator,
  ourStyle,
  serializeSample,
 )
import Test.Tasty.Providers (
  IsTest (run, testOptions),
  Result,
  singleTest,
  testFailed,
  testPassed,
 )
import Test.Tasty.Runners (resultSuccessful)
import Text.PrettyPrint (
  Doc,
  int,
  renderStyle,
  text,
  ($+$),
  (<+>),
 )
import Text.Read (readMaybe)
import Text.Show.Pretty (ppDoc)
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
  singleTest ("Golden JSON (" <> typeName @a <> ")")
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
  singleTest ("Golden Data (" <> typeName @a <> ")")
    . GoldenData (fullyQualifiedName @a)

{- | Golden test the OpenAPI schematization of @a@ via its 'ToSchema' instance.

 @since 1.0
-}
goldenToSchema ::
  forall (a :: Type).
  (ToSchema a) =>
  TestTree
goldenToSchema =
  singleTest ("Golden ToSchema (" <> typeName @a <> ")")
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
        let gen = genToGenerator g
        runReaderT goJSON . Config seed filePath sampleSize $ gen
      GoldenData tyName g -> do
        let folderPath' = folderPath </> "data"
        createDirectoryIfMissing True folderPath'
        let filePath = folderPath' </> tyName <.> "json"
        let gen = genToGenerator g
        runReaderT goData . Config seed filePath sampleSize $ gen
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
  ReaderT (Config a) IO Result
goJSON = do
  fp <- asks configGoldenPath
  ifM
    (liftIO . doesFileExist $ fp)
    loadAndCheckJSON
    (genAndWriteSample toJSON)

goData ::
  forall (a :: Type).
  (ToData a) =>
  ReaderT (Config a) IO Result
goData = do
  fp <- asks configGoldenPath
  ifM
    (liftIO . doesFileExist $ fp)
    loadAndCheckData
    (genAndWriteSample (dataToValue . toData))

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
  ReaderT (Config a) IO Result
loadAndCheckJSON = do
  fp <- asks configGoldenPath
  result <- liftIO . deserializeSample pure $ fp
  case result of
    Left err -> testFailed <$> sampleError err
    Right sample -> checkJSON sample

loadAndCheckData ::
  forall (a :: Type).
  (ToData a) =>
  ReaderT (Config a) IO Result
loadAndCheckData = do
  fp <- asks configGoldenPath
  result <- liftIO . deserializeSample parseData $ fp
  case result of
    Left err -> testFailed <$> sampleError err
    Right sample -> checkData sample

genAndWriteSample ::
  forall (a :: Type).
  (a -> Value) ->
  ReaderT (Config a) IO Result
genAndWriteSample f = do
  seed <- asks configSeed
  sampleSize <- asks configSampleSize
  rng <- newIOGenM . (\(QCGen g) -> g) . mkQCGen $ seed
  sample <- Sample seed <$> Vector.replicateM sampleSize (go rng)
  fp <- asks configGoldenPath
  liftIO . serializeSample f fp $ sample
  pure . testPassed . renderStyle ourStyle $
    "Generated:" <+> text fp
  where
    go :: IOGenM SMGen -> ReaderT (Config a) IO a
    go rng = asks configGenerator >>= flip applyRandomGenM rng

parseData :: Value -> Parser Data
parseData = withObject "Data" $ \obj -> do
  tag <- obj .: "tag"
  case tag of
    "Constr" -> do
      ix <- obj .: "index"
      asVals <- obj .: "data"
      ds <- traverse parseData asVals
      pure . Constr ix $ ds
    "Map" -> do
      asVals <- obj .: "data"
      keyVals <- traverse (bitraverse parseData parseData) asVals
      pure . Map $ keyVals
    "List" -> do
      asVals <- obj .: "data"
      ds <- traverse parseData asVals
      pure . List $ ds
    "I" -> I <$> obj .: "data"
    "B" -> do
      asString <- obj .: "data"
      case readMaybe asString of
        Nothing -> fail $ "Not a valid ByteString: " <> asString
        Just bs -> pure . B $ bs
    _ -> fail $ "Not a valid tag: " <> tag

dataToValue :: Data -> Value
dataToValue =
  object . \case
    Constr ix ds ->
      [ ("tag", "Constr")
      , ("index", toJSON ix)
      , ("data", toJSON . fmap dataToValue $ ds)
      ]
    Map keyVals ->
      [ ("tag", "Map")
      , ("data", toJSON . fmap (bimap dataToValue dataToValue) $ keyVals)
      ]
    List ds ->
      [ ("tag", "List")
      , ("data", toJSON . fmap dataToValue $ ds)
      ]
    I i ->
      [ ("tag", "I")
      , ("data", toJSON i)
      ]
    B bs ->
      [ ("tag", "B")
      , ("data", toJSON . show $ bs)
      ]

loadAndCheckSchema ::
  forall (a :: Type).
  (ToSchema a) =>
  ReaderT FilePath IO Result
loadAndCheckSchema = do
  fp <- ask
  result <- liftIO . eitherDecodeFileStrict' $ fp
  case result of
    Left err ->
      pure . testFailed . renderStyle ourStyle $
        "Could not parse sample."
          $+$ ""
          $+$ dumpSamplePath fp
          $+$ ""
          $+$ "Error message"
          $+$ ""
          $+$ text err
    Right scm -> do
      let scm' = toSchema (Proxy @a)
      if scm == scm'
        then pure . testPassed $ ""
        else testFailed <$> schemaMismatch scm scm'

writeSchema ::
  forall (a :: Type).
  (ToSchema a) =>
  ReaderT FilePath IO Result
writeSchema = do
  let scm = toSchema (Proxy @a)
  fp <- ask
  liftIO . BS.writeFile fp . Lazy.toStrict . encodePretty . toJSON $ scm
  pure . testPassed . renderStyle ourStyle $
    "Generated:" <+> text fp

sampleError ::
  forall (a :: Type).
  SampleError ->
  ReaderT (Config a) IO String
sampleError err = do
  fp <- asks configGoldenPath
  pure . renderStyle ourStyle $ case err of
    NotJSON err' ->
      "Could not parse sample file as JSON."
        $+$ ""
        $+$ dumpSamplePath fp
        $+$ ""
        $+$ "Parse error"
        $+$ ""
        $+$ text err'
    NotSample err' ->
      "Given file is not a valid sample file."
        $+$ ""
        $+$ dumpSamplePath fp
        $+$ ""
        $+$ "Parse error"
        $+$ ""
        $+$ text err'

checkJSON ::
  forall (a :: Type).
  (ToJSON a) =>
  Sample Value ->
  ReaderT (Config a) IO Result
checkJSON sample = do
  let seed = sampleSeed sample
  rng <- newIOGenM . (\(QCGen g) -> g) . mkQCGen $ seed
  Vector.ifoldM (go rng) (testPassed "") . sampleData $ sample
  where
    go ::
      IOGenM SMGen ->
      Result ->
      Int ->
      Value ->
      ReaderT (Config a) IO Result
    go rng acc ix expected
      | resultSuccessful acc = do
        gen <- asks configGenerator
        actual <- toJSON <$> applyRandomGenM gen rng
        case compare expected actual of
          EQ -> pure acc
          _ -> testFailed <$> jsonMismatch ix expected actual
      | otherwise = pure acc

checkData ::
  forall (a :: Type).
  (ToData a) =>
  Sample Data ->
  ReaderT (Config a) IO Result
checkData sample = do
  let seed = sampleSeed sample
  rng <- newIOGenM . (\(QCGen g) -> g) . mkQCGen $ seed
  Vector.ifoldM (go rng) (testPassed "") . sampleData $ sample
  where
    go ::
      IOGenM SMGen ->
      Result ->
      Int ->
      Data ->
      ReaderT (Config a) IO Result
    go rng acc ix expected
      | resultSuccessful acc = do
        gen <- asks configGenerator
        actual <- toData <$> applyRandomGenM gen rng
        case compare expected actual of
          EQ -> pure acc
          _ -> testFailed <$> dataMismatch ix expected actual
      | otherwise = pure acc

dumpSamplePath :: FilePath -> Doc
dumpSamplePath fp = "Sample file location:" <+> text fp

schemaMismatch :: Schema -> Schema -> ReaderT FilePath IO String
schemaMismatch expected actual = do
  fp <- ask
  pure . renderStyle ourStyle $
    "Schema does not match."
      $+$ ""
      $+$ dumpSamplePath fp
      $+$ ""
      $+$ "Expected"
      $+$ ""
      $+$ ppDoc expected
      $+$ ""
      $+$ "Actual"
      $+$ ppDoc actual

jsonMismatch ::
  forall (a :: Type).
  Int ->
  Value ->
  Value ->
  ReaderT (Config a) IO String
jsonMismatch ix expected actual = do
  fp <- asks configGoldenPath
  pure . renderStyle ourStyle $
    "Sample" <+> int ix <+> "does not match."
      $+$ ""
      $+$ dumpSamplePath fp
      $+$ ""
      $+$ "Expected"
      $+$ ""
      $+$ go expected
      $+$ ""
      $+$ "Actual"
      $+$ go actual
  where
    go :: Value -> Doc
    go = text . unpack . decodeUtf8 . Lazy.toStrict . encodePretty

dataMismatch ::
  forall (a :: Type).
  Int ->
  Data ->
  Data ->
  ReaderT (Config a) IO String
dataMismatch ix expected actual = do
  fp <- asks configGoldenPath
  pure . renderStyle ourStyle $
    "Sample" <+> int ix <+> "does not match."
      $+$ ""
      $+$ dumpSamplePath fp
      $+$ ""
      $+$ "Expected"
      $+$ ""
      $+$ ppDoc expected
      $+$ ""
      $+$ "Actual"
      $+$ ppDoc actual
