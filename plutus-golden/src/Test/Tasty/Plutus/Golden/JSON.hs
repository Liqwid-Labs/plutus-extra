{-# LANGUAGE AllowAmbiguousTypes #-}

module Test.Tasty.Plutus.Golden.JSON (doGoldenJSON) where

import Control.Monad.Extra (ifM, unlessM)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader (ReaderT, asks)
import Data.Aeson (
  ToJSON (toJSON),
  Value,
  eitherDecodeFileStrict',
 )
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as Lazy
import Data.Function (on)
import Data.Kind (Type)
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as Encoding
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import System.Directory (
  createDirectoryIfMissing,
  doesDirectoryExist,
  doesFileExist,
  getCurrentDirectory,
 )
import System.FilePath ((<.>), (</>))
import System.Random.Stateful (
  IOGenM,
  StdGen,
  mkStdGen,
  newIOGenM,
 )
import Test.Tasty.Plutus.Generator (Generator (Generator))
import Test.Tasty.Plutus.Golden.Internal (
  Config (
    configGenerator,
    configGoldenPath,
    configRng,
    configSampleSize,
    configSeed,
    configTypeName
  ),
  Sample (
    Sample,
    sampleData,
    sampleSeed
  ),
  SampleError (
    NotJSON,
    NotSample
  ),
  deserializeSample,
  serializeSample,
 )
import Test.Tasty.Providers (Result, testFailed, testPassed)
import Test.Tasty.Runners (resultSuccessful)
import Text.PrettyPrint (
  Doc,
  Style (lineLength),
  int,
  renderStyle,
  style,
  text,
  ($+$),
  (<+>),
 )
import Type.Reflection (
  Typeable,
  tyConModule,
  typeRep,
  typeRepTyCon,
 )

doGoldenJSON ::
  forall (a :: Type).
  (ToJSON a, Typeable a) =>
  ReaderT (Config a) IO Result
doGoldenJSON = do
  cwd <- liftIO getCurrentDirectory
  goldenPath <- asks configGoldenPath
  let folderPath = cwd </> goldenPath </> "json"
  tyName <- asks configTypeName
  let filePath = folderPath </> sampleFileName @a tyName <.> "json"
  liftIO . createDirectoryIfMissing True $ folderPath
  sample <- genJSONSample
  ifM
    (liftIO . doesFileExist $ filePath)
    (loadAndCompareSamples filePath sample)
    (writeSampleToFile filePath sample)

-- Helpers

-- We can easily have multiple types with the same name in the same package. To
-- avoid these clashing, we use fully qualified names, but with . replaced with
-- -, to avoid triggering bad filename behaviour.
sampleFileName :: forall (a :: Type). (Typeable a) => String -> FilePath
sampleFileName tyName = mapMaybe go $ moduleName <> "." <> tyName
  where
    go :: Char -> Maybe Char
    go =
      pure . \case
        '.' -> '-'
        c -> c
    moduleName :: String
    moduleName = tyConModule . typeRepTyCon $ typeRep @a

genJSONSample ::
  forall (a :: Type).
  (ToJSON a) =>
  ReaderT (Config a) IO (Sample Value)
genJSONSample = do
  rng <- asks configRng
  Generator f <- asks configGenerator
  sampleSize <- asks configSampleSize
  seed <- asks configSeed
  Sample seed <$> Vector.replicateM sampleSize (toJSON <$> f rng)

loadAndCompareSamples ::
  forall (a :: Type).
  FilePath ->
  Sample Value ->
  ReaderT (Config a) IO Result
loadAndCompareSamples fp sample = do
  result <- liftIO . deserializeSample pure $ fp
  case result of
    Left err -> pure . testFailed . sampleError $ err
    Right expected -> compareSamples fp expected sample

compareSamples ::
  forall (a :: Type).
  FilePath ->
  Sample Value ->
  Sample Value ->
  ReaderT (Config a) IO Result
compareSamples fp expected actual =
  case (compare `on` sampleSeed) expected actual of
    EQ -> case (compare `on` (Vector.length . sampleData)) expected actual of
      EQ -> do
        let expVec = sampleData expected
        let actVec = sampleData actual
        Vector.ifoldM (go fp) (testPassed "") . Vector.zip expVec $ actVec
      _ -> _
    _ -> _
  where
    go ::
      FilePath ->
      Result ->
      Int ->
      (Value, Value) ->
      ReaderT (Config a) IO Result
    go fp acc ix (ex, act) = _

sampleError :: SampleError -> String
sampleError err = renderStyle ourStyle _

writeSampleToFile ::
  FilePath ->
  Sample Value ->
  ReaderT (Config a) IO Result
writeSampleToFile fp sample = do
  liftIO . serializeSample toJSON fp $ sample
  pure . testPassed . renderStyle ourStyle $
    "Generated:" <+> text fp

ourStyle :: Style
ourStyle = style {lineLength = 80}

{-
doGoldenJSON ::
  forall (a :: Type).
  (Typeable a, ToJSON a) =>
  String ->
  Generator a ->
  IO Result
doGoldenJSON tyName (Generator f) = do
  rng <- newIOGenM . mkStdGen $ 123456
  cwd <- getCurrentDirectory
  let goldenPath = cwd </> "golden" </> "json"
  let goldenFilePath = goldenPath </> sampleFileName @a tyName <.> "json"
  unlessM (doesDirectoryExist goldenPath) (createDirectoryIfMissing True goldenPath)
  sampleArray <- genSamples rng
  ifM
    (doesFileExist goldenFilePath)
    (loadAndCompareSamples goldenFilePath sampleArray)
    (writeSamplesToFile goldenFilePath sampleArray)
  where
    genSamples :: IOGenM StdGen -> IO (Vector Value)
    genSamples rng = Vector.replicateM numSamples (toJSON <$> f rng)

sampleFileName :: forall (a :: Type). (Typeable a) => String -> FilePath
sampleFileName tyName = mapMaybe go $ moduleName <> "." <> tyName
  where
    go :: Char -> Maybe Char
    go =
      pure . \case
        '.' -> '-'
        c -> c
    moduleName :: String
    moduleName = tyConModule . typeRepTyCon $ typeRep @a

writeSamplesToFile :: FilePath -> Vector Value -> IO Result
writeSamplesToFile p vals = do
  BS.writeFile p . Lazy.toStrict . encodePretty $ vals
  pure . testPassed . renderStyle ourStyle $
    "Generated sample file:" <+> text p

loadAndCompareSamples :: FilePath -> Vector Value -> IO Result
loadAndCompareSamples fp vals = do
  result <- eitherDecodeFileStrict' fp
  pure $ case result of
    Left err -> testFailed . parseError $ err
    Right sampleVals ->
      Vector.ifoldl' go (testPassed "") . Vector.zip sampleVals $ vals
  where
    go :: Result -> Int -> (Value, Value) -> Result
    go acc ix (expected, actual)
      | resultSuccessful acc = case compare expected actual of
        EQ -> acc
        _ -> testFailed . mismatch ix expected $ actual
      | otherwise = acc
    parseError :: String -> String
    parseError err =
      renderStyle ourStyle $
        "Not a valid sample file."
          $+$ "Either this file wasn't JSON, or it wasn't a JSON array."
          $+$ ""
          $+$ dumpFileLocation
          $+$ ""
          $+$ "Error"
          $+$ ""
          $+$ text err
    mismatch :: Int -> Value -> Value -> String
    mismatch ix expected actual =
      renderStyle ourStyle $
        "Sample" <+> int ix <+> "did not match."
          $+$ ""
          $+$ dumpFileLocation
          $+$ ""
          $+$ "Expected"
          $+$ ""
          $+$ jsonToDoc expected
          $+$ ""
          $+$ "Actual"
          $+$ ""
          $+$ jsonToDoc actual
    dumpFileLocation :: Doc
    dumpFileLocation =
      "Sample file location"
        $+$ ""
        $+$ text fp

jsonToDoc :: Value -> Doc
jsonToDoc = text . T.unpack . renderJSON

renderJSON :: Value -> Text
renderJSON = Encoding.decodeUtf8 . Lazy.toStrict . encodePretty

numSamples :: Int
numSamples = 20

-}
