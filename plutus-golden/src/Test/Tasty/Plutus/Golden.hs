{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE RankNTypes #-}

module Test.Tasty.Plutus.Golden (
  Generator (..),
  goldenJSON,
  goldenData,
  goldenUnsafeData,
  goldenToSchema,
  goldenToArgument,
) where

import Control.Monad.Extra (ifM, unlessM)
import Data.Aeson (FromJSON, ToJSON (toJSON), Value, eitherDecodeFileStrict')
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as Strict
import Data.Kind (Type)
import Data.Maybe (mapMaybe)
import Data.Tagged (Tagged (Tagged))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding as Encoding
import Data.Vector qualified as Vector
import PlutusTx.IsData.Class (FromData, ToData, UnsafeFromData)
import Pretty.Diff qualified as Diff
import Schema qualified as Plutus
import System.Directory (
  createDirectoryIfMissing,
  doesDirectoryExist,
  doesFileExist,
  getCurrentDirectory,
 )
import System.FilePath ((<.>), (</>))
import System.Random.Stateful (
  IOGenM,
  StatefulGen,
  StdGen,
  mkStdGen,
  newIOGenM,
 )
import Test.Tasty (TestTree)
import Test.Tasty.Providers (
  IsTest (run, testOptions),
  Result,
  singleTest,
  testFailed,
  testPassed,
 )
import Text.PrettyPrint (
  Doc,
  Style (lineLength),
  renderStyle,
  style,
  text,
  ($+$),
  (<+>),
 )
import Type.Reflection (
  Typeable,
  tyConModule,
  tyConName,
  typeRep,
  typeRepTyCon,
 )

newtype Generator (a :: Type) = Generator
  { runGenerator :: forall (g :: Type) (m :: Type -> Type). (StatefulGen g m) => g -> m a
  }
  deriving stock (Functor)

instance Applicative Generator where
  {-# INLINEABLE pure #-}
  pure x = Generator (pure . const x)
  {-# INLINEABLE (<*>) #-}
  Generator fs <*> Generator xs = Generator $ \rng -> fs rng <*> xs rng

instance Monad Generator where
  {-# INLINEABLE (>>=) #-}
  Generator xs >>= f = Generator $ \rng -> xs rng >>= (($ rng) . runGenerator . f)

goldenJSON ::
  forall (a :: Type).
  (Typeable a, ToJSON a, FromJSON a) =>
  Generator a ->
  TestTree
goldenJSON = singleTest ("Golden JSON: " <> typeName @a) . GoldenJSON

goldenData ::
  forall (a :: Type).
  (Typeable a, FromData a, ToData a) =>
  Generator a ->
  TestTree
goldenData = singleTest ("Golden Data: " <> typeName @a) . GoldenData

goldenUnsafeData ::
  forall (a :: Type).
  (Typeable a, UnsafeFromData a, ToData a) =>
  Generator a ->
  TestTree
goldenUnsafeData = singleTest ("Golden UnsafeData: " <> typeName @a) . GoldenUnsafeData

goldenToSchema ::
  forall (a :: Type).
  (Typeable a, Plutus.ToSchema a) =>
  Generator a ->
  TestTree
goldenToSchema = singleTest ("Golden ToSchema: " <> typeName @a) . GoldenToSchema

goldenToArgument ::
  forall (a :: Type).
  (Typeable a, Plutus.ToArgument a) =>
  Generator a ->
  TestTree
goldenToArgument = singleTest ("Golden ToArgument: " <> typeName @a) . GoldenToArgument

-- Helpers

typeName :: forall (a :: Type). (Typeable a) => String
typeName = tyConName . typeRepTyCon $ typeRep @a

data GoldenTest (a :: Type) where
  GoldenJSON :: (ToJSON a, FromJSON a) => Generator a -> GoldenTest a
  GoldenData :: (FromData a, ToData a) => Generator a -> GoldenTest a
  GoldenUnsafeData :: (UnsafeFromData a, ToData a) => Generator a -> GoldenTest a
  GoldenToSchema :: (Plutus.ToSchema a) => Generator a -> GoldenTest a
  GoldenToArgument :: (Plutus.ToArgument a) => Generator a -> GoldenTest a

instance (Typeable a) => IsTest (GoldenTest a) where
  run _ gt _ = case gt of
    GoldenJSON gen -> doGoldenJSON gen
    GoldenData _ -> pure . testFailed $ "Unsupported for now"
    GoldenUnsafeData _ -> pure . testFailed $ "Unsupported for now"
    GoldenToSchema _ -> pure . testFailed $ "Unsupported for now"
    GoldenToArgument _ -> pure . testFailed $ "Unsupported for now"
  testOptions = Tagged []

doGoldenJSON ::
  forall (a :: Type).
  (Typeable a, ToJSON a) =>
  Generator a ->
  IO Result
doGoldenJSON (Generator f) = do
  rng <- newIOGenM . mkStdGen $ 123456
  cwd <- getCurrentDirectory
  let goldenPath = cwd </> "golden" </> "json"
  let goldenFilePath = goldenPath </> sampleFileName @a <.> "json"
  unlessM (doesDirectoryExist goldenPath) (createDirectoryIfMissing True goldenPath)
  sampleArray <- genSamples rng
  ifM
    (doesFileExist goldenFilePath)
    (loadAndCompareSamples goldenFilePath sampleArray)
    (writeSamplesToFile goldenFilePath sampleArray)
  where
    genSamples :: IOGenM StdGen -> IO Value
    genSamples rng = toJSON <$> Vector.replicateM numSamples (f rng)

writeSamplesToFile :: FilePath -> Value -> IO Result
writeSamplesToFile p vals = do
  BS.writeFile p . Strict.toStrict . encodePretty $ vals
  pure . testPassed . renderStyle ourStyle $
    "Generated sample file:" <+> text p

loadAndCompareSamples :: FilePath -> Value -> IO Result
loadAndCompareSamples fp vals = do
  result <- eitherDecodeFileStrict' fp
  pure $ case result of
    Left _ -> testFailed notSampleFile
    Right sampleVals -> case compare sampleVals vals of
      EQ -> testPassed ""
      _ -> testFailed . sampleMismatch $ sampleVals
  where
    notSampleFile :: String
    notSampleFile =
      renderStyle ourStyle $
        "Not a valid sample file."
          $+$ "Either it is not JSON, or is JSON, but not a JSON array."
          $+$ ""
          $+$ dumpFileLocation
    dumpFileLocation :: Doc
    dumpFileLocation = "Sample file location" $+$ text fp
    sampleMismatch :: Value -> String
    sampleMismatch sampleVals =
      renderStyle ourStyle $
        "Data did not match sample."
          $+$ ""
          $+$ "Diff"
          $+$ ""
          $+$ dumpJSONDiff sampleVals vals
          $+$ ""
          $+$ dumpFileLocation

dumpJSONDiff :: Value -> Value -> Doc
dumpJSONDiff expected actual =
  let renderedExpected = go expected
      renderedActual = go actual
   in text . T.unpack . Diff.pretty config renderedExpected $ renderedActual
  where
    go :: Value -> Text
    go = Encoding.decodeUtf8 . Strict.toStrict . encodePretty
    config :: Diff.Config
    config =
      Diff.Config
        { Diff.separatorText = Nothing
        , Diff.wrapping = Diff.NoWrap
        , Diff.multilineContext = Diff.Surrounding 2 "..."
        }

sampleFileName :: forall (a :: Type). (Typeable a) => FilePath
sampleFileName = mapMaybe go $ moduleName <> "." <> typeName @a
  where
    go :: Char -> Maybe Char
    go =
      pure . \case
        '.' -> '-'
        c -> c
    moduleName :: String
    moduleName = tyConModule . typeRepTyCon $ typeRep @a

numSamples :: Int
numSamples = 20

ourStyle :: Style
ourStyle = style {lineLength = 80}
