{-# LANGUAGE AllowAmbiguousTypes #-}

module Test.Tasty.Plutus.Golden.JSON (doGoldenJSON) where

import Control.Monad.Extra (ifM, unlessM)
import Data.Aeson (
  ToJSON (toJSON),
  Value,
  eitherDecodeFileStrict',
 )
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as Lazy
import Data.Kind (Type)
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as Encoding
import Data.Vector qualified as Vector
import Pretty.Diff qualified as Diff
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
import Test.Tasty.Providers (Result, testFailed, testPassed)
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

writeSamplesToFile :: FilePath -> Value -> IO Result
writeSamplesToFile p vals = do
  BS.writeFile p . Lazy.toStrict . encodePretty $ vals
  pure . testPassed . renderStyle ourStyle $
    "Generated sample file:" <+> text p

numSamples :: Int
numSamples = 20

ourStyle :: Style
ourStyle = style {lineLength = 80}

typeName :: forall (a :: Type). (Typeable a) => String
typeName = tyConName . typeRepTyCon $ typeRep @a

dumpJSONDiff :: Value -> Value -> Doc
dumpJSONDiff expected actual =
  let renderedExpected = go expected
      renderedActual = go actual
   in text . T.unpack . Diff.pretty config renderedExpected $ renderedActual
  where
    go :: Value -> Text
    go = Encoding.decodeUtf8 . Lazy.toStrict . encodePretty
    config :: Diff.Config
    config =
      Diff.Config
        { Diff.separatorText = Nothing
        , Diff.wrapping = Diff.NoWrap
        , Diff.multilineContext = Diff.Surrounding 2 "..."
        }
