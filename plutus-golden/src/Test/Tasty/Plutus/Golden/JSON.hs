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

ourStyle :: Style
ourStyle = style {lineLength = 80}
