module Test.Tasty.Plutus.Golden.Data (doGoldenData) where

import Control.Monad.Extra (ifM)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader (ReaderT, asks)
import Data.Aeson (Value, object, toJSON, withObject, (.:))
import Data.Aeson.Types (Parser)
import Data.Bifunctor (bimap)
import Data.Bitraversable (bitraverse)
import Data.Function (on)
import Data.Kind (Type)
import Data.Vector qualified as Vector
import PlutusTx (Data (B, Constr, I, List, Map))
import PlutusTx.IsData.Class (ToData, toData)
import System.Directory (
  createDirectoryIfMissing,
  doesFileExist,
  getCurrentDirectory,
 )
import System.FilePath ((<.>), (</>))
import Test.Tasty.Plutus.Golden.Internal (
  Config (
    configGoldenPath,
    configTypeName
  ),
  Sample (
    sampleData,
    sampleSeed
  ),
  SampleError (
    NotJSON,
    NotSample
  ),
  deserializeSample,
  ourStyle,
  sampleFileName,
  serializeSample,
 )
import Test.Tasty.Providers (testFailed, testPassed)
import Test.Tasty.Runners (Result, resultSuccessful)
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
import Type.Reflection (Typeable)
import Prelude hiding (exp)

doGoldenData ::
  forall (a :: Type).
  (Typeable a, ToData a) =>
  ReaderT (Config a) IO Result
doGoldenData = do
  cwd <- liftIO getCurrentDirectory
  goldenPath <- asks configGoldenPath
  let folderPath = cwd </> goldenPath </> "data"
  tyName <- asks configTypeName
  let filePath = folderPath </> sampleFileName @a tyName <.> "json"
  liftIO . createDirectoryIfMissing True $ folderPath
  sample <- genSample toData
  ifM
    (liftIO . doesFileExist $ filePath)
    (loadAndCompareSamples filePath sample)
    (writeSampleToFile filePath sample)

-- Helpers

loadAndCompareSamples ::
  forall (a :: Type).
  FilePath ->
  Sample Data ->
  ReaderT (Config a) IO Result
loadAndCompareSamples fp sample = do
  result <- liftIO . deserializeSample dataParseJSON $ fp
  pure $ case result of
    Left err -> testFailed . sampleError $ err
    Right expected -> compareSamples fp expected sample
  where
    sampleError :: SampleError -> String
    sampleError err = renderStyle ourStyle $ case err of
      NotJSON err' ->
        "Could not parse sample file as JSON."
          $+$ ""
          $+$ dumpFileLocation fp
          $+$ ""
          $+$ "Parse error"
          $+$ ""
          $+$ text err'
      NotSample err' ->
        "Given file is not a valid sample file."
          $+$ ""
          $+$ dumpFileLocation fp
          $+$ ""
          $+$ "Parse error"
          $+$ ""
          $+$ text err'

compareSamples ::
  FilePath ->
  Sample Data ->
  Sample Data ->
  Result
compareSamples fp expected actual =
  case (compare `on` sampleSeed) expected actual of
    EQ -> case (compare `on` (Vector.length . sampleData)) expected actual of
      EQ ->
        let expVec = sampleData expected
            actVec = sampleData actual
         in Vector.ifoldl' go (testPassed "") . Vector.zip expVec $ actVec
      _ -> testFailed mismatchedSampleCounts
    _ -> testFailed mismatchedSeeds
  where
    go ::
      Result ->
      Int ->
      (Data, Data) ->
      Result
    go acc ix (ex, act)
      | resultSuccessful acc = case compare ex act of
        EQ -> acc
        _ -> testFailed . mismatchedSample ix ex $ act
      | otherwise = acc
    mismatchedSample ::
      Int ->
      Data ->
      Data ->
      String
    mismatchedSample ix exp act =
      renderStyle ourStyle $
        "Sample" <+> int ix <+> "does not match."
          $+$ ""
          $+$ dumpFileLocation fp
          $+$ ""
          $+$ "Expected"
          $+$ ""
          $+$ ppDoc exp
          $+$ ""
          $+$ "Actual"
          $+$ ""
          $+$ ppDoc act
    mismatchedSampleCounts :: String
    mismatchedSampleCounts =
      let expectedLen = Vector.length . sampleData $ expected
          actualLen = Vector.length . sampleData $ actual
       in renderStyle ourStyle $
            "Unexpected number of samples"
              $+$ ""
              $+$ dumpFileLocation fp
              $+$ "Expected:" <+> int expectedLen
              $+$ "Actual:" <+> int actualLen
    mismatchedSeeds :: String
    mismatchedSeeds =
      let expectedSeed = sampleSeed expected
          actualSeed = sampleSeed actual
       in renderStyle ourStyle $
            "Seeds do not match"
              $+$ ""
              $+$ dumpFileLocation fp
              $+$ "Expected:" <+> int expectedSeed
              $+$ "Actual:" <+> int actualSeed

writeSampleToFile ::
  forall (a :: Type).
  FilePath ->
  Sample Data ->
  ReaderT (Config a) IO Result
writeSampleToFile fp sample = do
  liftIO . serializeSample dataToJSON fp $ sample
  pure . testPassed . renderStyle ourStyle $
    "Generated:" <+> text fp

dataToJSON :: Data -> Value
dataToJSON =
  object . \case
    Constr ix ds ->
      [ ("tag", "Constr")
      , ("index", toJSON ix)
      , ("data", toJSON . fmap dataToJSON $ ds)
      ]
    Map keyVals ->
      [ ("tag", "Map")
      , ("data", toJSON . fmap (bimap dataToJSON dataToJSON) $ keyVals)
      ]
    List ds ->
      [ ("tag", "List")
      , ("data", toJSON . fmap dataToJSON $ ds)
      ]
    I i ->
      [ ("tag", "I")
      , ("data", toJSON i)
      ]
    B bs ->
      [ ("tag", "B")
      , ("data", toJSON . show $ bs)
      ]

dataParseJSON :: Value -> Parser Data
dataParseJSON = withObject "Data" $ \obj -> do
  t <- obj .: "tag"
  case t of
    "Constr" -> do
      ix <- obj .: "index"
      asValues <- obj .: "data"
      ds <- traverse dataParseJSON asValues
      pure . Constr ix $ ds
    "Map" -> do
      asValuePairs <- obj .: "data"
      keyVals <- traverse (bitraverse dataParseJSON dataParseJSON) asValuePairs
      pure . Map $ keyVals
    "List" -> do
      asValues <- obj .: "data"
      ds <- traverse dataParseJSON asValues
      pure . List $ ds
    "I" -> I <$> obj .: "data"
    "B" -> do
      asString <- obj .: "data"
      case readMaybe asString of
        Nothing -> error $ "Could not parse ByteString: " <> asString
        Just bs -> pure . B $ bs
    _ -> error $ "Not a valid tag: " <> t

dumpFileLocation :: FilePath -> Doc
dumpFileLocation fp = "Sample file location:" <+> text fp
