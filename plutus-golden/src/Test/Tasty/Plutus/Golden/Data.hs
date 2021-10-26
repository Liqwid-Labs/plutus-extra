module Test.Tasty.Plutus.Golden.Data (doGoldenData) where

import Codec.Serialise.Class qualified as CBOR
import Control.Monad.Extra (ifM)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader (ReaderT, asks)
import Data.Kind (Type)
import Data.Vector qualified as Vector
import PlutusTx (Data)
import PlutusTx.IsData.Class (ToData, toData)
import System.Directory (
  createDirectoryIfMissing,
  doesFileExist,
  getCurrentDirectory,
 )
import System.FilePath ((<.>), (</>))
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
  genSample,
  ourStyle,
  sampleFileName,
  serializeSample,
 )
import Test.Tasty.Providers (testPassed)
import Test.Tasty.Runners (Result)
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
import Type.Reflection (Typeable)

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
loadAndCompareSamples fp sample = _

writeSampleToFile ::
  forall (a :: Type).
  FilePath ->
  Sample Data ->
  ReaderT (Config a) IO Result
writeSampleToFile fp sample = do
  liftIO . serializeSample (_ . CBOR.encode) fp $ sample
  pure . testPassed . renderStyle ourStyle $
    "Generated:" <+> text fp
