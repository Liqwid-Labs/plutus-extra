module Test.Tasty.Plutus.Golden.ToSchema (doGoldenToSchema) where

import Control.Monad.Extra (ifM)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader (ReaderT, asks)
import Data.Aeson (ToJSON (toJSON), eitherDecodeFileStrict')
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as Lazy
import Data.Kind (Type)
import Data.OpenApi (Schema)
import Data.OpenApi.Schema (ToSchema, toSchema)
import Data.Proxy (Proxy (Proxy))
import System.Directory (
  createDirectoryIfMissing,
  doesFileExist,
  getCurrentDirectory,
 )
import System.FilePath ((<.>), (</>))
import Test.Tasty.Plutus.Golden.Internal (
  StaticConfig (
    staticConfigGoldenPath,
    staticConfigTypeName
  ),
  ourStyle,
  sampleFileName,
 )
import Test.Tasty.Providers (Result, testFailed, testPassed)
import Text.PrettyPrint (
  Doc,
  renderStyle,
  text,
  ($+$),
  (<+>),
 )
import Text.Show.Pretty (ppDoc)
import Prelude hiding (exp)

doGoldenToSchema ::
  forall (a :: Type).
  (ToSchema a) =>
  ReaderT (StaticConfig a) IO Result
doGoldenToSchema = do
  cwd <- liftIO getCurrentDirectory
  goldenPath <- asks staticConfigGoldenPath
  let folderPath = cwd </> goldenPath </> "to-schema"
  tyName <- asks staticConfigTypeName
  let filePath = folderPath </> sampleFileName @a tyName <.> "json"
  liftIO . createDirectoryIfMissing True $ folderPath
  let scm = toSchema (Proxy @a)
  ifM
    (liftIO . doesFileExist $ filePath)
    (loadAndCompare filePath scm)
    (writeToFile filePath scm)

loadAndCompare ::
  forall (a :: Type).
  FilePath ->
  Schema ->
  ReaderT (StaticConfig a) IO Result
loadAndCompare fp scm = do
  result <- liftIO . eitherDecodeFileStrict' $ fp
  pure $ case result of
    Left err -> testFailed . notSchema $ err
    Right expected ->
      if expected == scm
        then testPassed ""
        else testFailed . mismatch $ expected
  where
    notSchema :: String -> String
    notSchema err =
      renderStyle ourStyle $
        "Could not parse JSON as Schema."
          $+$ ""
          $+$ dumpFileLocation fp
          $+$ ""
          $+$ "Parse error"
          $+$ ""
          $+$ text err
    mismatch :: Schema -> String
    mismatch expected =
      renderStyle ourStyle $
        "Sample did not match."
          $+$ ""
          $+$ dumpFileLocation fp
          $+$ ""
          $+$ "Expected"
          $+$ ""
          $+$ ppDoc expected
          $+$ ""
          $+$ "Actual"
          $+$ ""
          $+$ ppDoc scm

writeToFile ::
  forall (a :: Type).
  FilePath ->
  Schema ->
  ReaderT (StaticConfig a) IO Result
writeToFile fp scm = do
  liftIO . BS.writeFile fp . Lazy.toStrict . encodePretty . toJSON $ scm
  pure . testPassed . renderStyle ourStyle $
    "Generated:" <+> text fp

dumpFileLocation :: FilePath -> Doc
dumpFileLocation fp = "Sample file location:" <+> text fp
