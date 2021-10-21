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
import Data.Aeson.Internal (
  IResult (IError, ISuccess),
  JSONPathElement (Index),
  ifromJSON,
 )
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as Strict
import Data.Kind (Type)
import Data.Maybe (mapMaybe)
import Data.Tagged (Tagged (Tagged))
import Data.TreeDiff.Class (ediff)
import Data.TreeDiff.Pretty (ppEditExpr, prettyPretty)
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import PlutusTx.IsData.Class (FromData, ToData, UnsafeFromData)
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
import Test.Tasty.Runners (resultSuccessful)
import Text.PrettyPrint (
  Style (lineLength),
  hang,
  int,
  quotes,
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
    genSamples :: IOGenM StdGen -> IO (Vector Value)
    genSamples rng = Vector.replicateM numSamples (toJSON <$> f rng)

writeSamplesToFile :: FilePath -> Vector Value -> IO Result
writeSamplesToFile p vals = do
  BS.writeFile p . Strict.toStrict . encodePretty $ vals
  pure . testPassed $ "Generated new sample file."

loadAndCompareSamples :: FilePath -> Vector Value -> IO Result
loadAndCompareSamples fp vals = do
  result <- eitherDecodeFileStrict' fp
  pure $ case result of
    Left _ -> testFailed notSampleFile
    Right val -> case ifromJSON val of
      IError jpath err -> testFailed . valDidNotParse jpath $ err
      ISuccess sampleVals ->
        Vector.ifoldl' go (testPassed "") . Vector.zip sampleVals $ vals
  where
    go :: Result -> Int -> (Value, Value) -> Result
    go acc ix (expected, actual)
      | resultSuccessful acc = case compare expected actual of
        EQ -> acc
        _ -> testFailed . sampleMismatch ix expected $ actual
      | otherwise = acc
    notSampleFile :: String
    notSampleFile =
      renderStyle ourStyle $
        quotes (text fp) <+> "is not a valid sample file."
          $+$ "Either it's not JSON, or is JSON, but not a JSON array."
    valDidNotParse :: [JSONPathElement] -> String -> String
    valDidNotParse jpath err = renderStyle ourStyle $ case jpath of
      Index ix : _ ->
        "Could not parse element" <+> (int ix <> ".")
          $+$ hang "Error message" 4 (text err)
      _ ->
        "Could not parse."
          $+$ hang "Error message" 4 (text err)
    sampleMismatch :: Int -> Value -> Value -> String
    sampleMismatch ix expected actual =
      renderStyle ourStyle $
        "Sample" <+> int ix <+> "did not match."
          $+$ hang "Diff" 4 (ppEditExpr prettyPretty . ediff expected $ actual)

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
