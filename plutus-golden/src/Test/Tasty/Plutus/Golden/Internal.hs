{-# LANGUAGE AllowAmbiguousTypes #-}

module Test.Tasty.Plutus.Golden.Internal (
  Config (..),
  Sample (..),
  SampleError (..),
  serializeSample,
  deserializeSample,
  ourStyle,
  genToGenerator,
) where

import Data.Aeson (
  Value,
  eitherDecodeFileStrict',
  object,
  toJSON,
  withObject,
  (.:),
 )
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.Aeson.Types (Parser, parseEither)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as Lazy
import Data.Kind (Type)
import Data.Vector (Vector)
import System.Random.SplitMix (SMGen, splitSMGen)
import Test.QuickCheck.Gen (Gen (MkGen))
import Test.QuickCheck.Random (QCGen (QCGen))
import Text.PrettyPrint (Style (lineLength), style)

data Config (a :: Type) = Config
  { configSeed :: {-# UNPACK #-} !Int
  , configGoldenPath :: FilePath
  , configSampleSize :: {-# UNPACK #-} !Int
  , configGenerator :: SMGen -> (a, SMGen)
  }

data Sample (a :: Type) = Sample
  { sampleSeed :: {-# UNPACK #-} !Int
  , sampleData :: {-# UNPACK #-} !(Vector a)
  }

data SampleError
  = NotJSON String
  | NotSample String
  deriving stock (Eq, Show)

serializeSample ::
  forall (a :: Type).
  (a -> Value) ->
  FilePath ->
  Sample a ->
  IO ()
serializeSample f fp sample = do
  let obj =
        object
          [ ("seed", toJSON . sampleSeed $ sample)
          , ("data", toJSON . fmap f . sampleData $ sample)
          ]
  BS.writeFile fp . Lazy.toStrict . encodePretty $ obj

deserializeSample ::
  forall (a :: Type).
  (Value -> Parser a) ->
  FilePath ->
  IO (Either SampleError (Sample a))
deserializeSample f fp = do
  result <- eitherDecodeFileStrict' fp
  pure $ case result of
    Left err -> Left . NotJSON $ err
    Right val -> case parseEither go val of
      Left err -> Left . NotSample $ err
      Right sample -> pure sample
  where
    go :: Value -> Parser (Sample a)
    go = withObject "Sample" $ \obj -> do
      sampleSeed' <- obj .: "seed"
      sampleData' <- obj .: "data" >>= traverse f
      pure . Sample sampleSeed' $ sampleData'

ourStyle :: Style
ourStyle = style {lineLength = 80}

genToGenerator ::
  forall (a :: Type).
  Gen a ->
  SMGen ->
  (a, SMGen)
genToGenerator (MkGen f) rng =
  let (used, passed) = splitSMGen rng
   in (f (QCGen used) 100, passed) -- QuickCheck default size
