{-# LANGUAGE AllowAmbiguousTypes #-}

module Test.Tasty.Plutus.Golden.Internal (
  Config (..),
  Sample (..),
  SampleError (..),
  serializeSample,
  deserializeSample,
  sampleFileName,
  -- genSample,
  ourStyle,
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
import Data.Maybe (mapMaybe)
import Data.Vector (Vector)
import Text.PrettyPrint (Style (lineLength), style)
import Type.Reflection (Typeable, tyConModule, typeRep, typeRepTyCon)

data Config = Config
  { configSeed :: {-# UNPACK #-} !Int
  , configGoldenPath :: FilePath
  , configSampleSize :: {-# UNPACK #-} !Int
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

{-
genSample ::
  forall (a :: Type) (b :: Type).
  (a -> b) ->
  ReaderT (Config a) IO (Sample b)
genSample f = do
  rng <- asks configRng
  Generator g <- asks configGenerator
  sampleSize <- asks configSampleSize
  seed <- asks configSeed
  Sample seed <$> Vector.replicateM sampleSize (f <$> g rng)
-}

ourStyle :: Style
ourStyle = style {lineLength = 80}
