{-# LANGUAGE AllowAmbiguousTypes #-}

module Test.Tasty.Plutus.Internal.DumpScript (
  DumpScript (DumpScript),
) where

import Data.Coerce (Coercible, coerce)
import Data.Kind (Type)
import Data.Proxy (Proxy (Proxy))
import Data.Tagged (Tagged (Tagged))
import Data.Text.IO qualified as Text
import Plutus.V1.Ledger.Scripts (Script (unScript))
import PlutusCore.Pretty qualified as PLC
import PlutusTx (CompiledCode, getPir, getPlc)
import System.FilePath ((</>))
import Test.Tasty.Options (
  IsOption,
  OptionDescription (Option),
  OptionSet,
  lookupOption,
 )
import Test.Tasty.Plutus.Internal.Options (
  DumpPath (DumpPath, NoDumpPath),
  DumpPlutusCore (DumpPlutusCore),
  DumpPlutusIR (DumpPlutusIR),
  DumpPlutusTypedCore (DumpPlutusTypedCore),
 )
import Test.Tasty.Providers (IsTest (run, testOptions), testPassed)

runIf ::
  forall (v :: Type).
  (IsOption v, Coercible v DumpPath) =>
  OptionSet ->
  (FilePath -> IO ()) ->
  IO ()
runIf opts action = case coerce (lookupOption @v opts) of
  DumpPath path -> action path
  NoDumpPath -> pure ()

data DumpScript = forall a. DumpScript String Script (CompiledCode a)

instance IsTest DumpScript where
  run opts (DumpScript nm scr code) _ = do
    runIf @DumpPlutusIR opts (dumpIR nm code)
    runIf @DumpPlutusTypedCore opts (dumpTypedCore nm code)
    runIf @DumpPlutusCore opts (dumpCore nm scr)
    pure $ testPassed ""
  testOptions =
    Tagged
      [ Option $ Proxy @DumpPlutusIR
      , Option $ Proxy @DumpPlutusTypedCore
      , Option $ Proxy @DumpPlutusCore
      ]

dumpIR :: String -> CompiledCode a -> FilePath -> IO ()
dumpIR nm code path = case getPir code of
  Nothing -> pure ()
  Just pir ->
    Text.writeFile
      (path </> (nm <> ".pir"))
      (PLC.render $ PLC.prettyClassicDebug pir)

dumpTypedCore :: String -> CompiledCode a -> FilePath -> IO ()
dumpTypedCore nm code path = do
  Text.writeFile
    (path </> (nm <> ".tplc"))
    (PLC.render $ PLC.prettyPlcReadableDebug $ getPlc code)

dumpCore :: String -> Script -> FilePath -> IO ()
dumpCore nm scr path = do
  Text.writeFile
    (path </> (nm <> ".uplc"))
    (PLC.render $ PLC.prettyPlcReadableDebug $ unScript scr)
