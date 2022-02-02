{-# LANGUAGE AllowAmbiguousTypes #-}

module Test.Tasty.Plutus.Internal.DumpScript (
  dumpScript,
) where

import Data.Coerce (Coercible, coerce)
import Data.Kind (Type)
import Data.Proxy (Proxy (Proxy))
import Data.Tagged (Tagged (Tagged))
import Data.Text.IO qualified as Text
import Plutus.V1.Ledger.Scripts (
  MintingPolicy (getMintingPolicy),
  Script (unScript),
  Validator (getValidator),
 )
import PlutusCore.Pretty qualified as PLC
import PlutusTx (CompiledCode, getPir, getPlc)
import System.FilePath ((</>))
import Test.Plutus.ContextBuilder (Purpose)
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
import Test.Tasty.Plutus.Internal.TestScript (
  TestScript (TestMintingPolicy, TestValidator),
 )
import Test.Tasty.Providers (
  IsTest (run, testOptions),
  TestTree,
  singleTest,
  testPassed,
 )

dumpScript :: String -> TestScript p -> TestTree
dumpScript name ts = singleTest "Dumping scripts" (DumpScript name ts)

runIf ::
  forall (v :: Type).
  (IsOption v, Coercible v DumpPath) =>
  OptionSet ->
  (FilePath -> IO ()) ->
  IO ()
runIf opts action = case coerce (lookupOption @v opts) of
  DumpPath path -> action path
  NoDumpPath -> pure ()

data DumpScript = forall (p :: Purpose). DumpScript String (TestScript p)

getScript :: forall (p :: Purpose). TestScript p -> Script
getScript (TestValidator _ val) = getValidator val
getScript (TestMintingPolicy _ mp) = getMintingPolicy mp

data SomeCode = forall (a :: Type). SomeCode (CompiledCode a)

getCode :: forall (p :: Purpose). TestScript p -> SomeCode
getCode (TestValidator code _) = SomeCode code
getCode (TestMintingPolicy code _) = SomeCode code

instance IsTest DumpScript where
  run opts (DumpScript nm scr) _ = do
    runIf @DumpPlutusIR opts (dumpIR nm (getCode scr))
    runIf @DumpPlutusTypedCore opts (dumpTypedCore nm (getCode scr))
    runIf @DumpPlutusCore opts (dumpCore nm (getScript scr))
    pure $ testPassed ""
  testOptions =
    Tagged
      [ Option $ Proxy @DumpPlutusIR
      , Option $ Proxy @DumpPlutusTypedCore
      , Option $ Proxy @DumpPlutusCore
      ]

dumpIR :: String -> SomeCode -> FilePath -> IO ()
dumpIR nm (SomeCode code) path = case getPir code of
  Nothing -> pure ()
  Just pir ->
    Text.writeFile
      (path </> (nm <> ".pir"))
      (PLC.render $ PLC.prettyClassicDebug pir)

dumpTypedCore :: String -> SomeCode -> FilePath -> IO ()
dumpTypedCore nm (SomeCode code) path = do
  Text.writeFile
    (path </> (nm <> ".tplc"))
    (PLC.render $ PLC.prettyPlcReadableDebug $ getPlc code)

dumpCore :: String -> Script -> FilePath -> IO ()
dumpCore nm scr path = do
  Text.writeFile
    (path </> (nm <> ".uplc"))
    (PLC.render $ PLC.prettyPlcReadableDebug $ unScript scr)
