{-# LANGUAGE MultiWayIf #-}

{- |
 Module: Test.Tasty.Plutus.Size
 Copyright: (C) MLabs 2021
 License: Apache 2.0
 Maintainer: Koz Ross <koz@mlabs.city>
 Portability: GHC only
 Stability: Experimental

 Provides a means of checking that a 'Script' fits into a given
 size, either based on a user-specified limit, or the inherent limit of
 on-chain size (as currently known). Also allows comparing the on-chain size
 of a target against a baseline.

 = Example usage

 > someRandomFunction :: CompiledCode (Integer -> BuiltinString)
 > someRandomFunction = $$(compile [|| somethingSomething ||]
 >
 > someRandomData :: CompiledCode FooDataType
 > someRandomData = $$(compile [|| Foo 123 ||])
 >
 > mySizeTests :: TestTree
 > mySizeTests = testGroup "Size" [
 >    fitsOnChain myScript,
 >    fitsInto [kbytes | 3 |] myOtherScript,
 >    fitsOnChain . fromCompiledCode $ someRandomFunction
 >    fitsInto [bytes | 1023 |] . fromCompiledCode $ someRandomData
 >    fitsUnder possiblyHugeThing smallerBaselineThing
 >    ...

 = Important notes

 Several common issues can arise when using this library. We list them here,
 along with solutions.

 == Staging restriction

 Anything you test must be defined /outside/ of the module in which you call
 'PlutusTx.TH.compile'. For example, if we have this:

 > doAThing :: Integer -> BuiltinString
 > doAThing = ...
 >
 > compiledDoAThing :: CompiledCode (Integer -> BuiltinString)
 > compiledDoAThing = $$(compile [|| doAThing ||])

 When we compile, we will see an error message about the \'staging
 restriction\' of GHC. To solve this, define @doAThing@ in a separate module
 to @compiledDoAThing@.

 == 'PlutusTx.Code.CompiledCode' with type variables

 Consider the situation below. Imagine we have this in a module @Foo@:

 > data Foo (a :: Type) = Foo ...
 >
 > $(makeLift ''Foo)

 Then, in another module, we have:

 > module Bar where
 >
 > import Foo (Foo (Foo))
 >
 > -- Will be a problem.
 > testFoo :: CompiledCode (Foo a)
 > testFoo = $$(compile [|| Foo ... ||])
 >
 > aSizeTest :: TestTree
 > aSizeTest = testGroup "Foo" [
 >    fitsOnChain "Foo" . fromCompiledCode $ testFoo,
 >    ...

 If you try to compile this, you will see an error complaining about an
 un-inlined type variable @a@. To fix this, \'concretify\' all type variables:

 > -- Do this instead.
 > testFooFixed :: CompiledCode (Foo Integer)
 > testFooFixed = $$(compile [|| Foo ... ||])

 It is important to be consistent in your choice of concrete type when doing
 this, especially if your tests are of /values/ (rather than functions), as
 otherwise, your results may not be comparable or indicative.

 == Expected failures

 Sometimes, you may have 'Script's whose size is too
 large to fit on-chain (or your specified limit), but you want to use this
 library anyway, either to future-proof your implementation or to determine
 what the size actually is.

 One way to achieve this is to indicate that a test /should/ fail,
 using the @tasty-expected-failure@ library. An example of use follows.

 > import Test.Tasty.ExpectedFailure (expectFail)
 >
 > myBrokenSizeTests :: TestTree
 > myBrokenSizeTests = testGroup "Some of these break" [
 >    expectFail . fitsOnChain "Too big" . fromCompiledCode $ something,
 >    fitsOnChain "But this is OK" . fromCompiledCode $ somethingElse,
 >    ...
-}
module Test.Tasty.Plutus.Size (
  -- * Test API
  fitsOnChain,
  fitsInto,
  fitsUnder,

  -- * Byte size helper type
  Internal.ByteSize,
  Internal.byteSizeInt,
  QQ.bytes,
  QQ.kbytes,

  -- * Test helpers
  validatorToScript,
) where

import Cardano.Api.Shelley (
  PlutusScript (PlutusScriptSerialised),
  PlutusScriptV1,
  serialiseToCBOR,
 )
import Codec.Serialise (serialise)
import Data.ByteString qualified as BSS
import Data.ByteString.Lazy qualified as BS
import Data.ByteString.Short qualified as SBS
import Data.Kind (Type)
import Data.Tagged (Tagged (Tagged))
import Ledger.Typed.Scripts (TypedValidator, validatorScript)
import Plutus.V1.Ledger.Scripts (Script, getValidator)
import Test.Tasty (TestTree)
import Test.Tasty.Plutus.Internal qualified as Internal
import Test.Tasty.Plutus.QQ qualified as QQ
import Test.Tasty.Providers (
  IsTest (run, testOptions),
  singleTest,
  testFailed,
  testPassed,
 )
import Text.PrettyPrint (
  Doc,
  Style,
  lineLength,
  renderStyle,
  style,
  (<+>),
 )
import Text.Show.Pretty (dumpDoc)
import Prelude hiding (divMod)

{- | Checks whether the given 'Script' fits on-chain
 given current limits.

 = Note

 If a 'Script' represents a function that takes
 arguments (such as a validator), this will only check the size of said
 function on-chain, /not/ said function's arguments.

 To assist with judging this, a successful test will also output the size.

 @since 1.0
-}
fitsOnChain ::
  String ->
  Script ->
  TestTree
fitsOnChain scriptName =
  singleTest (scriptName <> " fits on-chain") . FitsOnChain

{- | Checks whether the given 'Script' is no bigger
 than the given size limit.

 = Note

 If a 'Script' represents a function that takes
 arguments (such as a validator), this will only check the size of said
 function on-chain, /not/ said function's arguments.

 To assist with judging this, a successful test will also output the size.

 @since 1.0
-}
fitsInto ::
  String ->
  Internal.ByteSize ->
  Script ->
  TestTree
fitsInto scriptName maxSize =
  singleTest (scriptName <> " fits into " <> prettyByteSize maxSize)
    . FitsInto maxSize

{- | Checks whether the first 'Script' is not larger in its representation than
 the second 'Script'. This can be used to ensure that comparative script size
 is favourable for the same functionality.

 = Note

 If a 'Script' represents a function that takes arguments (such as a
 validator), this will only check the size of said function on-chain, /not/
 said function's arguments.

 To assist with judging this, a successful test will also output the size of
 each script, along with a percentage size of the first relative the second.

 @since 1.1
-}
fitsUnder ::
  String ->
  Script ->
  Script ->
  TestTree
fitsUnder testName s1 =
  singleTest ("On-chain size comparison for " <> testName) . FitsUnder s1

{- | A helper for converting a 'TypedValidator' into its underlying 'Script'.

 @since 1.0
-}
validatorToScript :: forall (a :: Type). TypedValidator a -> Script
validatorToScript = getValidator . validatorScript

-- Helpers

prettyByteSize :: Internal.ByteSize -> String
prettyByteSize (Internal.ByteSize n) = case n `quotRem` 1024 of
  (d, r) ->
    renderStyle ourStyle $
      if r == 0 then dumpDoc d <> "KiB" else dumpDoc n <> "B"

data FitTest
  = FitsOnChain Script
  | FitsInto Internal.ByteSize Script
  | FitsUnder Script Script

instance IsTest FitTest where
  run _ ft _ = pure $ case ft of
    FitsOnChain script ->
      let serializedSize = serialisedScriptSize script
          limit = Internal.byteSizeInt maxBound
       in case compare serializedSize limit of
            GT -> testFailed . produceSize $ serializedSize
            _ -> testPassed . produceSize $ serializedSize
    FitsInto (Internal.ByteSize maxSize) script ->
      let serializedSize = serialisedScriptSize script
          limit = fromIntegral maxSize
       in case compare serializedSize limit of
            GT -> testFailed . produceSize $ serializedSize
            _ -> testPassed . produceSize $ serializedSize
    FitsUnder script1 script2 ->
      let script1Size = serialisedScriptSize script1
          script2Size = serialisedScriptSize script2
       in case compare script1Size script2Size of
            GT -> testFailed . produceRelativeSizes script1Size $ script2Size
            _ -> testPassed . produceRelativeSizes script1Size $ script2Size
  testOptions = Tagged []

serialisedScriptSize :: Script -> Int
serialisedScriptSize =
  BSS.length
    . serialiseToCBOR
    . PlutusScriptSerialised @PlutusScriptV1
    . SBS.toShort
    . BS.toStrict
    . serialise

ourStyle :: Style
ourStyle = style {lineLength = 80}

produceRelativeSizes :: Int -> Int -> String
produceRelativeSizes size1 size2 =
  renderStyle ourStyle $
    renderSize size1 <+> "vs" <+> renderSize size2 <+> renderPercentDiff size1 size2

produceSize :: Int -> String
produceSize i = renderStyle ourStyle $ "Size:" <+> renderSize i

renderSize :: Int -> Doc
renderSize i = case i `quotRem` 1024 of
  (0, 0) -> "0B"
  (d, 0) -> dumpDoc d <> "KiB"
  (0, r) ->
    dumpDoc i <> "B"
      <> ( if
              | r <= 256 -> ""
              | r > 256 && r < 768 -> " (~0.5KiB)"
              | otherwise -> " (~1KiB)"
         )
  (d, r) ->
    dumpDoc i <> "B (~"
      <> ( if
              | r <= 256 -> dumpDoc d <> "KiB)"
              | r > 256 && r < 768 -> dumpDoc d <> ".5KiB)"
              | otherwise -> dumpDoc (d + 1) <> "KiB)"
         )

renderPercentDiff :: Int -> Int -> Doc
renderPercentDiff size1 size2 = "\n" <> go
  where
    go :: Doc
    go =
      let diff = size2 - size1
          ratio :: Double = fromIntegral diff / fromIntegral size2
          result :: Int = round (ratio * 100.0)
       in case signum result of
            0 -> "Same size"
            (-1) -> "~" <> dumpDoc (abs result) <> "% bigger"
            _ -> "~" <> dumpDoc result <> "% smaller"
