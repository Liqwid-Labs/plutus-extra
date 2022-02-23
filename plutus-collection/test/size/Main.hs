{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

import Data.Kind (Type)
import Plutus.V1.Ledger.Scripts (fromCompiledCode)
import PlutusTx (CompiledCode)
import PlutusTx.AssocMap (Map)
import PlutusTx.AssocMap.Natural qualified as MapNat
import PlutusTx.List.Natural qualified as Nat
import PlutusTx.List.Ord qualified as Ord
import PlutusTx.Natural (Natural)
import PlutusTx.Prelude
import PlutusTx.TH (compile)
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.Plutus.Size (ByteSize, bytes, fitsInto)
import Prelude qualified

-- byte values are script sizes from c36555e35289c7aaabf62af8dccac65d9ab52c11
main :: Prelude.IO ()
main =
  defaultMain . testGroup "Size checks" $
    [ testGroup
        "List.Natural"
        [ runSizeTest "length" [bytes| 121 |] naturalLength
        , runSizeTest "take" [bytes| 173 |] naturalTake
        , runSizeTest "drop" [bytes| 168 |] naturalDrop
        , runSizeTest "splitAt" [bytes| 263 |] naturalSplitAt
        , runSizeTest "replicate" [bytes| 163 |] naturalReplicate
        ]
    , testGroup
        "List.Ord"
        [ runSizeTest "sort" [bytes| 467 |] ordSort
        , runSizeTest "sortOn" [bytes| 469 |] ordSortOn
        , runSizeTest "sortBy" [bytes| 266 |] ordSortBy
        , runSizeTest "ordNub" [bytes| 540 |] ordOrdNub
        , runSizeTest "ordNubBy" [bytes| 340 |] ordOrdNubBy
        , runSizeTest "isSorted" [bytes| 426 |] ordIsSorted
        , runSizeTest "isSortedOn" [bytes| 428 |] ordIsSortedOn
        , runSizeTest "isSortedAscending" [bytes| 426 |] ordIsSortedAscending
        , runSizeTest "isSortedAscendingOn" [bytes| 428 |] ordIsSortedAscendingOn
        , runSizeTest "isSortedBy" [bytes| 218 |] ordIsSortedBy
        ]
    , testGroup
        "AssocMap.Natural"
        [ runSizeTest "take" [bytes| 178 |] naturalMapTake
        , runSizeTest "drop" [bytes| 173 |] naturalMapDrop
        , runSizeTest "splitAt" [bytes| 290 |] naturalMapSplitAt
        ]
    ]
  where
    runSizeTest ::
      forall (a :: Type).
      Prelude.String ->
      ByteSize ->
      CompiledCode a ->
      TestTree
    runSizeTest scriptName maxSize =
      fitsInto scriptName maxSize . fromCompiledCode

{-# INLINEABLE naturalLength #-}
naturalLength :: CompiledCode ([Integer] -> Natural)
naturalLength = $$(compile [||Nat.length||])

{-# INLINEABLE naturalTake #-}
naturalTake :: CompiledCode (Natural -> [Integer] -> [Integer])
naturalTake = $$(compile [||Nat.take||])

{-# INLINEABLE naturalDrop #-}
naturalDrop :: CompiledCode (Natural -> [Integer] -> [Integer])
naturalDrop = $$(compile [||Nat.drop||])

{-# INLINEABLE naturalSplitAt #-}
naturalSplitAt :: CompiledCode (Natural -> [Integer] -> ([Integer], [Integer]))
naturalSplitAt = $$(compile [||Nat.splitAt||])

{-# INLINEABLE naturalReplicate #-}
naturalReplicate :: CompiledCode (Natural -> Integer -> [Integer])
naturalReplicate = $$(compile [||Nat.replicate||])

{-# INLINEABLE ordSort #-}
ordSort :: CompiledCode ([Integer] -> [Integer])
ordSort = $$(compile [||Ord.sort||])

{-# INLINEABLE ordSortOn #-}
ordSortOn :: CompiledCode ((Integer -> Integer) -> [Integer] -> [Integer])
ordSortOn = $$(compile [||Ord.sortOn||])

{-# INLINEABLE ordSortBy #-}
ordSortBy ::
  CompiledCode
    ( (Integer -> Integer -> Ordering) -> [Integer] -> [Integer]
    )
ordSortBy = $$(compile [||Ord.sortBy||])

{-# INLINEABLE ordOrdNub #-}
ordOrdNub :: CompiledCode ([Integer] -> [Integer])
ordOrdNub = $$(compile [||Ord.ordNub||])

{-# INLINEABLE ordOrdNubBy #-}
ordOrdNubBy ::
  CompiledCode
    ( (Integer -> Integer -> Ordering) -> [Integer] -> [Integer]
    )
ordOrdNubBy = $$(compile [||Ord.ordNubBy||])

{-# INLINEABLE ordIsSorted #-}
ordIsSorted :: CompiledCode ([Integer] -> Bool)
ordIsSorted = $$(compile [||Ord.isSorted||])

{-# INLINEABLE ordIsSortedAscending #-}
ordIsSortedAscending :: CompiledCode ([Integer] -> Bool)
ordIsSortedAscending = $$(compile [||Ord.isSortedAscending||])

{-# INLINEABLE ordIsSortedOn #-}
ordIsSortedOn :: CompiledCode ((Integer -> Integer) -> [Integer] -> Bool)
ordIsSortedOn = $$(compile [||Ord.isSortedOn||])

{-# INLINEABLE ordIsSortedAscendingOn #-}
ordIsSortedAscendingOn ::
  CompiledCode
    ( (Integer -> Integer) -> [Integer] -> Bool
    )
ordIsSortedAscendingOn = $$(compile [||Ord.isSortedAscendingOn||])

{-# INLINEABLE ordIsSortedBy #-}
ordIsSortedBy ::
  CompiledCode
    ( (Integer -> Integer -> Bool) -> [Integer] -> Bool
    )
ordIsSortedBy = $$(compile [||Ord.isSortedBy||])

{-# INLINEABLE naturalMapTake #-}
naturalMapTake :: CompiledCode (Natural -> Map Integer Integer -> Map Integer Integer)
naturalMapTake = $$(compile [||MapNat.take||])

{-# INLINEABLE naturalMapDrop #-}
naturalMapDrop :: CompiledCode (Natural -> Map Integer Integer -> Map Integer Integer)
naturalMapDrop = $$(compile [||MapNat.drop||])

{-# INLINEABLE naturalMapSplitAt #-}
naturalMapSplitAt :: CompiledCode (Natural -> Map Integer Integer -> (Map Integer Integer, Map Integer Integer))
naturalMapSplitAt = $$(compile [||MapNat.splitAt||])
