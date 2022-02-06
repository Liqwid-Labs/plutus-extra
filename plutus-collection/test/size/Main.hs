{-# LANGUAGE TemplateHaskell #-}

import Plutus.V1.Ledger.Scripts (fromCompiledCode)
import PlutusTx (CompiledCode)
import PlutusTx.AssocMap (Map)
import PlutusTx.AssocMap.Natural qualified as MapNat
import PlutusTx.List.Natural qualified as Nat
import PlutusTx.List.Ord qualified as Ord
import PlutusTx.Natural (Natural)
import PlutusTx.Prelude
import PlutusTx.TH (compile)
import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.Plutus.Size (fitsOnChain)
import Prelude qualified

main :: Prelude.IO ()
main =
  defaultMain . testGroup "Size checks" $
    [ testGroup
        "List.Natural"
        [ fitsOnChain "length" . fromCompiledCode $ naturalLength
        , fitsOnChain "take" . fromCompiledCode $ naturalTake
        , fitsOnChain "drop" . fromCompiledCode $ naturalDrop
        , fitsOnChain "splitAt" . fromCompiledCode $ naturalSplitAt
        , fitsOnChain "replicate" . fromCompiledCode $ naturalReplicate
        ]
    , testGroup
        "List.Ord"
        [ fitsOnChain "sort" . fromCompiledCode $ ordSort
        , fitsOnChain "sortOn" . fromCompiledCode $ ordSortOn
        , fitsOnChain "sortBy" . fromCompiledCode $ ordSortBy
        , fitsOnChain "ordNub" . fromCompiledCode $ ordOrdNub
        , fitsOnChain "ordNubBy" . fromCompiledCode $ ordOrdNubBy
        , fitsOnChain "isSorted" . fromCompiledCode $ ordIsSorted
        , fitsOnChain "isSortedOn" . fromCompiledCode $ ordIsSortedOn
        , fitsOnChain "isSortedAscending" . fromCompiledCode $ ordIsSortedAscending
        , fitsOnChain "isSortedAscendingOn" . fromCompiledCode $ ordIsSortedAscendingOn
        , fitsOnChain "isSortedBy" . fromCompiledCode $ ordIsSortedBy
        ]
    , testGroup
        "AssocMap.Natural"
        [ fitsOnChain "take" . fromCompiledCode $ naturalMapTake
        , fitsOnChain "drop" . fromCompiledCode $ naturalMapDrop
        , fitsOnChain "splitAt" . fromCompiledCode $ naturalMapSplitAt
        ]
    ]

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
