{-# LANGUAGE NoImplicitPrelude #-}

module PlutusTx.Skeleton.String (
  intToString,
  bsToString,
) where

import PlutusTx.Prelude
import PlutusTx.Ratio (abs)

-- Slow digit-by-digit converter, to make _sure_ we're safe on-chain.
{-# INLINEABLE intToString #-}
intToString :: Integer -> BuiltinString
intToString i
  | i < zero = "-" <> (go . abs $ i)
  | i == zero = "0"
  | otherwise = go i
  where
    go :: Integer -> BuiltinString
    go arg =
      let (q, r) = arg `quotRem` 10
       in if q == zero
            then digitToString r
            else intToString q <> digitToString r

-- We render bytestrings as their individual code points, with backslashes.
bsToString :: BuiltinByteString -> BuiltinString
bsToString bbs =
  let len = lengthOfByteString bbs
   in if len == zero
        then ""
        else
          let byte = indexByteString bbs zero
           in "\\" <> intToString byte <> (bsToString . sliceByteString 1 (len - 1) $ bbs)

-- This is unsafe, but only ever called internally.
{-# INLINEABLE digitToString #-}
digitToString :: Integer -> BuiltinString
digitToString i
  | i == zero = "0"
  | i == one = "1"
  | i == 2 = "2"
  | i == 3 = "3"
  | i == 4 = "4"
  | i == 5 = "5"
  | i == 6 = "6"
  | i == 7 = "7"
  | i == 8 = "8"
  | otherwise = "9"
