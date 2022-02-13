{-# LANGUAGE Trustworthy #-}

{- |
 Module: Witherable.Natural
 Copyright: (C) MLabs 2022
 License: Apache 2.0
 Maintainer: Xiaoyan Ren <xiaoyan@mlabs.city>
 Portability: GHC only
 Stability: Experimental
 Functions related to sorting lists.
-}
module Witherable.Natural (
  takeW,
  dropW,
) where

import Control.Monad.Trans.State (evalState, state)
import PlutusTx.Natural (Natural)
import PlutusTx.Prelude
import Witherable (Witherable (wither))
import Prelude ()

{- | @'takeW' n xs@ returns the first @n@ elements of any 'Witherable' @xs@, or
@xs@ itself if @n@ exceeds the size of @xs@. This has a time complexity of
\( O(size) \).

@since 3.0
-}
takeW :: Witherable t => Natural -> t a -> t a
takeW n =
  flip evalState zero
    . wither
      ( \x ->
          state $ \s -> (if s >= n then Nothing else Just x, succ s)
      )

{- | @'dropW' n xs@ returns the remainder after removing @n@ elements from any
'Witherable' @xs@, or an empty collection if @n@ exceeds the size of @xs@. This
has a time complexity of \( O(size) \).

@since 3.0
-}
dropW :: Witherable t => Natural -> t a -> t a
dropW n =
  flip evalState zero
    . wither
      ( \x ->
          state $ \s -> (if s < n then Nothing else Just x, succ s)
      )
