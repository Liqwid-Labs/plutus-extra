module PlutusTx.Skeleton.QQ (makeSkeletal) where

import Language.Haskell.TH (Dec, Q, Type)

makeSkeletal :: Type -> Q [Dec]
makeSkeletal = _
