module Test.Tasty.Plutus.Internal (
  ourStyle,
) where

import Text.PrettyPrint (Style (lineLength), style)

ourStyle :: Style
ourStyle = style {lineLength = 80}
