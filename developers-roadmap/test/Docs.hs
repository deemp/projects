{-# LANGUAGE ImportQualifiedPost #-}

import Converter (Format (Hs, Md), convertTo, def)
import Data.Text.IO qualified as T

main :: IO ()
main = T.readFile "README.hs" >>= T.writeFile "README.md" . (Hs `convertTo` Md) def