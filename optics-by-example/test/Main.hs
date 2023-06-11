{-# LANGUAGE QuasiQuotes #-}

import Converter
import Data.String.Interpolate (i)
import Data.Text.IO qualified as T

main :: IO ()
main = do
  prefix <- T.readFile "README/Prefix.md"
  body <- T.readFile "README.hs"
  let 
    convert = (Hs `convertTo` Md) (def & indent ?~ "i" & dedent ?~ "d" & enable ?~ "E" & disable ?~ "D")
    full = [i|#{prefix}\n#{convert body}|]
  T.writeFile "README.md" full