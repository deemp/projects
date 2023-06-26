{-# LANGUAGE QuasiQuotes #-}

module Main where

import Converter
import Data.String.Interpolate
import Data.Text.IO qualified as T

main :: IO ()
main = do
  putStrLn "Generating docs"
  let 
    convert :: FilePath -> FilePath -> IO ()
    convert hs md = T.readFile hs >>= T.writeFile [i|../docs/src/#{md}|] . (Hs `convertTo` Md) (def & indent ?~ "i" & dedent ?~ "d" & enable ?~ "E" & disable ?~ "D")
  mapM_
    (uncurry convert)
    [ ("optics-by-example/README.hs", "OpticsByExample.md")
    -- , ("developers-roadmap/README.hs", "DevelopersRoadmap.md")
    ]