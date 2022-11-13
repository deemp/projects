{-# LANGUAGE QuasiQuotes #-}

module Inits (initPackageYaml, initStackYaml, initSimpleMain) where

import Data.ByteString (ByteString)
import Text.RawString.QQ (r)

initPackageYaml :: ByteString
initPackageYaml =
  [r|
default-extensions:
- TypeApplications

dependencies:
- base >= 4.7 && < 5
- sockets-and-pipes >= 0.3

executables:
  Templates.SimpleMain:
    main: SimpleMain.hs
    source-dirs: ./Templates

ghc-options:
- -Wall

license: BSD3

name: nix-managed

version: 0.1.0.0
|]

initStackYaml :: ByteString
initStackYaml =
  [r| 
resolver: nightly-2022-11-12

ghc-options:
  "$everything":
    -Wno-missing-signatures
    -Wno-unused-top-binds
    -haddock
packages:
  - .
extra-deps:
  - sockets-and-pipes-0.3
|]

initSimpleMain :: ByteString
initSimpleMain =
  [r|module Main where

main :: IO ()
main = print ("Hello, World!" :: String)
|]
