{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Main (main) where

import Control.Monad.Fix (fix)
import Data.Binary (decode, encode)
import Data.ByteString qualified as BS
import Data.ByteString.Builder (intDec, toLazyByteString)
import Data.ByteString.Char8 qualified as BSC
import Data.ByteString.Lazy qualified as LBS
import Data.ByteString.Lazy.Char8 qualified as LBSC
import Data.Foldable (Foldable (..))
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.HashMap.Strict qualified as M
import GHC.IO.Handle.FD (withFile)
import GHC.IO.IOMode (IOMode (..))
import System.Random (newStdGen)
import System.Random.Stateful (Random (..))

nNumbers :: Int
nNumbers = 70 * 1024 * 1024

fname :: FilePath
fname = "tmp/file-1"

bounds :: (Int, Int)
bounds = (1, 10000)

-- | write generated numbers into a file
writeNumbers :: IO ()
writeNumbers = do
  print "generating numbers"
  g <- newStdGen
  let randomStream :: [Int] = randomRs bounds g
  LBS.writeFile fname $ LBSC.unwords (toLazyByteString . intDec <$> take nNumbers randomStream)

kb :: Int
kb = 1024

chunkSize :: Int
chunkSize = 16 * kb

type MyState = (M.HashMap Int Int, LBS.ByteString)

-- | count numbers while reading the file in fixed chunks
-- and inserting them into a map in one go
countNumbersChunks :: IO ()
countNumbersChunks = do
  print "counting numbers (chunks)"
  print . sum . fst
    =<< withFile
      fname
      ReadMode
      ( \h -> do
          fix
            ( \(ret :: MyState -> IO MyState) statePrev@(!quantities, unparsed) -> do
                chunk_ <- LBS.hGet h chunkSize
                let
                  newChunk = unparsed <> chunk_
                  stateNew =
                    foldl'
                      ( \(!qs, !unparsed_) (!y) ->
                          maybe
                            (qs, y)
                            (\(x_, _) -> (M.insertWith (+) x_ 1 qs, ""))
                            (LBSC.readInt y)
                      )
                      statePrev
                      (LBSC.words newChunk)

                (if LBS.null chunk_ then return else ret) stateNew
            )
            (M.empty, "")
      )

-- | count numbers using lazy bytestring's @readFile@
countNumbersReadFile :: IO ()
countNumbersReadFile = do
  print "counting numbers (readFile)"
  print
    . sum
    . M.fromListWith (+)
    . fmap (maybe undefined ((,1) . fst) . LBSC.readInt)
    . LBSC.words
    =<< LBS.readFile fname

main :: IO ()
main = do
  -- writeNumbers
  countNumbersChunks

-- countNumbersReadFile

-- -------
-- trying serialization

nums :: [Int]
nums = [1, 3, 4, 4, 5, 2, 5]

-- binary <= 0.8.9.1
-- binary isn't human-readable

s :: [Int]
s = decode . encode $ [1 :: Int, 2, 3]

-- >>> s
-- [1,2,3]

-- bytestring

tryConvertLBS' :: [Maybe (Int, LBSC.ByteString)]
tryConvertLBS' = LBSC.readInt <$> LBSC.words (LBSC.unwords (toLazyByteString . intDec <$> nums))

-- >>> tryConvertLBS'
-- [Just (1,""),Just (3,""),Just (4,""),Just (4,""),Just (5,""),Just (2,""),Just (5,"")]
