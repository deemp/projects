{-# LANGUAGE ImplicitParams #-}
{-# OPTIONS_GHC -fplugin Debug.Breakpoint #-}

module Main where

import Debug.Breakpoint (breakpointIO)

main :: IO ()
main = do
  putStrLn "Type something!"
  x <- getLine
  let y = 2 :: Int
      z = id :: Bool -> Bool
  breakpointIO
  pure ()

data C

f :: Bool -> C
f = undefined

-- так задаем
x :: C
x = let ?a = True; ?b = False in y

-- так используем
y :: (?b :: Bool, ?a :: Bool) => C
y = f ?a