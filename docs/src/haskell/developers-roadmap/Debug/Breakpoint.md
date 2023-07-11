```haskell
{-# OPTIONS_GHC -fplugin Debug.Breakpoint #-}

module Try.Debug.Breakpoint where

import Debug.Breakpoint (breakpointIO)

main :: IO ()
main = do
  putStrLn "Type something!"
  x <- getLine
  let y = 2 :: Int
      z = id :: Bool -> Bool
  breakpointIO
  pure ()
```
