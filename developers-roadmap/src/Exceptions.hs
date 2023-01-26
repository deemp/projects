{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use tuple-section" #-}

module Exceptions where

import RIO
import GHC.Enum (Enum(..))

oneSecond, fiveSeconds :: Int
oneSecond = 1000000
fiveSeconds = 5000000

main :: IO ()
main = runSimpleApp $ do
  res <- timeout oneSecond $ do
    logInfo "Inside the timeout"
    res <-
      tryAny $
        threadDelay fiveSeconds
          `finally` logInfo "Inside the finally"
    logInfo $ "Result: " <> displayShow res
  logInfo $ "After timeout: " <> displayShow res

newtype State s a = State {runState :: s -> (a, s)}

instance Functor (State s) where
  fmap :: (a -> b) -> State s a -> State s b
  fmap f (State a) = State $ \s ->
    let (a1, s1) = a s
     in (f a1, s1)

instance Applicative (State s)

-- pure :: a -> State s a
-- pure a = State $ \s -> (a, s)
-- (<*>) :: State s (a -> b) -> State s a -> State s b
-- (State f) <*> (State x) = State $ \s ->
--   let (a1, s1) = x s
--       (a2, s2) = f s1
--    in (a2 a1, s2)

instance Monad (State s) where
  return :: a -> State s a
  return = pure
  (>>=) :: State s a -> (a -> State s b) -> State s b
  (State x) >>= f = State $ \s ->
    let (a1, s1) = x s
        (a2, s2) = runState (f a1) s1
     in (a2, s2)

data MyMaybe a = MyJust a | MyNothing

-- instance Functor MyMaybe where

a :: Maybe Int
a = Just 3

s :: Maybe Int
s = fmap succ a

-- >>>s
-- Just 4

fmap1 :: Functor f => (a -> b) -> (f a -> f b)
fmap1 = undefined
