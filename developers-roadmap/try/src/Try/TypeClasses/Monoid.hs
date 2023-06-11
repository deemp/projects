{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Try.TypeClasses.Monoid where

import Control.Monad (guard)

{-
## Monoid, Semigroup

From https://medium.com/@stackdoesnotwork/magical-monoids-50da92b069f4
-}

-- f <> g = \x -> f x <> g x

expr1 :: [Char]
expr1 = (take 3 <> const "oi" <> drop 4) "Monads are cool!"

-- >>>expr1
-- "Monoids are cool!"

{-
## Guards
-}

deleteIfNegative :: (Num a, Ord a) => a -> Maybe a
deleteIfNegative x = guard (x >= 0) >> pure x

expr2 :: [Maybe Int]
expr2 = [deleteIfNegative 3, deleteIfNegative (-3)]

-- >>>expr2
-- [Just 3,Nothing]

pyth :: [(Integer, Integer, Integer)]
pyth = do
  z <- [1 ..]
  x <- [1 .. z]
  y <- [x .. z]
  guard (x ^ 2 + y ^ 2 == z ^ 2)
  return (x, y, z)

expr3 :: [(Integer, Integer, Integer)]
expr3 = take 5 pyth

-- >>>expr3
-- [(3,4,5),(6,8,10),(5,12,13),(9,12,15),(8,15,17)]
