module Try.TypeClasses where

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

-- Trying conversions
-- Problem is with numeric literals

data Foo = Foo
data Bar = Bar

data FooOrA a = TFoo Foo | TA a

class Convert a b where
  toFooOrA :: a -> FooOrA b

instance Convert Foo a where
  toFooOrA = TFoo

instance (a ~ b) => Convert a b where
  toFooOrA = TA

-- type family

s1 :: Integer
s1 = case toFooOrA @Int 42 of
  TFoo _ -> 1
  TA _ -> 2

-- >>>s
-- 1

s2 :: Integer
s2 = case toFooOrA Bar of
  TFoo _ -> 1
  TA _ -> 2

-- >>>s2
-- 2
