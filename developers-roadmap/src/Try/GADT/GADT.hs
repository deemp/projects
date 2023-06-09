{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Try.GADT.GADT where

-- GADT
-- we have to define these types
data Empty
data NonEmpty

data SafeList a b where
  Nil :: SafeList a Empty
  Cons :: a -> SafeList a b -> SafeList a NonEmpty

safeHead :: SafeList a NonEmpty -> a
safeHead (Cons a _) = a

safeTail :: SafeList a b -> a
safeTail (Cons a Nil) = a
safeTail (Cons _ b) = safeTail b

st1 :: Integer
st1 = safeTail $ Cons 3 (Cons 4 Nil)

-- >>>st1
-- 4

data P = MkP -- 1
data Prom = P -- 2

-- sprom :: 'P -> 'P
-- sprom x = x

-- https://www.parsonsmatt.org/2017/04/26/basic_type_level_programming_in_haskell.html#heterogeneous-lists

data HList (xs :: a) where
  HNil :: HList '[]
  (:::) :: a -> HList as -> HList (a ': as)

infixr 6 :::

hex :: HList '[Char, Integer, String]
hex = 'a' ::: 1 ::: "hello" ::: HNil

-- >>>:t
-- 'a' ::: 1 ::: "hello" ::: HNil :: Num a => HList '[Char, a, String]
