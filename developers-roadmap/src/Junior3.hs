{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Junior3 where

import Data.Data (Proxy (..), Typeable)
import Data.Functor
import Data.Functor.Identity (Identity (Identity))
import Data.Kind (Type)
import Data.Typeable (typeRep)
import GHC.Base (Symbol)

-- Type families

class Add a b where
  type SumTy a b
  plus :: a -> b -> SumTy a b

instance Add Integer Double where
  type SumTy Integer Double = Double
  plus :: Integer -> Double -> SumTy Integer Double
  plus x y = fromIntegral x + y

instance Add Double Integer where
  type SumTy Double Integer = Double
  plus :: Double -> Integer -> SumTy Double Integer
  plus x y = x + fromIntegral y

instance (Num a) => Add a a where
  type SumTy a a = a
  plus :: Num a => a -> a -> SumTy a a
  plus x y = x + y

checkAdd :: Double
checkAdd = plus (5 :: Integer) (6 :: Double)

-- >>>checkAdd
-- 11.0

-- Type families https://serokell.io/blog/type-families-haskell

-- kind signature
type Append :: forall a. [a] -> [a] -> [a]
type family Append xs ys where -- header
  Append '[] ys = ys -- clause 1
  Append (x : xs) ys = x : Append xs ys

type MaybeIf :: Bool -> Type -> Type
type family MaybeIf b t where
  MaybeIf True t = Maybe t
  MaybeIf False t = Identity t

data PlayerInfo b = MkPlayerInfo
  { name :: MaybeIf b String
  , score :: MaybeIf b Integer
  }

s1 :: Identity Int
s1 = Identity 3 :: MaybeIf False Int

s2 :: Maybe Int
s2 = Just 3 :: MaybeIf True Int

-- move type family parameter from header to body
type MaybeIf' :: Bool -> Type -> Type
type family MaybeIf' b where
  MaybeIf' True = Maybe
  MaybeIf' False = Identity

-- Open type families

type family F a
type instance F a = [a]
type instance F Char = String

-- Compatibility
-- - Their left-hand sides are apart (i.e. not overlapping)
-- - Their left-hand sides unify with a substitution, under which the right-hand sides are equal.

-- Like, we make right-hand sides equal, and then rewrite left-hand sides until we get the same expressions
type family G a b
type instance G a Bool = a -> Bool
type instance G Char b = Char -> b

-- a -> Bool ---> Char -> Bool => a = Char
-- Char -> b ---> Char -> Bool => b = Bool
-- =>
-- G a    Bool ---> G Char Bool
-- G Char b    ---> G Char Bool

type instance G Char Bool = Char -> Bool

{-
-- Multiline ghci code
>>> a = 3
>>> b = a
>>> b + a
6
-}

-- Associated types
-- Allows to switch from this
type family Elem a
class Container a where
  elements :: a -> [Elem a]

type instance Elem [a] = a
instance Container [a] where
  elements :: [a] -> [Elem [a]]
  elements = id

-- to this
class Container1 a where
  type Elem1 a
  elements1 :: a -> [Elem a]

instance Container1 [a] where
  type Elem1 [a] = a
  elements1 :: [a] -> [Elem [a]]
  elements1 = id

-- and get default values

type family Unwrap x where
  Unwrap (f a) = a

class Container2 a where
  type Elem2 a

  -- default
  type Elem2 x = Unwrap x
  elements' :: a -> [Elem2 a]

-- Checks during pattern matching
dedup :: Eq a => [a] -> [a]
dedup (x1 : x2 : xs) | x1 == x2 = dedup (x1 : xs)
dedup (y : xs) = y : dedup xs
dedup [] = []

-- Type family dependencies
-- injectivity

type family Not x = r | r -> x where
  Not True = False

s :: forall x. (Not x ~ True, Typeable x) => String
s = show (typeRep $ Proxy @x)

-- ?
-- >>>:set -XTypeFamilyDependencies
-- >>>s
-- Couldn't match type `Not x0_a1S7U[tau:1]' with 'True
--   arising from a use of `s'
-- The type variable `x0_a1S7U[tau:1]' is ambiguous
-- In the expression: s
-- In an equation for `it_a1S6U': it_a1S6U = s

-- Associated data type
class Vectorizable a where
  data Vector a
  vlength :: Vector a -> Int

newtype S = S {unS :: [Int]}
instance Vectorizable S where
  data Vector S = Vector {unVector :: S}
  vlength :: Vector S -> Int
  vlength = length . unS . unVector

-- Data family
data family SomeFamily a
newtype instance SomeFamily Int = SomeF Int

-- Fundeps exercise - https://www.fpcomplete.com/haskell/tutorial/fundeps/#exercises

newtype PersonReader a = PersonReader {runPersonReader :: Person -> a}
  deriving (Functor, Applicative, Monad)

class Monad m => MonadReader env m | m -> env where
  ask :: m env

data Person = Person
  { nameP :: String
  , ageP :: Int
  }
  deriving (Show)

askAge :: MonadReader Person m => m Int
askAge = ask <&> ageP

askName :: MonadReader Person m => m String
askName = ask <&> nameP

greeting :: forall m. (Monad m, MonadReader Person m) => m String
greeting = do
  name <- askName
  age <- askAge
  pure $ name ++ " is " ++ show age ++ " years old"

instance MonadReader Person PersonReader where
  ask :: PersonReader Person
  ask = PersonReader id

greetingId :: String
greetingId = runPersonReader (greeting @PersonReader) Person{nameP = "ah", ageP = 3}

-- >>>greetingId
-- "ah is 3 years old"

-- GADTs
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

data HList xs where
  HNil :: HList '[]
  (:::) :: a -> HList as -> HList (a ': as)

infixr 6 :::

hex :: HList '[Char, Integer, String]
hex = 'a' ::: 1 ::: "hello" ::: HNil

-- >>>:t
-- 'a' ::: 1 ::: "hello" ::: HNil :: Num a => HList '[Char, a, String]
