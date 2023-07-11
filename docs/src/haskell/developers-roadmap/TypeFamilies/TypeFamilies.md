```haskell
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-missing-kind-signatures #-}
{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}
{-# OPTIONS_GHC -Wno-unused-type-patterns #-}

module Try.TypeFamilies.TypeFamilies where

import Data.Data (Proxy (Proxy), Typeable, typeRep)
import Data.Functor.Identity (Identity (Identity))
import Data.Kind (Type)

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
  plus :: a -> a -> SumTy a a
  plus x y = x + y

checkAdd :: Double
checkAdd = plus (5 :: Integer) (6 :: Double)

-- >>> checkAdd
-- 11.0

-- Type families https://serokell.io/blog/type-families-haskell

-- kind signature
type Append :: forall a. [a] -> [a] -> [a]
type family Append xs ys where -- header
  Append '[] ys = ys -- clause 1
  Append (x : xs) ys = x : Append xs ys

type MaybeIf :: Bool -> Type -> Type
type family MaybeIf b t where
  MaybeIf 'True t = Maybe t
  MaybeIf 'False t = Identity t

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
```

-- Multiline ghci code
>>> a = 3
>>> b = a
>>> b + a
6

```haskell
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
dedup :: (Eq a) => [a] -> [a]
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

-- TODO deduplicate with Theory
```
