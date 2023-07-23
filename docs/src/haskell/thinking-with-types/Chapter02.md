# Chapter 2

```haskell
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Chapter02 where

import Control.Monad.Trans.Class (MonadTrans)
import GHC.TypeLits
```

## 2.3.2

```haskell
-- >>>:kind! (1 + 17) - 3
-- (1 + 17) - 3 :: Natural
-- = 15

-- >>>:kind! (Div 128 8) ^ 2
-- (Div 128 8) ^ 2 :: Natural
-- = 256
```

## 2.3.3


### 2.3.3-i

```haskell
-- >>>:kind! Show
-- Show :: * -> Constraint
-- = Show
```

### 2.3.3-ii

```haskell
-- >>>:kind! Functor
-- Functor :: (* -> *) -> Constraint
-- = Functor
```

### 2.3.3-iv

```haskell
-- >>>:kind! Monad
-- Monad :: (* -> *) -> Constraint
-- = Monad
```

### 2.3.3-v

```haskell
-- >>>:kind! MonadTrans
-- MonadTrans :: ((* -> *) -> * -> *) -> Constraint
-- = MonadTrans
```

### 2.4.1

```haskell
type family Not (x :: Bool) :: Bool where
  Not 'True = 'False
  Not 'False = 'True

type family Foo1 (x :: Bool) (y :: Bool) :: Bool
type family Bar1 x y :: Bool -> Bool -> Bool

-- >>>:kind Foo1
-- Foo1 :: Bool -> Bool -> Bool

-- >>>:kind Bar1
-- Bar1 :: * -> * -> Bool -> Bool -> Bool
```
