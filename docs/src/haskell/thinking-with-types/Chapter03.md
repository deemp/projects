# Chapter 3

```haskell
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Chapter03 where
```

Variance

```haskell
newtype T1 a = T1 (Int -> a)
newtype T2 a = T2 (a -> Int)
newtype T3 a = T3 (a -> a)
newtype T4 a = T4 ((Int -> a) -> a)
newtype T5 a = T5 ((a -> Int) -> Int)
```

## 3-is

```haskell
instance Functor T1 where
  fmap f (T1 g) = T1 $ f . g

instance Functor T5 where
  fmap f (T5 g) = T5 $ \b -> g (b . f)
```
