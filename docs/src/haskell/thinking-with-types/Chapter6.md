# Chapter 6

<!-- FOURMOLU_DISABLE -->

```haskell
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Chapter6 () where

import Data.Void (absurd)
```

## 6.2 Ranks

```haskell
-- takes a function that accepts any possible a
applyToFive :: (forall a. a -> a) -> Int
applyToFive f = f 5

p :: Int -> (forall a. (a -> a))
p b = id

p1 :: (a -> b) -> (forall c. c -> a) -> b
p1 a b = (a . b) absurd

type A a = a

type B a r = (forall r. (a -> r) -> r)
```

## 6.4 The Continuation Monad

```haskell
from :: A a -> B a r
from a b = b a

to :: B a r -> A a
to b = b id
```

```hs
(to . from) x =
    = to (from x)
    = (from x) id
    = (\b -> b x) id
    = id x
    = x

(from . to) x =
    = from (to x)
    = from (x id)
    = \b -> b (x id)
    = \b -> b ((\s -> s (a :: a)) id)
    = \b -> b (a :: a)
```

```haskell
newtype Cont a = Cont {unCont :: forall r. (a -> r) -> r}

instance Functor Cont where
  fmap f (Cont a) = Cont $ \x -> a $ x . f

--   fmap f (Cont a) = Cont $ \x -> a $ \y -> x $ f y

instance Applicative Cont where
  pure a = Cont $ \x -> x a
  (Cont a) <*> (Cont b) = Cont $ \x -> a $ \y -> b (x . y)

instance Monad Cont where
  return = pure
  (Cont a) >>= f = Cont $ \x -> a (\y -> unCont (f y) x)
```

```hs
(Cont a) <*> (Cont b) = Cont $ \x -> a $ \y -> b $ \z -> x (y z)
(Cont a) <*> (Cont b) = Cont $ \x -> a $ \y -> b $ \z -> (x . y) z
\z -> x (y z) =
    x . y
```

```haskell
withVersionNumber :: (Double -> r) -> r
withVersionNumber f = f 1.0

withTimestamp :: (Int -> r) -> r
withTimestamp f = f 1532083362

withOS :: (String -> r) -> r
withOS f = f "linux"

releaseStringCont :: String
releaseStringCont = flip unCont id $ do
  version <- Cont withVersionNumber
  date <- Cont withTimestamp
  os <- Cont withOS
  return $ os ++ "-" ++ show version ++ "-" ++ show date

newtype ContT r m a = ContT {unContT :: (a -> m r) -> m r}

instance (Functor m) => Functor (ContT r m) where
  fmap f (ContT a) = ContT $ \x -> a $ x . f

instance (Functor m) => Applicative (ContT r m) where
  pure x = ContT $ \y -> y x
  (ContT a) <*> (ContT b) = ContT $ \x -> a $ \y -> b (x . y)
```

[Why use ContT](https://ro-che.info/articles/2019-06-07-why-use-contt)

```haskell
instance (Functor m) => Monad (ContT r m) where
  return = pure
  (ContT a) >>= f = ContT $ \x -> a (\y -> unContT (f y) x)
```
