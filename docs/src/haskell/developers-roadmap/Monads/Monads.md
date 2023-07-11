```haskell
{-# LANGUAGE BlockArguments #-}
{-# HLINT ignore "Use newtype instead of data" #-}
```

<!-- LIMA_ENABLE -->

```haskell
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TupleSections #-}
```

<!-- LIMA_DISABLE -->

```haskell
{-# HLINT ignore "Use <$>" #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
```

<!-- LIMA_DISABLE -->

```haskell
module Try.Monads.Monads where
```

<!-- LIMA_ENABLE -->

## State

```haskell
newtype State s a = State {runState :: s -> (a, s)}

instance Functor (State s) where
  fmap :: (a -> b) -> State s a -> State s b
  fmap f s = pure f <*> s

instance Applicative (State s) where
  pure :: a -> State s a
  pure x = State (x,)
  (<*>) :: State s (a -> b) -> State s a -> State s b
  f <*> x = do
    f1 <- f
    x1 <- x
    return $ f1 x1

instance Monad (State s) where
  return :: a -> State s a
  return = pure
  (>>=) :: State s a -> (a -> State s b) -> State s b
  (State f) >>= y = State $ \s ->
    let (a1, s1) = f s
        State x = y a1
        (a2, s2) = x s1
     in (a2, s2)

get :: State s s
get = State $ \s -> (s, s)

put :: s -> State s ()
put s = State $ const ((), s)

expr :: State s ()
expr = do
  t1 <- get
  put t1

expr1 :: ((), Int)
expr1 = flip runState 3 do
  t1 <- get
  put (t1 + 2)

-- >>>expr1
-- ((),5)
```

## Cont

```haskell
newtype Cont r a = Cont {runCont :: (a -> r) -> r} deriving (Functor)

instance Applicative (Cont r) where
  pure :: a -> Cont r a
  pure x = Cont ($ x)
  (<*>) :: Cont r (a -> b) -> Cont r a -> Cont r b
  (Cont f) <*> (Cont x) = Cont $ \y -> f $ \k -> x $ y . k

instance Monad (Cont r) where
  return :: a -> Cont r a
  return = pure
  (>>=) :: Cont r a -> (a -> Cont r b) -> Cont r b
  (Cont f) >>= x = Cont $ \b -> f $ \a -> runCont (x a) $ \t -> b t
```

## ContT

```haskell
data ContT r m a = ContT {runContT :: (a -> m r) -> m r} deriving (Functor)

instance Applicative (ContT r m) where
  pure :: a -> ContT r m a
  pure x = ContT ($ x)
  (<*>) :: ContT r m (a -> b) -> ContT r m a -> ContT r m b
  (ContT f) <*> (ContT x) = ContT $ \y -> f $ \k -> x $ y . k

instance Monad (ContT r m) where
  return :: a -> ContT r m a
  return = pure
  (>>=) :: ContT r m a -> (a -> ContT r m b) -> ContT r m b
  (ContT f) >>= x = ContT $ \b -> f $ \a -> runContT (x a) $ \t -> b t
```
