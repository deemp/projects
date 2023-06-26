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

{-
## GADTs

- Wikibooks ([src](https://en.wikibooks.org/wiki/Haskell/GADT)):
  > With GADTs, a constructor for `Foo` a is not obliged to return `Foo a`; it can return any `Foo blah` that you can think of.
-}

{- i 4 -}

data TrueGadtFoo a where
  MkTrueGadtFoo :: a -> TrueGadtFoo Int

{-
    - Still, need to use a relevant data constructor

      ```hs
      data Foo where
        MkFoo :: Bar Int -- This will not typecheck
      ```
- Support record syntax - [src](https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/gadt.html)
  - [example](https://hackage.haskell.org/package/servant-server-0.19.2/docs/src/Servant.Server.Internal.Delayed.html#Delayed)

- Is it considered a good practice to put constraints in consructors inside GADT declaration?
  - No. Use a compiler to derive instances like `Functor`, put constraints in functions.

## Heterogeneous list

[src](https://www.parsonsmatt.org/2017/04/26/basic_type_level_programming_in_haskell.html#heterogeneous-lists)
-}

{- i 4 -}

data HList_ xs where
  HNil_ :: HList_ '[]
  (:::) :: a -> HList_ as -> HList_ (a ': as)

infixr 6 :::

hex :: HList_ '[Char, Integer, String]
hex = 'a' ::: 1 ::: "hello" ::: HNil_

{-
## Non-empty list
-}

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
