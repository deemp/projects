{-
# Chapter 4
-}

{- FOURMOLU_DISABLE -}

{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Unused LANGUAGE pragma" #-}

module Chapter4 where

import Data.Typeable (Proxy (..), typeRep)

tr = typeRep (Proxy :: Proxy (Maybe Int))

-- >>> tr
-- Maybe Int

-- >>>:t fmap @Maybe
-- fmap @Maybe :: (a -> b) -> Maybe a -> Maybe b
-- >>>:t fmap @_ @Int @Bool
-- fmap @_ @Int @Bool :: Functor _ => (Int -> Bool) -> _ Int -> _ Bool

-- sugar for
-- p' :: forall a. a -> a
p' :: a -> a
p' = id

p :: forall a b c d. (Functor a, Functor b) => a c -> b c -> b c
p a b = b

-- >>>:t p
-- p :: (Functor a, Functor b) => a c -> b c -> b c
-- >>>:t p @_ @_
-- p @_ @_ :: (Functor _1, Functor _2) => _1 c -> _2 c -> _2 c

type family AlwaysUnit a where
  AlwaysUnit a = ()

p1 :: AlwaysUnit a -> a
p1 = undefined
