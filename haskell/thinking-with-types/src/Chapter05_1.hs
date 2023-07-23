{-
## 5.3 Heterogeneous lists
-}

{- FOURMOLU_DISABLE -}

{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Chapter05_1 () where

import Data.Kind (Constraint, Type)

data HList (ts :: [Type]) where
  HNil :: HList '[]
  (:#) :: t -> HList ts -> HList (t ': ts)
infixr 5 :#

type family All (c :: Type -> Constraint) (ts :: [Type]) :: Constraint where
  All c '[] = ()
  All c (t ': ts) = (c t, All c ts)

data HList' (ts :: [Type]) where
  HNil' :: HList' '[]
  (:#:) :: t -> HList' ts -> HList' (t ': ts)
infixr 5 :#:

instance (All Show ts) => Show (HList' ts) where
  show HNil' = ""
  show (a :#: HNil') = show a
  show (a :#: as) = show a <> ", " <> show as

f :: HList ts -> HList' ts
f hs = g hs HNil'
 where
  g :: HList ps -> HList' ps' -> HList' ps
  g HNil HNil' = HNil'
  g (a :# as) _ = a :#: g as HNil'
  g HNil _ = HNil'

instance (All Show ts) => Show (HList ts) where
  show ps = "[ " <> (show . f) ps <> "]"

p2 :: HList '[[Char], Bool, Integer, Bool]
p2 = "Hou" :# False :# 3 :# True :# HNil

-- >>>p2
-- [ "Hou", False, 3, True]
