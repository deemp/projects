{-
# Chapter 5
-}

{- FOURMOLU_DISABLE -}

{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-} 
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}


module Chapter5() where 


import Control.Monad.Trans.Class (MonadTrans)
import Data.Kind(Constraint, Type)

{-
## 5.2 GADTs
-}

data Expr a where
    LitInt :: Int -> Expr Int
    LitBool :: Bool -> Expr Bool
    Add :: Expr Int -> Expr Int -> Expr Int
    Not :: Expr Bool -> Expr Bool
    If :: Expr Bool -> Expr a -> Expr a -> Expr a

evalExpr :: Expr a -> a
evalExpr (LitInt i) = i
evalExpr (LitBool b) = b
evalExpr (Add x y) = evalExpr x + evalExpr y
evalExpr (Not x) = not $ evalExpr x
evalExpr (If b x y ) =
    if evalExpr b
    then evalExpr x
    else evalExpr y

ex1 :: Expr Int
ex1 =  If (LitBool False ) ( LitInt 1) (Add ( LitInt 5) (LitInt 13))

-- >>>evalExpr ex1
-- 18

{-
## 5.3 Heterogeneous lists
-}

data HList (ts :: [Type]) where
    HNil :: HList '[]
    (:#) :: t -> HList ts -> HList (t ': ts)
infixr 5 :#

-- >>>:t HNil
-- HNil :: HList '[]
-- >>>:t True :# HNil
-- True :# HNil :: HList '[Bool]

hHead :: HList (t:ts :: [Type]) -> t
hHead (x :# y) = x

p1 :: HList '[[Char], Bool, Integer, Bool]
p1 = "Hey" :# True :# 3 :# True :# HNil

-- >>>hHead p1
-- "Hey"

hLength :: HList ts -> Int
hLength HNil = 0
hLength (p :# ps) = 1 + hLength ps


-- >>>hLength p1
-- 4

showBool :: HList (t1 : Bool : ts) -> String
showBool (_ :# b :# _) = show b

instance Eq (HList '[]) where
    HNil == HNil = True

instance (Eq t, Eq (HList ts)) => Eq (HList (t ': ts)) where
    (a :# as) == (b :# bs) = a == b && as == bs

-- 5.3-i
instance Ord (HList '[]) where
    HNil <= HNil = True

instance (Ord t, Ord (HList ts)) => Ord (HList (t ': ts)) where
    (a :# as) <= (b :# bs) = a < b || a == b && as <= bs

p2 :: HList '[[Char], Bool, Integer, Bool]
p2 = "Hou" :# False :# 3 :# True :# HNil


-- >>>p1 <= p2
-- True

{-
## 5.3-ii
-}


newtype SepHList (ts :: [Type])= SepHList (HList ts)

instance Show (SepHList '[]) where
    show (SepHList HNil) = ""

instance (Show t, Show (SepHList ts)) => Show (SepHList (t:ts)) where
    show (SepHList as) =
        case as of
            a :# HNil -> show a 
            a :# as -> show a <> ", " <> show (SepHList as)

instance Show (HList '[]) where
    show HNil = ""

instance (Show t, Show (HList ts), Show (SepHList ts)) => Show (HList (t ': ts)) where
    show as = "[ " <> show (SepHList as) <> " ]"

-- >>>p2
-- [ "Hou", False, 3, True ]

type family AllEq (ts :: [Type]) :: Constraint where
    AllEq '[] = ()
    AllEq (t ': ts) = (Eq t, AllEq ts)

-- >>>:kind! AllEq '[Int, Bool]
-- AllEq '[Int, Bool] :: Constraint
-- = (Eq Int, (Eq Bool, () :: Constraint))
