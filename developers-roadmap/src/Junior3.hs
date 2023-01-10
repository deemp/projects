{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Junior3 where

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