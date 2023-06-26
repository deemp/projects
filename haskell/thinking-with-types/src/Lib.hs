{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Lib () where

import Data.Data (Proxy)
import GHC.Generics (Generic (..))

{-
## 13 Generics

### 13.1 Generic Representations

All data types have a canonical representation as sums of products.
They can be built from `Either`s of pairs `(,)`. E.g., for `Maybe`:
-}

toCanonical :: Maybe a -> Either () a
toCanonical Nothing = Left ()
toCanonical (Just a) = Right a

fromCanonical :: Either () a -> Maybe a
fromCanonical (Left ()) = Nothing
fromCanonical (Right a) = Just a

-- >>> :kind! Rep Bool
-- Rep Bool :: * -> *
-- = M1
--     D
--     ('MetaData "Bool" "GHC.Types" "ghc-prim" 'False)
--     (M1 C ('MetaCons "False" 'PrefixI 'False) U1
--      :+: M1 C ('MetaCons "True" 'PrefixI 'False) U1)

{-
### 13.2 Deriving Structural Polymorphism

Generically derive structural polymorphism:

1. Define a typeclass to act as a carrier .
1. Provide inductive instances of the class for the generic
constructors.
1. Finally, write a helper function to map between the Rep and the
desired type.

### 2.4 Type-Level functions

- Type families must be saturated
  - no currying

-}

{- INDENT 4 -}

type family Map (x :: a -> b) (i :: [a]) :: [b] where
  Map f '[] = '[]
  Map f (x ': xs) = f x ': Map f xs

type family Or (x :: Bool) (y :: Bool) :: Bool where
  Or 'True y = 'True
  Or 'False y = y

-- >>>  :t undefined :: Proxy (Map (Or True) '[True, 'False, 'False])
-- The type family `Or' should have 2 arguments, but has been given 1
-- In an expression type signature:
--   Proxy (Map (Or True) '[True, 'False, 'False])
-- In the expression:
--     undefined :: Proxy (Map (Or True) '[True, 'False, 'False])

