{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Lib () where

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

-}