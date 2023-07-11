```haskell
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ImpredicativeTypes #-}

module Try.Generics.Generics () where

import Data.Kind (Type)
import GHC.Generics (Generic, Rep)

data Tree a = Leaf a | Node (Tree a) (Tree a)
  deriving (Generic)

-- >>>:kind! forall a. Rep (Tree a)
-- forall a. Rep (Tree a) :: * -> *
-- = M1
--     D
--     ('MetaData "Tree" "TryGenerics" "main" 'False)
--     (M1
--        C
--        ('MetaCons "Leaf" 'PrefixI 'False)
--        (M1
--           S
--           ('MetaSel
--              'Nothing 'NoSourceUnpackedness 'NoSourceStrictness 'DecidedLazy)
--           (K1 R a))
--      :+: M1
--            C
--            ('MetaCons "Node" 'PrefixI 'False)
--            (M1
--               S
--               ('MetaSel
--                  'Nothing 'NoSourceUnpackedness 'NoSourceStrictness 'DecidedLazy)
--               (K1 R (Tree a))
--             :*: M1
--                   S
--                   ('MetaSel
--                      'Nothing 'NoSourceUnpackedness 'NoSourceStrictness 'DecidedLazy)
--                   (K1 R (Tree a))))

-- no generic instance
data Empty1

-- >>>:kind! Rep Empty1
-- Rep Empty1 :: * -> *
-- = Rep Empty1

-- has generic instance
data Empty2 deriving (Generic)

-- >>>:kind! Rep Empty2
-- Rep Empty2 :: * -> *
-- = M1 D ('MetaData "Empty2" "TryGenerics" "main" 'False) V1

-- >>>:kind! Rep Bool
-- Rep Bool :: * -> *
-- = M1
--     D
--     ('MetaData "Bool" "GHC.Types" "ghc-prim" 'False)
--     (M1 C ('MetaCons "False" 'PrefixI 'False) U1
--      :+: M1 C ('MetaCons "True" 'PrefixI 'False) U1)
```

[Representation of types with many constructors or many fields](https://hackage.haskell.org/package/base-4.18.0.0/docs/GHC-Generics.html#g:9)

```haskell
data ManyFields a b c d e f g h i = A1 a b c d e f g h i deriving (Generic)

-- >>>:kind! forall a b c d e f g h i. Rep (ManyFields a b c d e f g h i)
-- forall a b c d e f g h i. Rep (ManyFields a b c d e f g h i) :: *
--                                                                 -> *
-- = M1
--     D
--     ('MetaData "ManyFields" "Try.Generics" "main" 'False)
--     (M1
--        C
--        ('MetaCons "A1" 'PrefixI 'False)
--        (((M1
--             S
--             ('MetaSel
--                'Nothing 'NoSourceUnpackedness 'NoSourceStrictness 'DecidedLazy)
--             (K1 R a)
--           :*: M1
--                 S
--                 ('MetaSel
--                    'Nothing 'NoSourceUnpackedness 'NoSourceStrictness 'DecidedLazy)
--                 (K1 R b))
--          :*: (M1
--                 S
--                 ('MetaSel
--                    'Nothing 'NoSourceUnpackedness 'NoSourceStrictness 'DecidedLazy)
--                 (K1 R c)
--               :*: M1
--                     S
--                     ('MetaSel
--                        'Nothing 'NoSourceUnpackedness 'NoSourceStrictness 'DecidedLazy)
--                     (K1 R d)))
--         :*: ((M1
--                 S
--                 ('MetaSel
--                    'Nothing 'NoSourceUnpackedness 'NoSourceStrictness 'DecidedLazy)
--                 (K1 R e)
--               :*: M1
--                     S
--                     ('MetaSel
--                        'Nothing 'NoSourceUnpackedness 'NoSourceStrictness 'DecidedLazy)
--                     (K1 R f))
--              :*: (M1
--                     S
--                     ('MetaSel
--                        'Nothing 'NoSourceUnpackedness 'NoSourceStrictness 'DecidedLazy)
--                     (K1 R g)
--                   :*: (M1
--                          S
--                          ('MetaSel
--                             'Nothing 'NoSourceUnpackedness 'NoSourceStrictness 'DecidedLazy)
--                          (K1 R h)
--                        :*: M1
--                              S
--                              ('MetaSel
--                                 'Nothing 'NoSourceUnpackedness 'NoSourceStrictness 'DecidedLazy)
--                              (K1 R i))))))

data ManyConstructors = B1 | C1 | D1 | E1 | F1 | G1 | H1 | I1 | J deriving (Generic)

-- >>>:kind! Rep ManyConstructors
-- Rep ManyConstructors :: * -> *
-- = M1
--     D
--     ('MetaData "ManyConstructors" "Try.Generics" "main" 'False)
--     (((M1 C ('MetaCons "B1" 'PrefixI 'False) U1
--        :+: M1 C ('MetaCons "C1" 'PrefixI 'False) U1)
--       :+: (M1 C ('MetaCons "D1" 'PrefixI 'False) U1
--            :+: M1 C ('MetaCons "E1" 'PrefixI 'False) U1))
--      :+: ((M1 C ('MetaCons "F1" 'PrefixI 'False) U1
--            :+: M1 C ('MetaCons "G1" 'PrefixI 'False) U1)
--           :+: (M1 C ('MetaCons "H1" 'PrefixI 'False) U1
--                :+: (M1 C ('MetaCons "I1" 'PrefixI 'False) U1
--                     :+: M1 C ('MetaCons "J" 'PrefixI 'False) U1))))
```

## Defining datatype-generic functions

https://hackage.haskell.org/package/base-4.18.0.0/docs/GHC-Generics.html#g:10
