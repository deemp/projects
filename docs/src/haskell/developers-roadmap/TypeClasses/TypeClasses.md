```haskell
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Try.TypeClasses.TypeClasses where
```

## Problem

Make a function that converts a `Foo` to `TFoo Foo` and other types to `TA a`.
Avoid `TypeApplications` if possible.

```haskell
data Foo = Foo
data Bar = Bar

data FooOrA a = TFoo Foo | TA a

class Convert a b where
  toFooOrA :: a -> FooOrA b

instance Convert Foo a where
  toFooOrA = TFoo

instance (a ~ b) => Convert a b where
  toFooOrA = TA

s1 :: Integer
s1 = case toFooOrA @Int 42 of
  TFoo _ -> 1
  TA _ -> 2

-- >>>s
-- 1

s2 :: Integer
s2 = case toFooOrA Bar of
  TFoo _ -> 1
  TA _ -> 2

-- >>>s2
-- 2
```
