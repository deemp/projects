```haskell
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Use section" #-}
{-# HLINT ignore "Redundant lambda" #-}
{-# HLINT ignore "Redundant bracket" #-}

module Try.Functions.Composition where

import Control.Monad.Fix (fix)

-- Is
-- (. (+)) . (.) . (*)
-- equivalent to
-- \a b c -> a * (b + c)
-- ?

-- (.) :: (b -> c) -> (a -> b) -> a -> c

s = (. (+)) . (.) . (*)

-- infixr 9  .
-- f . g . h ~ f . (g . h)
s1 = (. (+)) . ((.) . (*))

-- (.) f g = \x -> f (g x)
s2 = (. (+)) . (\x -> (.) ((*) x))

-- f . g = \x -> f (g x)
s3 = \y -> (. (+)) ((\x -> (.) ((*) x)) y)

-- a = \x -> b ~ a x = b
s4 y = (. (+)) ((\x -> (.) ((*) x)) y)

-- (\x -> f) y ~ [y/x]f
s5 y = (. (+)) ((.) ((*) y))

-- (\a b -> a `op` b) f = \b -> f `op` b
-- (.) f = \g -> f . g
s6 y = (. (+)) (\z -> ((*) y) . z)

-- (.) f g = \x -> f (g x)
s7 y = (. (+)) (\z i -> ((*) y) (z i))

-- (\a b -> a `op` b) f ~ \b -> f `op` b
-- (*) f = \g -> f * g
s8 y = (. (+)) (\z i -> y * z i)

-- (`op` g) f ~ f `op` g
-- (. (+)) f ~ f . (+)
s9 y = (\z i -> y * z i) . (+)

-- f . g ~ \x -> f (g x)
s10 y = \p -> (\z i -> y * z i) ((+) p)

s11 y p = (\z i -> y * z i) ((+) p)

-- (\x -> f) y ~ [y/x]f
s12 y p = \i -> y * ((+) p) i

-- a = \x -> b ~ a x = b
s13 y p i = y * (p + i)

-- alpha reduction
s14 a b c = a * (b + c)

-- >>> s1 3 5 6 == s12 3 5 6
-- True

-- >>> s1 4 98 12 == s12 4 98 12
-- True
```
