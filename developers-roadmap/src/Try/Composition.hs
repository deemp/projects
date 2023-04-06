{-# LANGUAGE LambdaCase #-}

module Try.Composition where

import Control.Monad.Fix (fix)

s = (. (+)) . (.) . (*)
s1 = (. (+)) . ((.) . (*))
s2 = (. (+)) . (\x -> (.) ((*) x))
s3 = \y -> (. (+)) ((\x -> (.) ((*) x)) y)
s4 y = (. (+)) (\z -> (.) ((*) y) z)
s5 y = (. (+)) (\z -> ((*) y) . z)
s6 y = (. (+)) (\z i -> ((*) y) (z i))
s7 y = (. (+)) (\z i -> y * z i)
s8 y = (\z i -> y * z i) . (+)
s9 y = \p -> (\z i -> y * z i) ((+) p)
s10 y p = \i -> y * ((+) p) i
s11 y p i = y * (p + i)
s12 a b c = a * (b + c)

-- >>>s1 3 5 6 == s12 3 5 6
-- True
