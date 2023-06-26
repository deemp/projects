{-# LANGUAGE ImplicitParams #-}

module Try.ImplicitParams.ImplicitParams where

data C

f :: Bool -> C
f = undefined

-- так задаем
x :: C
x = let ?a = True; ?b = False in y

-- так используем
y :: (?b :: Bool, ?a :: Bool) => C
y = f ?a