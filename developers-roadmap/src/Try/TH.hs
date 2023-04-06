{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Try.TH where

import Language.Haskell.TH

myFunc :: Q Exp
myFunc = [|\x -> x + 1|]

add2 :: Q Exp
add2 = [|$myFunc . $myFunc|]

-- >>> runQ add2
-- InfixE (Just (LamE [VarP x_0] (InfixE (Just (VarE x_0)) (VarE GHC.Num.+) (Just (LitE (IntegerL 1)))))) (VarE GHC.Base..) (Just (LamE [VarP x_1] (InfixE (Just (VarE x_1)) (VarE GHC.Num.+) (Just (LitE (IntegerL 1))))))

myFuncTyped :: Code Q (Integer -> Integer)
myFuncTyped = [||\x -> x + 1||]

add2Typed :: Code Q (Integer -> Integer)
add2Typed = [||$$myFuncTyped . $$myFuncTyped||]

-- Couldn't match type `[Char]' with `Bool'
-- Expected: Code m0_a21g[tau:1] Bool
--   Actual: Code m0_a21g[tau:1] String
-- In the Template Haskell quotation [|| "foo" ||]
-- In the expression: [|| "foo" ||]
-- In the Template Haskell splice $$([|| "foo" ||])
-- Couldn't match expected type: Q a_a21e[sk:1]
--             with actual type: Code m0_a21g[tau:1] Bool
-- In the Template Haskell quotation [|| True == $$([|| "foo" ||]) ||]
-- In the first argument of `runQ', namely
--   `[|| True == $$([|| "foo" ||]) ||]'
-- In the expression: runQ [|| True == $$([|| "foo" ||]) ||]
-- Relevant bindings include
--   it_a1ZV :: m_a21d[sk:1] a_a21e[sk:1]
--     (bound at /home/eyjafjallajokull/Desktop/projects/developers-roadmap/src/TryTH.hs:22:2)

