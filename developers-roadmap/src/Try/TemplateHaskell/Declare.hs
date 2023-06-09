{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# HLINT ignore "Avoid lambda using `infix`" #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Try.TemplateHaskell.Declare where

import Language.Haskell.TH
import Language.Haskell.TH.Syntax

myFunc :: Q Exp
myFunc = [|\x -> x + 1|]

add2 :: Q Exp
add2 = [|$myFunc . $myFunc|]

runAdd2 :: Quasi m => m Exp
runAdd2 = runQ add2

-- >>> runAdd2
-- InfixE (Just (LamE [VarP x_2] (InfixE (Just (VarE x_2)) (VarE GHC.Num.+) (Just (LitE (IntegerL 1)))))) (VarE GHC.Base..) (Just (LamE [VarP x_3] (InfixE (Just (VarE x_3)) (VarE GHC.Num.+) (Just (LitE (IntegerL 1))))))

myFuncTyped :: Code Q (Integer -> Integer)
myFuncTyped = [||\x -> x + 1||]

runMyFuncTyped :: Quasi m => m Exp
runMyFuncTyped = runQ $ unTypeCode myFuncTyped

-- >>> runMyFuncTyped
-- LamE [VarP x_5] (InfixE (Just (VarE x_5)) (VarE GHC.Num.+) (Just (LitE (IntegerL 1))))

add2Typed :: Code Q (Integer -> Integer)
add2Typed = [||$$myFuncTyped . $$myFuncTyped||]

runAdd2Typed :: Quasi m => m Exp
runAdd2Typed = runQ $ unTypeCode add2Typed

-- >>> runAdd2Typed
-- InfixE (Just (LamE [VarP x_6] (InfixE (Just (VarE x_6)) (VarE GHC.Num.+) (Just (LitE (IntegerL 1)))))) (VarE GHC.Base..) (Just (LamE [VarP x_7] (InfixE (Just (VarE x_7)) (VarE GHC.Num.+) (Just (LitE (IntegerL 1))))))
