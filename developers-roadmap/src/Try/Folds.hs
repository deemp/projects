{-# LANGUAGE LambdaCase #-}

module Try.Folds where

import Control.Monad.Fix (fix)
import Data.List (scanl')

calc l1 = reverse $ fix (\go s acc -> \case (x : xs) -> go (s - x) (s : acc) xs; [] -> s : acc) (sum l1) [] l1

-- >>> calc [3,5,6]
-- [14,11,6,0]

calc1 l1 = reverse $ scanr (flip (-)) (sum l1) (reverse l1)

-- >>> calc1 [3,5,6]
-- [14,11,6,0]
 
calc2 l1 = scanl' (-) (sum l1) l1

-- >>> calc2 [3,5,6]
-- [14,11,6,0]
