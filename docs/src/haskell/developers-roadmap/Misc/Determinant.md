```haskell
module Try.Misc.Determinant where

import Data.Matrix (Matrix, detLU, matrix)

m1 :: Matrix Double
m1 = matrix 3 3 (\(a, b) -> fromIntegral (a ^ b))

-- >>> m1
-- ┌                ┐
-- │  1.0  1.0  1.0 │
-- │  2.0  4.0  8.0 │
-- │  3.0  9.0 27.0 │
-- └                ┘

det :: Double
det = detLU m1

-- >>> det
-- 12.0
```
