module Matrixmultiplication where

import Data.Primitive
import Data.VecList

-- vectorised form
matmultVec :: [[Float]] -> [[Float]] -> [[Float]]
matmultVec a b =
  [ [ fold (\x y -> x + y :: FloatX4) (\x y -> x + y :: Float) 0 $
  zipVec
    (\x y -> x * y :: FloatX4)
    (\x y -> x * y :: Float)
    (toVecList ar)
    (toVecList bc)
  | bc <- (transpose b)
  ]
  | ar <- a
  ]

-- scalar form
matmult :: [[Float]] -> [[Float]] -> [[Float]]
matmult a b = [[sum $ zipWith (*) ar bc | bc <- (transpose b)] | ar <- a]

transpose :: [[a]] -> [[a]]
transpose [] = []
transpose ([]:xs) = transpose xs
transpose ((y:ys):xs) =
  (y : [h | (h:_) <- xs]) : (transpose (ys : [t | (_:t) <- xs]))
