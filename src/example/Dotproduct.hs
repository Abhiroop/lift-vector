module Dotproduct where

import Data.Primitive
import Data.Operations
-- vectorised form
dotVec :: [Float] -> [Float] -> Float
dotVec xs ys =
  fold (\x y -> x + y :: FloatX4) (\x y -> x + y :: Float) 0 $
  zipVec
    (\x y -> x * y :: FloatX4)
    (\x y -> x * y :: Float)
    (toVecList xs)
    (toVecList ys)

-- scalar form
dotp :: [Float] -> [Float] -> Float
dotp xs ys = sum $ zipWith (*) xs ys
