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

dotVec' :: [Float] -> [Float] -> Float
dotVec' xs ys
 = let l1 = length xs
       l2 = length ys
    in fold (\x y -> x + y :: FloatX4) (\x y -> x + y :: Float) 0 $
       zipVec
        (\x y -> x * y :: FloatX4)
        (\x y -> x * y :: Float)
        (toVecArray (Z :. l1) xs)
        (toVecArray (Z :. l2) ys)

-- scalar form
dotp :: [Float] -> [Float] -> Float
dotp xs ys = sum $ zipWith (*) xs ys

dotp' :: [Float] -> [Float] -> Float
dotp' xs ys = foldr (+) 0 $ zipWith (*) xs ys
