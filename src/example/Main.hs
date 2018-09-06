{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE BangPatterns #-}
module Main where

import Data.Primitive
import Data.VecList
import Data.VecArray
import Utils

-- This module is for experiments only
sizeOfVec :: Int
sizeOfVec = 4

evalPolyVec :: Float -> [Float] -> Float
evalPolyVec value coeffs = go (splitEvery sizeOfVec coeffs) (length coeffs)
  where
    go [[x]] _ = x
    go ([a, b, c, d]:xs) len =
      let packed_coeff = packVector (a, b, c, d)
          vec_val = broadcastVector value :: FloatX4 -- to help the type family constraint solver
          step_length = len - sizeOfVec
       in (go' packed_coeff vec_val step_length) + (go xs step_length)
      where
        go' pc _ 0 =
          let (x1, x2, x3, x4) = unpackVector pc
           in (x1 * value ^ 3) + (x2 * value ^ 2) + (x3 * value) + x4
        go' pc v l =
          let t = pc * v
           in go' t v (l - 1)

evalPoly :: Float -> [Float] -> Float
evalPoly value coeffs = go coeffs (length coeffs - 1)
  where
    go [] _ = 0
    go (x:xs) len = (x * (value ^ len)) + go xs (len - 1)

evalPolyFold :: Float -> [Float] -> Float
evalPolyFold value coeffs =
  let index_coeffs =
        (zip coeffs (reverse $ take (length coeffs) (iterate (+ 1) 0)))
   in foldr (\(c, p) v -> v + c * value ^ p) 0 index_coeffs

-- Desired DSL
-- evalPolyVector :: Float -> [Float] -> Float
-- evalPolyVector value coeffs
--   = let index_coeffs = (zip coeffs (reverse $ take (length coeffs) (iterate (+ 1) 0)))
--      in fold (\(c,p) v -> ArithExpr v :+
--                           (ArithExpr c :*:
--                            (ArithExpr value :^: ArithExpr p)))
--         0
--         (toVecList index_coeffs)
dotp :: [Float] -> [Float] -> Float
dotp xs ys = sum $ zipWith (*) xs ys

-- Can we hide this using template haskell?
dotVec :: [Float] -> [Float] -> Float
dotVec xs ys
  -- sum $
  -- fromVecList $
 =
  fold (\x y -> x + y :: FloatX4) (\x y -> x + y :: Float) 0 $
  zipVec
    (\x y -> x * y :: FloatX4)
    (\x y -> x * y :: Float)
    (toVecList xs)
    (toVecList ys)

multiply :: [Float] -> Float
multiply xs =
  fold (\x y -> x * y :: FloatX4) (\x y -> x * y :: Float) 0 (toVecList xs)

-- 16  0.2 [1,4,2,3,4,1,5,6,7,8,8,9,1,3,1,5,6]
-- 32 0.2 [1,5,6,3,4,5,6,7,8,9,9,2,3,1,2,3,4,5,1,2,8,3,2,3,4,2,8,6,7,4,2,1,2]
-- write dot product
transpose :: [[a]] -> [[a]]
transpose [] = []
transpose ([]:xs) = transpose xs
transpose ((y:ys):xs) =
  (y : [h | (h:_) <- xs]) : (transpose (ys : [t | (_:t) <- xs]))

matmult :: [[Float]] -> [[Float]] -> [[Float]]
matmult a b = [[sum $ zipWith (*) ar bc | bc <- (transpose b)] | ar <- a]

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

foo1 :: Float
foo1 =
  fold
    (\x y -> x + y :: FloatX4)
    (\x y -> x + y :: Float)
    0
    (toVecList [1 .. 15000000])

foo2 :: Float
foo2 = foldr (+) 0 [1 .. 15000000]

bar1 :: Array Int
bar1 = fromList (Z :. 10) [1..10]

bar2 :: Matrix Int
bar2 = fromList (Z :. 3 :. 5) [1..15]


main :: IO ()
main = do
  print $ bar2 ! (Z :. 1 :. 1)
  --print $ foo1
  -- let l    = replicate 80000 7
  --     arr1 = replicate 80000 l
  --     m    = replicate 80000 8
  --     arr2 = replicate 80000 m
  --     !r   = matmultVec arr1 arr2
  -- print $
  --   (last $ last $ r)
  -- print $
  --   matmult [[2, 3, 4, 5], [6, 7, 8, 9], [10, 11, 12, 13], [14, 15, 16, 17]]
  --           [[1, 7, 8, 9], [15, 14, 13, 11], [9, 19, 7, 8], [4, 5, 6, 7]]
  -- print $
  --   matmultVec [[2, 3, 4, 5], [6, 7, 8, 9], [10, 11, 12, 13], [14, 15, 16, 17]]
  --              [[1, 7, 8, 9], [15, 14, 13, 11], [9, 19, 7, 8], [4, 5, 6, 7]]
  -- print $ evalPolyFold 0.2 [1, 1, 1, 1, 1]
  -- print $ evalPoly 0.2 [1, 1, 1, 1, 1]
  -- print $ evalPolyVec 0.2 [1, 1, 1, 1, 1]
  -- print $ dotVec [1..10000000] [1..10000000]
  -- print $ dotVec [1,2,3,4,5] [1,2,3,4,5]
  -- print $ foo1
  --print $ foo2
