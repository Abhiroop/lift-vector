{-# LANGUAGE TypeFamilies #-}
module Main where

import Data.Primitive
import Data.VecList
import Utils

sizeOfVec :: Int
sizeOfVec = 4

evalPolyVec :: Float -> [Float] -> Float
evalPolyVec value coeffs = go (splitEvery sizeOfVec coeffs) (length coeffs)
  where
    go [[x]] _    = x
    go ([a,b,c,d]:xs) len =
      let packed_coeff                     = packVector (a, b, c, d)
          vec_val                          = broadcastVector value  :: FloatX4 -- to help the type family constraint solver
          step_length                      = len - sizeOfVec
      in (go' packed_coeff vec_val step_length) + (go xs step_length)
      where
        go' pc _ 0 =
          let (x1, x2, x3, x4) = unpackVector pc
          in (x1 * value ^ 3) +
             (x2 * value ^ 2) +
             (x3 * value) + x4
        go' pc v l =
          let t = pc * v
          in go' t v (l - 1)


evalPoly :: Float -> [Float] -> Float
evalPoly value coeffs = go coeffs (length coeffs - 1)
  where
    go [] _ = 0
    go (x:xs) len = (x * (value ^ len)) + go xs (len - 1)

evalPolyFold :: Float -> [Float] -> Float
evalPolyFold value coeffs
  = let index_coeffs = (zip coeffs (reverse $ take (length coeffs) (iterate (+ 1) 0)))
     in foldr (\(c,p) v -> v + c * value^p) 0 index_coeffs

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

-- Is there any point having the typeclass?
-- Take design verdict.
dotVec :: [Float] -> [Float] -> Float
dotVec xs ys = fold   (\x y -> x + y :: FloatX4) 0 $
               zipVec (\x y -> x * y :: FloatX4) (\x y -> x * y :: Float)
               (toVecList xs)
               (toVecList ys)



-- 16  0.2 [1,4,2,3,4,1,5,6,7,8,8,9,1,3,1,5,6]
-- 32 0.2 [1,5,6,3,4,5,6,7,8,9,9,2,3,1,2,3,4,5,1,2,8,3,2,3,4,2,8,6,7,4,2,1,2]


-- write dot product

main :: IO ()
main = do
  print $ evalPolyFold 0.2 [1,1,1,1,1]
  print $ evalPoly 0.2 [1,1,1,1,1]
  print $ evalPolyVec 0.2 [1,1,1,1,1]
