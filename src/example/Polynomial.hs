{-# LANGUAGE TypeFamilies #-}
module Polynomial where

import Data.Primitive
import Utils

-- vectorised form
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

-- scalar form
evalPoly :: Float -> [Float] -> Float
evalPoly value coeffs = go coeffs (length coeffs - 1)
  where
    go [] _ = 0
    go (x:xs) len = (x * (value ^ len)) + go xs (len - 1)

evalPolyFold :: Float -> [Float] -> Float
evalPolyFold value coeffs
  = let index_coeffs = (zip coeffs (reverse $ take (length coeffs) (iterate (+ 1) 0)))
     in foldr (\(c,p) v -> v * c^p) 0 index_coeffs
