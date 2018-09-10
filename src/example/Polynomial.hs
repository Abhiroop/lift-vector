{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE TypeFamilies #-}
module Polynomial where

import Debug.Trace
import Data.Primitive
import Utils

import GHC.Exts
-- vectorised form
sizeOfVec :: Int
sizeOfVec = 4

evalPolyVec :: Float -> [Float] -> Float
evalPolyVec value coeffs = go (broadcastVector value) (splitEvery sizeOfVec coeffs) (length coeffs)
  where
    go _ [[x]] _     = x
    go _ [[x,y]] _   = x + y
    go _ [[x,y,z]] _ = x + y + z
    go vec_val ([a,b,c,d]:xs) len =
      let packed_coeff                     = packVector (a, b, c, d) :: FloatX4
          step_length                      = len - sizeOfVec
      in (go' packed_coeff vec_val step_length) + (go vec_val xs step_length)
      where
        go' pc _ 0 =
          let (x1, x2, x3, x4) = unpackVector pc
          in (x1 * value ^ 3) +
             (x2 * value ^ 2) +
             (x3 * value) + x4
        go' pc v l =
          let t = pc * v
          in go' t v (l - 1)

-- The unlifted variant solely exists for the purpose of benchmarking
evalPolyVec' :: Float -> [Float] -> Float
evalPolyVec' value@(F# val) coeffs = go (broadcastFloatX4# val) (splitEvery sizeOfVec coeffs) (length coeffs)
  where
    go _ [[x]] _    = x
    go vec_val (x:xs) len =
      let [(F# a), (F# b), (F# c), (F# d)] = x
          packed_coeff                     = packFloatX4# (# a, b, c, d #)
          step_length                      = len - sizeOfVec
      in (go' packed_coeff vec_val step_length) + (go vec_val xs step_length)
      where
        go' pc _ 0 =
          let (# a, b, c, d #) = unpackFloatX4# pc
          in ((F# a) * value ^ 3) +
             ((F# b) * value ^ 2) +
             ((F# c) * value) +
             (F# d)
        go' pc v l =
          let t = (timesFloatX4# pc v)
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
