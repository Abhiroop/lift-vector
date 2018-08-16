{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
module Data.Primitive where

import Data.Internal.FloatX4
import Data.Internal.DoubleX2

-- | The SIMD vector type class

class (Num v, Real (Elem v)) => SIMDVector v where

    type Elem v

    type ElemTuple v

    nullVector       :: v

    vectorSize       :: v -> Int
-- | The size of each element in bytes
    elementSize      :: v -> Int

    broadcastVector  :: Elem v -> v

    mapVector        :: (Elem v -> Elem v) -> v -> v

    zipVector        :: (Elem v -> Elem v -> Elem v) -> v -> v -> v

    foldVector       :: (Elem v -> Elem v -> Elem v) -> v -> Elem v

    sumVector        :: v -> Elem v
    sumVector        = foldVector (+)

    packVector       :: ElemTuple v -> v

    unpackVector     :: v -> ElemTuple v

instance Num FloatX4 where
  (+) = plusFloatX4
  (-) = minusFloatX4
  (*) = timesFloatX4
  negate = negateFloatX4
  abs    = mapVector abs
  signum = mapVector signum
  fromInteger = broadcastVector . fromInteger

instance SIMDVector FloatX4 where
  type Elem      FloatX4 = Float
  type ElemTuple FloatX4 = (Float, Float, Float, Float)
  nullVector         = broadcastVector 0
  vectorSize  _      = 4
  elementSize _      = 4
  broadcastVector    = broadcastFloatX4
  mapVector          = mapFloatX4
  zipVector          = zipFloatX4
  foldVector         = foldFloatX4
  packVector         = packFloatX4
  unpackVector       = unpackFloatX4

instance Num DoubleX2 where
  (+) = plusDoubleX2
  (-) = minusDoubleX2
  (*) = timesDoubleX2
  negate = negateDoubleX2
  abs    = mapVector abs
  signum = mapVector signum
  fromInteger = broadcastVector . fromInteger

instance SIMDVector DoubleX2 where
  type Elem      DoubleX2 = Double
  type ElemTuple DoubleX2 = (Double, Double)
  nullVector         = broadcastVector 0
  vectorSize  _      = 2
  elementSize _      = 8
  broadcastVector    = broadcastDoubleX2
  mapVector          = mapDoubleX2
  zipVector          = zipDoubleX2
  foldVector         = foldDoubleX2
  packVector         = packDoubleX2
  unpackVector       = unpackDoubleX2


-- helper functions, move to a separate module


-- sizeOfVec :: Int
-- sizeOfVec = 4

-- evalPolyVec :: Float -> [Float] -> Float
-- evalPolyVec value coeffs = go (splitEvery sizeOfVec coeffs) (length coeffs)
--   where
--     go [[x]] _    = x
--     go (x:xs) len =
--       let [(F# a), (F# b), (F# c), (F# d)] = x
--           (F# val)                         = value
--           packed_coeff                     = packFloatX4# (# a, b, c, d #)
--           vec_val                          = broadcastFloatX4# val
--           step_length                      = len - sizeOfVec
--       in (go' packed_coeff vec_val step_length) + (go xs step_length)
--       where
--         go' pc _ 0 =
--           let (# a, b, c, d #) = unpackFloatX4# pc
--           in ((F# a) * value ^ 3) +
--              ((F# b) * value ^ 2) +
--              ((F# c) * value) +
--              (F# d)
--         go' pc v l =
--           let t = (timesFloatX4# pc v)
--           in go' t v (l - 1)


-- evalPoly :: Float -> [Float] -> Float
-- evalPoly value coeffs = go coeffs (length coeffs - 1)
--   where
--     go [] _ = 0
--     go (x:xs) len = (x * (value ^ len)) + go xs (len - 1)
