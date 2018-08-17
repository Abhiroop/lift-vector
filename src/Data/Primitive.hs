{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
module Data.Primitive
  ( FloatX4,
    DoubleX2,

    nullVector,
    vectorSize,
    elementSize,
    broadcastVector,
    mapVector,
    zipVector,
    foldVector,
    sumVector,
    packVector,
    unpackVector
  ) where

import Data.Internal.FloatX4
import Data.Internal.DoubleX2

-- | The SIMD vector type class
class (Num v, Real (Elem v)) => SIMDVector v where

    type Elem v

    type ElemTuple v

    nullVector       :: v
-- | The number of scalar element inside each vector
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
