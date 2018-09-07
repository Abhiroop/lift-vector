{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
module Data.Operations
  ( A.Array
  , A.Matrix
  , A.Shape
  , A.Z
  , L.VecList
  , fold
  , zipVec
  , fmap
  , L.toVecList
  , L.fromVecList
  , A.toVecArray
  , A.fromVecArray) where

import Data.Primitive
import Prelude hiding (fmap)

import qualified Data.VecArray as A
import qualified Data.VecList  as L

-- | This module provide an abstraction over all operations on vector data structures.

class (Num a, Num b) =>
      ArithVector t a b
  where
  -- | The folding function should be commutative.
  fold :: (a -> a -> a) -> (b -> b -> b) -> b -> t b -> b
  zipVec ::
       (a -> a -> a) -> (b -> b -> b) -> t b -> t b -> t b
  fmap :: (a -> a) -> (b -> b) -> t b -> t b

-- VecArray instances below
instance (A.Shape sh) => ArithVector (A.VecArray sh) FloatX4 Float where
  fold   = A.foldFloatX4
  zipVec = A.zipFloatX4
  fmap   = A.fmapFloatX4

instance (A.Shape sh) => ArithVector (A.VecArray sh) DoubleX2 Double where
  fold   = A.foldDoubleX2
  zipVec = A.zipDoubleX2
  fmap   = A.fmapDoubleX2

-- VecList instances below
instance ArithVector L.VecList FloatX4 Float where
  fold   = L.foldFloatX4
  zipVec = L.zipFloatX4
  fmap   = L.fmapFloatX4

instance ArithVector L.VecList DoubleX2 Double where
  fold   = L.foldDoubleX2
  zipVec = L.zipDoubleX2
  fmap   = L.fmapDoubleX2
