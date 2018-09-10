{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
module Data.Operations
  ( Array
  , Matrix
  , Shape
  , Z (..)
  , (:.) (..)
  , L.VecList
  , fold
  , zipVec
  , fmap
  , L.toVecList
  , L.fromVecList
  , toVecArray
  , fromVecArray) where

import Data.Primitive
import Data.VecArray
import Prelude hiding (fmap)

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
instance (Shape sh) => ArithVector (VecArray sh) FloatX4 Float where
  fold   = foldFloatX4
  zipVec = zipFloatX4
  fmap   = fmapFloatX4

instance (Shape sh) => ArithVector (VecArray sh) DoubleX2 Double where
  fold   = foldDoubleX2
  zipVec = zipDoubleX2
  fmap   = fmapDoubleX2

-- VecList instances below
instance ArithVector L.VecList FloatX4 Float where
  fold   = L.foldFloatX4
  zipVec = L.zipFloatX4
  fmap   = L.fmapFloatX4

instance ArithVector L.VecList DoubleX2 Double where
  fold   = L.foldDoubleX2
  zipVec = L.zipDoubleX2
  fmap   = L.fmapDoubleX2
