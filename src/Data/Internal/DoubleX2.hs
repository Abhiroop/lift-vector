{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
module Data.Internal.DoubleX2 where

import GHC.Exts

data DoubleX2 = DX2# DoubleX2#

plusDoubleX2 :: DoubleX2 -> DoubleX2 -> DoubleX2
plusDoubleX2 (DX2# d1) (DX2# d2) = DX2# (plusDoubleX2# d1 d2)

minusDoubleX2 :: DoubleX2 -> DoubleX2 -> DoubleX2
minusDoubleX2 (DX2# d1) (DX2# d2) = DX2# (minusDoubleX2# d1 d2)

timesDoubleX2 :: DoubleX2 -> DoubleX2 -> DoubleX2
timesDoubleX2 (DX2# d1) (DX2# d2) = DX2# (timesDoubleX2# d1 d2)

negateDoubleX2 :: DoubleX2 -> DoubleX2
negateDoubleX2 (DX2# d) = DX2# (negateDoubleX2# d)

broadcastDoubleX2 :: Double -> DoubleX2
broadcastDoubleX2 (D# x) = DX2# (broadcastDoubleX2# x)

packDoubleX2 :: (Double, Double) -> DoubleX2
packDoubleX2 (D# x1, D# x2) = DX2# (packDoubleX2# (# x1, x2 #))

unpackDoubleX2 :: DoubleX2 -> (Double, Double)
unpackDoubleX2 (DX2# d) = case unpackDoubleX2# d of
    (# x1, x2 #) -> (D# x1, D# x2)

mapDoubleX2 :: (Double -> Double) -> DoubleX2 -> DoubleX2
mapDoubleX2 func d = case unpackDoubleX2 d of
  (d1, d2) -> packDoubleX2 (func d1, func d2)

zipDoubleX2 :: (Double -> Double -> Double) -> DoubleX2 -> DoubleX2 -> DoubleX2
zipDoubleX2 func d1 d2 =
  let (x1, x2) = unpackDoubleX2 d1
      (y1, y2) = unpackDoubleX2 d2
   in packDoubleX2 (func x1 y1, func x2 y2)

foldDoubleX2 :: (Double -> Double -> Double) -> DoubleX2 -> Double
foldDoubleX2 func d =
  let (x1, x2) = unpackDoubleX2 d
      func' !x !y      = func x y
   in func' x1 x2
