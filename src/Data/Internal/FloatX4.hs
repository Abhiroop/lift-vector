{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
module Data.Internal.FloatX4 where

import GHC.Exts

data FloatX4  = FX4# FloatX4#

plusFloatX4 :: FloatX4 -> FloatX4 -> FloatX4
plusFloatX4 (FX4# f1) (FX4# f2) = FX4# (plusFloatX4# f1 f2)

minusFloatX4 :: FloatX4 -> FloatX4 -> FloatX4
minusFloatX4 (FX4# f1) (FX4# f2) = FX4# (minusFloatX4# f1 f2)

timesFloatX4 :: FloatX4 -> FloatX4 -> FloatX4
timesFloatX4 (FX4# f1) (FX4# f2) = FX4# (timesFloatX4# f1 f2)

negateFloatX4 :: FloatX4 -> FloatX4
negateFloatX4 (FX4# f) = FX4# (negateFloatX4# f)

broadcastFloatX4 :: Float -> FloatX4
broadcastFloatX4 (F# x) = FX4# (broadcastFloatX4# x)

packFloatX4 :: (Float, Float, Float, Float) -> FloatX4
packFloatX4 (F# x1, F# x2, F# x3, F# x4) = FX4# (packFloatX4# (# x1, x2, x3, x4 #))

unpackFloatX4 :: FloatX4 -> (Float, Float, Float, Float)
unpackFloatX4 (FX4# f) = case unpackFloatX4# f of
    (# x1, x2, x3, x4 #) -> (F# x1, F# x2, F# x3, F# x4)

mapFloatX4 :: (Float -> Float) -> FloatX4 -> FloatX4
mapFloatX4 func f = case unpackFloatX4 f of
  (f1, f2, f3, f4) -> packFloatX4 (func f1, func f2, func f3, func f4)

zipFloatX4 :: (Float -> Float -> Float) -> FloatX4 -> FloatX4 -> FloatX4
zipFloatX4 func f1 f2 =
  let (x1, x2, x3, x4) = unpackFloatX4 f1
      (y1, y2, y3, y4) = unpackFloatX4 f2
   in packFloatX4 (func x1 y1, func x2 y2, func x3 y3, func x4 y4)

foldFloatX4 :: (Float -> Float -> Float) -> FloatX4 -> Float
foldFloatX4 func f =
  let (x1, x2, x3, x4) = unpackFloatX4 f
      func' !x !y      = func x y
   in x1 `func'` x2 `func'` x3 `func'` x4
