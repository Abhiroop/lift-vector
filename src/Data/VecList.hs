{-# LANGUAGE MultiParamTypeClasses #-}

module Data.VecList
  ( VecList(..)
  , fromVecList
  , toVecList
  , fold
  , zipVec
  ) where

import Control.Monad
import Data.Maybe
import Data.Primitive
import Utils

newtype VecList a =
  VecList [a]
  deriving (Show, Eq)

toVecList :: [a] -> VecList a
toVecList = VecList

fromVecList :: VecList a -> [a]
fromVecList (VecList x) = x

class (Num a, Num b) =>
      ArithVecList a b
  where
  -- | The folding function should be commutative
  fold :: (a -> a -> a) -> (b -> b -> b) -> b -> VecList b -> b
  zipVec ::
       (a -> a -> a) -> (b -> b -> b) -> VecList b -> VecList b -> VecList b

{- | Why doesn't VecList be an instance of Foldable, Traversable and the other friendly typeclasses?

Because say you have

instance Foldable VecList where
  foldr :: (a -> b -> b) -> b -> VecList a -> b
  foldr = foldFloatX4, foldFloatX8, foldDoubleX2.....

We need
instance Foldable VecList Float FloatX4 .....

so Foldable should be of kind  (* -> * -> * -> *) -> Constraint

but all we have is (* -> *) -> Constraint

-}
instance ArithVecList FloatX4 Float where
  fold = foldFloatX4
  zipVec = zipFloatX4

instance ArithVecList DoubleX2 Double where
  fold = foldDoubleX2
  zipVec = zipDoubleX2

{-# INLINE foldFloatX4 #-}
foldFloatX4 ::
     (FloatX4 -> FloatX4 -> FloatX4)
  -> (Float -> Float -> Float)
  -> Float
  -> VecList Float
  -> Float
foldFloatX4 f g seed (VecList xs') = go seed xs'
  where
    go acc [] = acc
    go acc (x1:x2:x3:x4:y1:y2:y3:y4:xs) =
      let op = f (packVector (x1,x2,x3,x4)) (packVector (y1,y2,y3,y4))
       in go (g (foldVector g op) acc) xs
    go acc (x:xs) = go (g x acc) xs

  -- let l_1 = splitEvery 4 xs
  --     (vec_l_1, seq_l_1) = partition (\x -> length x == 4) l_1
  --     vect_l_1 = map (packVector . fromMaybe defaultTuple4 . tuplify4) vec_l_1
  --     folded_vec = foldr f (broadcastVector seed) vect_l_1
  --     folded_seq = foldr g seed (join seq_l_1)
  -- in g (foldVector g folded_vec) folded_seq

foldDoubleX2 ::
     (DoubleX2 -> DoubleX2 -> DoubleX2)
  -> (Double -> Double -> Double)
  -> Double
  -> VecList Double
  -> Double
foldDoubleX2 f g seed (VecList xs) =
  let l_1 = splitEvery 2 xs
      (vec_l_1, seq_l_1) = partition (\x -> length x == 2) l_1
      vect_l_1 = map (packVector . fromMaybe defaultTuple2 . tuplify2) vec_l_1
      folded_vec = foldr f (broadcastVector seed) vect_l_1
      folded_seq = foldr g seed (join seq_l_1)
  in g (foldVector g folded_vec) folded_seq

{-# INLINE zipFloatX4 #-}
zipFloatX4 ::
     (FloatX4 -> FloatX4 -> FloatX4)
  -> (Float -> Float -> Float)
  -> VecList Float
  -> VecList Float
  -> VecList Float
zipFloatX4 f g (VecList xs') (VecList ys') = VecList (go xs' ys')
  where
    go [] _ = []
    go _ [] = []
    go (x1:x2:x3:x4:xs) (y1:y2:y3:y4:ys) =
      let op = f (packVector (x1,x2,x3,x4)) (packVector (y1,y2,y3,y4))
          (a,b,c,d) = unpackVector op
       in a : b : c : d : (go xs ys)
    go (x:xs) (y:ys) = (g x y) : (go xs ys)

zipDoubleX2 ::
     (DoubleX2 -> DoubleX2 -> DoubleX2)
  -> (Double -> Double -> Double)
  -> VecList Double
  -> VecList Double
  -> VecList Double
zipDoubleX2 f g (VecList xs) (VecList ys) =
  let l_1 = splitEvery 2 xs
      l_2 = splitEvery 2 ys
      (vec_l_1, seq_l_1) = partition (\x -> length x == 2) l_1
      (vec_l_2, seq_l_2) = partition (\x -> length x == 2) l_2
      vect_l_1 = map (packVector . fromMaybe defaultTuple2 . tuplify2) vec_l_1
      vect_l_2 = map (packVector . fromMaybe defaultTuple2 . tuplify2) vec_l_2
      zipped_l = zipWith f vect_l_1 vect_l_2
      vec_l = concatMap (untuplify2 . unpackVector) zipped_l
      seq_l = zipWith g (join seq_l_1) (join seq_l_2)
  in VecList $ vec_l ++ seq_l
