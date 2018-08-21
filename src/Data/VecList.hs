{-# LANGUAGE MultiParamTypeClasses #-}
module Data.VecList
  ( VecList   (..)
  --, ArithExpr (..)
  , fromVecList
  , toVecList
  , fold
  , zipVec) where

import Control.Monad
import Data.Primitive
import Data.Maybe
import Utils
import Prelude hiding (foldr)


newtype VecList a = VecList [a]
  deriving (Show, Eq)

-- The vectorized operators are represented as :operator:
-- The sequential operators are represented as :operator
-- data ArithExpr a = :+:
--                  | :*:
--                  | :^:
--                  | :+
--                  | :*
--                  | :^
--                  --  | ArithExpr a

-- type Func a = a -> a -> a

-- evalArithExpr :: Num b => ArithExpr a -> Func b
-- evalArithExpr (e1 :+: e2) = (+)
-- evalArithExpr (e1 :*: e2) = (*)


toVecList :: [a] -> VecList a
toVecList = VecList

fromVecList :: VecList a -> [a]
fromVecList (VecList x) = x

foldFloatX4 :: (FloatX4 -> FloatX4 -> FloatX4)
            -> Float
            -> VecList a
            -> Float
foldFloatX4 f seed (VecList xs) =
  let splits = splitEvery 4 xs
   in undefined

-- foldDouble :: (a -> Double -> ArithExpr Double)
--            -> Double
--            -> VecList a
--            -> Double
-- foldDouble = undefined

zipFloatX4 :: (FloatX4 -> FloatX4 -> FloatX4)
           -> (Float   -> Float   -> Float)
           -> VecList Float
           -> VecList Float
           -> VecList Float
zipFloatX4 f g (VecList xs) (VecList ys) =
  let l_1 = splitEvery 4 xs
      l_2 = splitEvery 4 ys
      (vec_l_1, seq_l_1) = partition (\x -> length x == 4) l_1
      (vec_l_2, seq_l_2) = partition (\x -> length x == 4) l_2
      vect_l_1 = map (packVector . fromMaybe defaultTuple4 . tuplify4) vec_l_1
      vect_l_2 = map (packVector . fromMaybe defaultTuple4 . tuplify4) vec_l_2
      zipped_l = zipWith f vect_l_1 vect_l_2
      vec_l = concatMap ( fromMaybe defaultList4 . untuplify4 . unpackVector) zipped_l
      seq_l = zipWith g (join seq_l_1) (join seq_l_2)
   in VecList $ vec_l ++ seq_l

zipDouble :: DoubleX2 -> DoubleX2 -> DoubleX2
          -> VecList Double
          -> VecList Double
          -> VecList Double
zipDouble = undefined
-- This typeclass allows you to polymorphic on the type of Float, Double, Int8..etc
-- The Veclist constructor will allow to determine the size of the vector
class (Num a, Num b) => ArithVecList a b where
  -- | The folding function should be commutative
  fold      :: (a -> a -> a) -> b -> VecList b -> b
  zipVec    :: (a -> a -> a) -> (b -> b -> b) -> VecList b -> VecList b -> VecList b

instance ArithVecList FloatX4 Float where
  fold = foldFloatX4
  zipVec = zipFloatX4

-- instance ArithVecList Double where
--   fold = foldDouble
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
-- foldr :: (Float -> Float -> Float)
--       -> Float
--       -> VecList Float
--       -> Float
-- foldr f start (VecList n l) =
--   -- l = 16 elements
--   let splits = splitEvery n l
--   -- splits = 4 elements each a list of length 4
--       splitTup = map listtoTuple4 splits
--       -- splitTup = 4 elems each a 4 tup
--       veclist  = map packVector splitTup :: [FloatX4]
--       -- vec list - 4 FloatX4s
--       finalList = map (foldVector f) veclist
--       -- finalList = list of 4 floats
--    in undefined
