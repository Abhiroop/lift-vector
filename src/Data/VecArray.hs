{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}

module Data.VecArray where

import Data.Primitive
import GHC.Base (quotInt, remInt)

import qualified Data.Vector.Unboxed as U

data VecArray sh a = VecArray !sh (U.Vector a)

-- | index of the element O(1)
{-# INLINE linearIndex #-}
linearIndex :: (Shape sh, U.Unbox e) => VecArray sh e -> Int -> e
linearIndex (VecArray _ vec) ix = vec U.! ix

{-# INLINE slice #-}
slice :: (Shape sh, U.Unbox e) =>
         Int
      -> Int
      -> VecArray sh e
      -> VecArray sh e
slice i j (VecArray sh vec) = VecArray sh (U.slice i j vec)

-- | size of the array
{-# INLINE shape #-}
shape :: (Shape sh, U.Unbox e) => VecArray sh e -> sh
shape (VecArray sh _) = sh

-- | element at an index given by shape. O(1)
{-# INLINE (!) #-}
(!) :: (Shape sh, U.Unbox e) => VecArray sh e -> sh -> e
(!) arr ix = arr `linearIndex` toIndex (shape arr) ix

{-# INLINE deepSeqArray #-}
deepSeqArray :: (Shape sh, U.Unbox e) => VecArray sh e -> b -> b
deepSeqArray (VecArray sh vec) x
  = sh `deepSeq` vec `seq` x

{-# INLINE toVecArray #-}
toVecArray :: (Shape sh, U.Unbox e)
         => sh -> [e] -> VecArray sh e
toVecArray sh xs
 = let len = length xs
    in if len /= size sh
       then error "Mismatch between the length of the list and the shape provided!"
       else VecArray sh (U.fromList xs)

{-# INLINE fromVecArray #-}
fromVecArray :: (Shape sh, U.Unbox e)
         => VecArray sh e -> [e]
fromVecArray (VecArray _ vec) = U.toList vec

type Array  = VecArray DIM1
type Matrix = VecArray DIM2

foldFloatX4 :: (Shape sh) =>
               (FloatX4 -> FloatX4 -> FloatX4)
            -> (Float   -> Float   -> Float)
            -> Float
            -> VecArray sh Float
            -> Float
foldFloatX4 = undefined

zipFloatX4 :: (Shape sh) =>
              (FloatX4 -> FloatX4 -> FloatX4)
           -> (Float   -> Float   -> Float)
           -> VecArray sh Float
           -> VecArray sh Float
           -> VecArray sh Float
zipFloatX4 = undefined


fmapFloatX4 :: (Shape sh) =>
  (FloatX4 -> FloatX4) -> (Float -> Float) -> VecArray sh Float -> VecArray sh Float
fmapFloatX4 f g (VecArray sh vec) = toVecArray sh $ go (size sh) 0
  where
    go s i
      | (i + 4) < s =
        let e1 = vec U.! i
            e2 = vec U.! (i + 1)
            e3 = vec U.! (i + 2)
            e4 = vec U.! (i + 3)
            vect = packVector (e1,e2,e3,e4) :: FloatX4
            op = f vect
            (x1, x2, x3, x4) = unpackVector op
         in x1 : x2 : x3 : x4 : go s (i + 4)
      | i == s = []
      | otherwise = (g $ vec U.! i) : go s (i + 1)



-------------Shape Polymorphism from Repa/Accelerate/Ypnos etc-----------------

data Z = Z
  deriving (Show, Read, Eq, Ord) -- rank-0

data tail :. head = tail :. head
  deriving (Show, Read, Eq, Ord) --increase rank by 1

type DIM0 = Z
type DIM1 = DIM0 :. Int
type DIM2 = DIM1 :. Int
type DIM3 = DIM2 :. Int

class Eq sh => Shape sh where
        -- | Get the number of dimensions in a shape.
        rank    :: sh -> Int

        -- | The shape of an array of size zero, with a particular dimensionality.
        zeroDim :: sh

        -- | The shape of an array with size one, with a particular dimensionality.
        unitDim :: sh

        -- | Compute the intersection of two shapes.
        intersectDim :: sh -> sh -> sh

        -- | Add the coordinates of two shapes componentwise
        addDim  :: sh -> sh -> sh

        -- | Get the total number of elements in an array with this shape.
        size    :: sh -> Int

        -- | Check whether this shape is small enough so that its flat
        --      indices an be represented as `Int`. If this returns `False` then your
        --      array is too big. Mostly used for writing QuickCheck tests.
        sizeIsValid :: sh -> Bool


        -- | Convert an index into its equivalent flat, linear, row-major version.
        toIndex :: sh   -- ^ Shape of the array.
                -> sh   -- ^ Index into the array.
                -> Int

        -- | Inverse of `toIndex`.
        fromIndex
                :: sh   -- ^ Shape of the array.
                -> Int  -- ^ Index into linear representation.
                -> sh

        -- | Check whether an index is within a given shape.
        inShapeRange
                :: sh   -- ^ Start index for range.
                -> sh   -- ^ Final index for range.
                -> sh   -- ^ Index to check for.
                -> Bool

        -- | Convert a shape into its list of dimensions.
        listOfShape     :: sh -> [Int]

        -- | Convert a list of dimensions to a shape
        shapeOfList     :: [Int] -> sh

        -- | Ensure that a shape is completely evaluated.
        infixr 0 `deepSeq`
        deepSeq :: sh -> a -> a

instance Shape Z where
        {-# INLINE [1] rank #-}
        rank _                  = 0

        {-# INLINE [1] zeroDim #-}
        zeroDim                 = Z

        {-# INLINE [1] unitDim #-}
        unitDim                 = Z

        {-# INLINE [1] intersectDim #-}
        intersectDim _ _        = Z

        {-# INLINE [1] addDim #-}
        addDim _ _              = Z

        {-# INLINE [1] size #-}
        size _                  = 1

        {-# INLINE [1] sizeIsValid #-}
        sizeIsValid _           = True


        {-# INLINE [1] toIndex #-}
        toIndex _ _             = 0

        {-# INLINE [1] fromIndex #-}
        fromIndex _ _           = Z


        {-# INLINE [1] inShapeRange #-}
        inShapeRange Z Z Z      = True

        {-# NOINLINE listOfShape #-}
        listOfShape _           = []

        {-# NOINLINE shapeOfList #-}
        shapeOfList []          = Z
        shapeOfList _           = error $ "non-empty list when converting to Z."

        {-# INLINE deepSeq #-}
        deepSeq Z x             = x


instance Shape sh => Shape (sh :. Int) where
        {-# INLINE [1] rank #-}
        rank   (sh  :. _)
                = rank sh + 1

        {-# INLINE [1] zeroDim #-}
        zeroDim = zeroDim :. 0

        {-# INLINE [1] unitDim #-}
        unitDim = unitDim :. 1

        {-# INLINE [1] intersectDim #-}
        intersectDim (sh1 :. n1) (sh2 :. n2)
                = (intersectDim sh1 sh2 :. (min n1 n2))

        {-# INLINE [1] addDim #-}
        addDim (sh1 :. n1) (sh2 :. n2)
                = addDim sh1 sh2 :. (n1 + n2)

        {-# INLINE [1] size #-}
        size  (sh1 :. n)
                = size sh1 * n

        {-# INLINE [1] sizeIsValid #-}
        sizeIsValid (sh1 :. n)
                | size sh1 > 0
                = n <= maxBound `div` size sh1

                | otherwise
                = False

        {-# INLINE [1] toIndex #-}
        toIndex (sh1 :. sh2) (sh1' :. sh2')
                = toIndex sh1 sh1' * sh2 + sh2'

        {-# INLINE [1] fromIndex #-}
        fromIndex (ds :. d) n
                = fromIndex ds (n `quotInt` d) :. r
                where
                -- If we assume that the index is in range, there is no point
                -- in computing the remainder for the highest dimension since
                -- n < d must hold. This saves one remInt per element access which
                -- is quite a big deal.
                r       | rank ds == 0  = n
                        | otherwise     = n `remInt` d

        {-# INLINE [1] inShapeRange #-}
        inShapeRange (zs :. z) (sh1 :. n1) (sh2 :. n2)
                = (n2 >= z) && (n2 < n1) && (inShapeRange zs sh1 sh2)

        {-# NOINLINE listOfShape #-}
        listOfShape (sh :. n)
         = n : listOfShape sh

        {-# NOINLINE shapeOfList #-}
        shapeOfList xx
         = case xx of
                []      -> error $ "empty list when converting to  (_ :. Int)"
                x:xs    -> shapeOfList xs :. x

        {-# INLINE deepSeq #-}
        deepSeq (sh :. n) x = deepSeq sh (n `seq` x)


---------------------------------------------------------------------------------------



------------------------------------------------

-- type family Dim a :: *
-- type instance Dim Z = ()
-- type instance Dim (t :. h) = (Dim t, Dim h)

-- data Array dim e where
--   Array :: UArray (Dim dim) e
--         -> Array dim e

-- -- type VecList   = Array DIM1  -- we can deprecate VecList
-- type VecArray  = Array DIM1
-- type VecMatrix = Array DIM2


-- mapMat :: (a -> b) -> VecMatrix a -> VecMatrix b
-- mapMat = undefined

-- stencilM :: (VecMatrix a -> b) -> VecMatrix a -> VecMatrix b
-- stencilM = undefined

-- reduceM :: VecMatrix a -> a
-- reduceM = undefined

-- class (Functor w) => Comonad w where
--   counit :: w a -> a
--   cojoin :: w a -> w (w a)

--   cobind :: (w a -> b) -> w a -> w b
--   cobind f = fmap f . cojoin

-- instance Functor VecMatrix where
--   fmap = mapMat

-- instance Comonad VecMatrix where
--   counit = reduceM
--   cojoin = cobind id -- does this make any sense?????
--   cobind = stencilM
-- {-
-- What does counit represent

-- -}
