{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}

module Data.VecArray where

data Z = Z -- rank-0
data tail :. head = tail :. head --increase rank by 1

type DIM0 = Z
type DIM1 = DIM0 :. Int
type DIM2 = DIM1 :. Int
type DIM3 = DIM2 :. Int

type family Dim a :: *
type instance Dim Z = ()
type instance Dim (t :. h) = (Dim t, Dim h)

data Array dim e where
  Array :: Dim dim -> e -> Array dim e

-- type VecList   = Array DIM1  -- we can deprecate VecList
type VecMatrix = Array DIM2

mapMat :: (a -> b) -> VecMatrix a -> VecMatrix b
mapMat = undefined

stencilM :: (VecMatrix a -> b) -> VecMatrix a -> VecMatrix b
stencilM = undefined

reduceM :: VecMatrix a -> a
reduceM = undefined

class (Functor w) => Comonad w where
  counit :: w a -> a
  cojoin :: w a -> w (w a)

  cobind :: (w a -> b) -> w a -> w b
  cobind f = fmap f . cojoin

instance Functor VecMatrix where
  fmap = mapMat

instance Comonad VecMatrix where
  counit = reduceM
  cojoin = cobind id -- does this make any sense?????
  cobind = stencilM
{-
What does counit

-}
