{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
module Main where

import Control.Monad (forM)
import Control.DeepSeq
import Data.Primitive
import Data.Operations
import Data.List (unfoldr,intercalate)
import System.CPUTime (getCPUTime)
import System.Random

import qualified Data.VecList as L

import Prelude hiding (fmap)
import Matrixmultiplication
import Pearson
import Polynomial
import Dotproduct
import Utils

-- This module is for experiments only

-- We assume the function we are timing is an IO monad computation
timeIt :: (Fractional c, NFData b) => (a -> IO b) -> a -> IO c
timeIt action arg = do
  startTime <- getCPUTime
  m <- action arg
  finishTime <- m `deepseq` getCPUTime
  return $ fromIntegral (finishTime - startTime) / 1000000000000

-- Version for use with evaluating regular non-monadic functions
timeIt_ :: (Fractional c) => (a -> b) -> a -> IO c
timeIt_ f = timeIt ((`seq` return ()) . f)

forceElements :: [a] -> ()
forceElements = foldr seq ()
-- Desired DSL
-- evalPolyVector :: Float -> [Float] -> Float
-- evalPolyVector value coeffs
--   = let index_coeffs = (zip coeffs (reverse $ take (length coeffs) (iterate (+ 1) 0)))
--      in fold (\(c,p) v -> ArithExpr v :+
--                           (ArithExpr c :*:
--                            (ArithExpr value :^: ArithExpr p)))
--         0
--         (toVecList index_coeffs)

multiply :: [Float] -> Float
multiply xs =
  fold (\x y -> x * y :: FloatX4) (\x y -> x * y :: Float) 0 (toVecList xs)

-- 16  0.2 [1,4,2,3,4,1,5,6,7,8,8,9,1,3,1,5,6]
-- 32 0.2 [1,5,6,3,4,5,6,7,8,9,9,2,3,1,2,3,4,5,1,2,8,3,2,3,4,2,8,6,7,4,2,1,2]
-- write dot product

foo1 :: Float
foo1 =
  fold
    (\x y -> x + y :: FloatX4)
    (\x y -> x + y :: Float)
    0
    (L.toVecList [1 .. 15000000])

foo2 :: Float
foo2 = foldr (+) 0 [1 .. 15000000]

bar1 :: Array Int
bar1 = toVecArray (Z :. 10) [1..10]

bar2 :: Matrix Int
bar2 = toVecArray (Z :. 3 :. 5) [1..15]

foo3 :: Matrix Float
foo3 = toVecArray (Z :. 4 :. 4) [1..16]

foo4 :: Matrix Float
foo4 = fmap (\ x -> x + 1 :: FloatX4)
            (\ x -> x + 1 :: Float)
            foo3

randomlist :: Int -> StdGen -> [Float]
randomlist n = take n . unfoldr (Just . random)


forceMatMult :: [[Float]] -> [[Float]] -> ()
forceMatMult xs ys = let z = matmult xs ys
                         x = map forceElements z
                         q = forceElements x
                     in q `deepseq` ()

forceMatMultVec :: [[Float]] -> [[Float]] -> ()
forceMatMultVec xs ys = let z = matmultVec xs ys
                            x = map forceElements z
                            q = forceElements x
                        in q `deepseq` ()

forceMatMultVec' :: [[Float]] -> [[Float]] -> ()
forceMatMultVec' xs ys = let z = matmultVec' xs ys
                             x = map forceElements z
                             q = forceElements x
                         in q `deepseq` ()

main :: IO ()
main = do
  -- let heading = "List,Vector List,Vector Array,Time in seconds\n"
  -- seed1 <- newStdGen
  -- seed2 <- newStdGen

  -- --let datasize = [10,100,1000,10000,100000]--,1000000,2000000,3000000,4000000,5000000]
  -- let datasize = [4000000,5000000,10000000]--,1000000,2000000,3000000,4000000,5000000]
  -- ts <- forM datasize $ \i -> do
  --         let r1 = randomlist i seed1
  --             r2 = randomlist i seed2
  --         x <- timeIt_ (dotp'   r1) r2
  --         y <- timeIt_ (dotVec  r1) r2
  --         z <- timeIt_ (dotVec' r1) r2
  --         return $ (show x) <> "," <> (show y) <>"," <> (show z) <> "," <> (show i)
  -- writeFile "/Users/abhiroop/Desktop/test.csv" $ heading <> (intercalate "\n" ts)


  -- let heading = "List,Vector List,Vector Array,Time in seconds\n"
  -- seed1 <- newStdGen
  -- seed2 <- newStdGen

  -- let mat_rows = 1000
  -- let datasize = [1 .. mat_rows]
  -- mat1 <- forM datasize $ \_ -> do
  --            seed <- newStdGen
  --            let r = randomlist mat_rows seed
  --            return r

  -- mat2 <- forM datasize $ \_ -> do
  --            seed <- newStdGen
  --            let r = randomlist mat_rows seed
  --            return r

  -- x <- timeIt_ (forceMatMult mat1) mat2
  -- y <- timeIt_ (forceMatMultVec mat1) mat2
  -- z <- timeIt_ (forceMatMultVec' mat1) mat2
  -- let csv = (show x) <> "," <> (show y) <>"," <> (show z) <> "," <> (show mat_rows)
  -- writeFile "/Users/abhiroop/Desktop/test.csv" $ heading <> csv

  let heading = "List,Lift Vector,Unlifted Type,Time in seconds\n"
  seed <- newStdGen
  let datasize = [513,1025,2049,4097, 8193]--,16393,49177,98353]--,1000000,2000000,3000000,4000000,5000000]
  ts <- forM datasize $ \i -> do
          num <- randomIO :: IO Float
          let r = randomlist i seed
          x <- timeIt_ (evalPolyFold num) r
          y <- timeIt_ (evalPolyVec  num) r
          z <- timeIt_ (evalPolyVec' num) r
          return $ (show x) <> "," <> (show y) <>"," <> (show z) <> "," <> (show i)
  writeFile "/Users/abhiroop/Desktop/test.csv" $ heading <> (intercalate "\n" ts)




  -- let datasize = 800000
  -- x <- timeIt_ (pearson [1..800000]) [800001..1600000]
  -- y <- timeIt_ (pearsonVec [1..800000]) [800001..1600000]
  -- z <- timeIt_ (pearsonVec [1..800000]) [1600001..2400000]
  -- let heading = "foo,foo1,foo2,bar,\n"
  -- let foo = (show x) <> "," <> (show y) <>"," <> (show z) <> "," <> (show datasize) <> "\n"
  --     bar = (show z) <> "," <> (show y) <>"," <> (show x) <> "," <> (show 900000) <> "\n"
  --     baz = (show y) <> "," <> (show x) <>"," <> (show z) <> "," <> (show 1000000) <> "\n"
  -- writeFile "/Users/abhiroop/Desktop/test.csv" (heading <> foo <> bar <> baz)
  --print $ bar2 ! (Z :. 1 :. 1)
  --print $ foo4
  --print $ dotp [1..4] [5..8]
  --print $ dotVec' [1..320] [321..640]
  --print $ pearsonVec [1..800000] [800001..1600000] 
  --print $ pearsonVec [1..4] [1005,-1036,711,18]
  --print $ dotVec' [1..4] [5..8]
  --print $ foo1
  -- let l    = replicate 80000 7
  --     arr1 = replicate 80000 l
  --     m    = replicate 80000 8
  --     arr2 = replicate 80000 m
  --     !r   = matmultVec arr1 arr2
  -- print $
  --   (last $ last $ r)
  -- print $
  --   matmult [[2, 3, 4, 5], [6, 7, 8, 9], [10, 11, 12, 13], [14, 15, 16, 17]]
  --           [[1, 7, 8, 9], [15, 14, 13, 11], [9, 19, 7, 8], [4, 5, 6, 7]]
  -- print $
  --   matmultVec [[2, 3, 4, 5], [6, 7, 8, 9], [10, 11, 12, 13], [14, 15, 16, 17]]
  --              [[1, 7, 8, 9], [15, 14, 13, 11], [9, 19, 7, 8], [4, 5, 6, 7]]
  -- print $ evalPolyFold 0.2 [1, 1, 1, 1, 1]
  -- print $ evalPoly 0.2 [1, 1, 1, 1, 1]
  -- print $ evalPolyVec 0.2 [1, 1, 1, 1, 1]
  -- print $ dotVec [1..10000000] [1..10000000]
  -- print $ dotVec [1,2,3,4,5] [1,2,3,4,5]
  -- print $ foo1
  --print $ foo2
