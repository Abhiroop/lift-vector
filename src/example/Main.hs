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


import Prelude hiding (fmap)
import Matrixmultiplication
import Pearson
import Polynomial
import Dotproduct

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

  -- let heading = "List,Lift Vector,Unlifted Type,Time in seconds\n"
  -- seed <- newStdGen
  -- let datasize = [2000,3000,5000,7000,9000]
  -- ts <- forM datasize $ \i -> do
  --         num <- randomIO :: IO Float
  --         let r = randomlist i seed
  --         x <- timeIt_ (evalPoly num) r
  --         y <- timeIt_ (evalPolyVec  num) r
  --         z <- timeIt_ (evalPolyVec' num) r
  --         return $ (show x) <> "," <> (show y) <>"," <> (show z) <> "," <> (show i)
  -- writeFile "/Users/abhiroop/Desktop/test.csv" $ heading <> (intercalate "\n" ts)

  let heading = "List,Vector List,Vector Array,Time in seconds\n"
  seed1 <- newStdGen
  seed2 <- newStdGen
  let datasize = [8000000]--[10000,20000,50000,100000,500000]
  ts <- forM datasize $ \i -> do
          let r1 = randomlist i seed1
              r2 = randomlist i seed2
          x <- timeIt_ (pearson r1) r2
          y <- timeIt_ (pearsonVec  r1) r2
          z <- timeIt_ (pearsonVec' r1) r2
          return $ (show x) <> "," <> (show y) <>"," <> (show z) <> "," <> (show i)
  writeFile "/Users/abhiroop/Desktop/test.csv" $ heading <> (intercalate "\n" ts)
