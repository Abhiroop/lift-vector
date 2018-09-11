module Pearson where

import Data.Operations
import Data.Primitive

{-| Pearson Correlation coefficient

                          n * Σ(x_i * y_i) - (Σx_i * Σy_i)
r =  ----------------------------------------------------------------------------
      (sqrt (n * Σ(x_i ^ 2) - (Σ(x_i))^2)) * (sqrt (n * Σ(y_i ^ 2) - (Σ(y_i))^2))
-}
-- vectorised form
pearsonVec :: [Float] -> [Float] -> Float
pearsonVec xs ys
  | (length xs) /= (length ys) = error "Incorrect dataset"
  | otherwise =
    let n = fromIntegral (length xs) :: Float
        num = (n * (sumVecF (zipMultF xs ys))) - ((sumVecF xs) * (sumVecF ys))
        denom1 = sqrt $ (n * (sumVecF (zipMultF xs xs))) - ((sumVecF xs) ^ 2)
        denom2 = sqrt $ (n * (sumVecF (zipMultF ys ys))) - ((sumVecF ys) ^ 2)
     in num / (denom1 * denom2)

sumVecF :: [Float] -> Float
sumVecF xs =
  fold (\x y -> x + y :: FloatX4) (\x y -> x + y :: Float) 0 (toVecList xs)

zipMultF :: [Float] -> [Float] -> [Float]
zipMultF xs ys =
  fromVecList $
  (zipVec
     (\x y -> x * y :: FloatX4)
     (\x y -> x * y :: Float)
     (toVecList xs)
     (toVecList ys))

pearsonVec' :: [Float] -> [Float] -> Float
pearsonVec' xs ys
  | (length xs) /= (length ys) = error "Incorrect dataset"
  | otherwise =
    let n = fromIntegral (length xs) :: Float
        num =
          (n * (sumVecF' (zipMultF' xs ys))) - ((sumVecF' xs) * (sumVecF' ys))
        denom1 = sqrt $ (n * (sumVecF' (zipMultF' xs xs))) - ((sumVecF' xs) ^ 2)
        denom2 = sqrt $ (n * (sumVecF' (zipMultF' ys ys))) - ((sumVecF' ys) ^ 2)
     in num / (denom1 * denom2)

sumVecF' :: [Float] -> Float
sumVecF' xs =
  fold
    (\x y -> x + y :: FloatX4)
    (\x y -> x + y :: Float)
    0
    (toVecArray (Z :. (length xs)) xs)

zipMultF' :: [Float] -> [Float] -> [Float]
zipMultF' xs ys =
  let l = length xs
   in fromVecArray $
      (zipVec
         (\x y -> x * y :: FloatX4)
         (\x y -> x * y :: Float)
         (toVecArray (Z :. l) xs)
         (toVecArray (Z :. l) ys))

-- scalar form
pearson :: [Float] -> [Float] -> Float
pearson xs ys
  | (length xs) /= (length ys) = error "Incorrect dataset"
  | otherwise =
    let n = fromIntegral (length xs) :: Float
        num = (n * (sum (zipWith (*) xs ys))) - ((sum xs) * (sum ys))
        denom1 = sqrt $ (n * (sum (map (^ 2) xs))) - ((sum xs) ^ 2)
        denom2 = sqrt $ (n * (sum (map (^ 2) ys))) - ((sum ys) ^ 2)
     in num / (denom1 * denom2)
