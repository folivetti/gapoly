{-|
Module      : Fitness
Description : Fitness Function for Genetic Algorithm for Poly Regression
Copyright   : (c) Fabricio Olivetti de Franca, 2021
License     : GPL-3
Maintainer  : fabricio.olivetti@gmail.com
Stability   : experimental
Portability : POSIX
|-}

module Fitness (evalFitness) where

import GA
import Data.List.Split hiding (split)
import Numeric.LinearAlgebra ((<\>),(#>),(|||), sumElements, size, Matrix, Vector, fromRows, fromList)

-- | Creates a fitness function 
evalFitness :: Int            -- ^ number of terms
            -> [[Double]]     -- ^ matrix X
            -> Vector Double  -- ^ vector y
            -> [Bool]         -- ^ variables of the poly
            -> [Int]          -- ^ exponents of the poly
            -> Solution 
evalFitness nTerms xss ys vecBool vecInt = Sol vecBool vecInt betas f
  where
    zss    = decode xss vecBool vecInt
    betas  = zss <\> ys 
    ysHat  = zss #> betas 
    f      = mse ys ysHat

-- | Decodes the polynomial into a transformed matrix 
decode :: [[Double]] -> [Bool] -> [Int] -> Matrix Double
decode xss vecBool vecInt = 1.0 ||| fromRows (map (evalPoly vecBool vecInt) xss)

-- | Eval a single row from the dataset 
evalPoly :: [Bool] -> [Int] -> [Double] -> Vector Double
evalPoly vecBool vecInt xs = fromList $ zipWith (evalTerm xs) termsBools termsInts
  where
    nVars           = length xs
    termsBools      = chunksOf nVars vecBool
    termsInts       = chunksOf nVars vecInt

-- | Eval a term of the polynomial 
evalTerm :: [Double] -> [Bool] -> [Int] -> Double
evalTerm xs' vecBool vecInt = go vecBool vecInt xs' 1
  where
    go [] _ _ acc = acc
    go _ [] _ acc = acc
    go _ _ [] acc = acc
    go (False:bs) (k:ks) (x:xs) acc = go bs ks xs acc
    go (True:bs)  (k:ks) (x:xs) acc = go bs ks xs (acc * x^k)

-- | Calculates the mean squared error
mse :: Vector Double -> Vector Double -> Double
mse ys ysHat = s / fromIntegral (size ys)
  where
    s = sumElements $ (ys - ysHat)^2
