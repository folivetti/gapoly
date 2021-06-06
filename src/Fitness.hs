{-# language BangPatterns #-}
{-|
Module      : Fitness
Description : Fitness Function for Genetic Algorithm for Poly Regression
Copyright   : (c) Fabricio Olivetti de Franca, 2021
License     : GPL-3
Maintainer  : fabricio.olivetti@gmail.com
Stability   : experimental
Portability : POSIX
|-}

module Fitness (evalFitness, decode) where

import GA
import Data.List.Split hiding (split)
import Numeric.LinearAlgebra ((<\>),(#>),(|||), sumElements, size, Matrix, Vector, fromRows, fromList)

-- | Creates a fitness function 
evalFitness :: Int            -- ^ number of terms
            -> [[Double]]     -- ^ matrix X
            -> Vector Double  -- ^ vector y
            -> Solution       -- ^ solution to evaluate
            -> Solution 
evalFitness nTerms xss ys sol = 
  case _fitness sol of
       Nothing -> sol{ _coeffs = Just betas, _fitness = Just f }
       _       -> sol 
  where
    zss    = decode xss (_chromo sol) 
    betas  = zss <\> ys 
    ysHat  = zss #> betas 
    f      = mse ys ysHat

-- | Decodes the polynomial into a transformed matrix 
decode :: [[Double]] -> Chromossome -> Matrix Double
decode xss chromo = fromRows (map (evalPoly chromo) xss)

-- | Eval a single row from the dataset 
evalPoly :: Chromossome -> [Double] -> Vector Double
evalPoly chromo xs = fromList $ 1.0 : go chromo 
-- zipWith (evalTerm xs) termsBools termsInts
  where
    nVars           = length xs

    go [] = []
    go cs = let (cs1, cs2) = splitAt nVars cs 
                term       = evalTerm xs cs1
               in  term `seq` term : go cs2

-- | Eval a term of the polynomial 
evalTerm :: [Double] -> [(Bool, Int)] -> Double
evalTerm xs' chromo = go chromo xs' 1
  where
    go [] _ !acc = acc
    go _ [] !acc = acc
    go ((False, _):cs) (_:xs) !acc = go cs xs acc
    go ((True,  k):cs) (x:xs) !acc = go cs xs (acc * x^k)

-- | Calculates the mean squared error
mse :: Vector Double -> Vector Double -> Double
mse ys ysHat = s / fromIntegral (size ys)
  where
    s = sumElements $ (ys - ysHat)^2
