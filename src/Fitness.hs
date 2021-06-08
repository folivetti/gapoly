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

import Data.List (foldl')
import Data.List.Split hiding (split)
import Numeric.LinearAlgebra ((<\>),(#>),(|||), sumElements, size, Matrix, Vector, fromRows, fromList)

-- | Creates a fitness function 
evalFitness :: Int            -- ^ number of terms
            -> [[Double]]     -- ^ matrix X
            -> Vector Double  -- ^ vector y
            -> Solution (Poly n)       -- ^ solution to evaluate
            -> Solution (Poly n) 
evalFitness nTerms xss ys sol = 
  case _fitness sol of
       Nothing -> sol{ _coeffs = Just betas, _fitness = Just f }
       _       -> sol 
  where
    zss    = decode xss (map _getPoly $ _chromo sol) 
    betas  = zss <\> ys 
    ysHat  = zss #> betas 
    f      = mse ys ysHat

-- | Decodes the polynomial into a transformed matrix 
decode :: [[Double]] -> [(Bool, Int)] -> Matrix Double
decode xss chromo = fromRows (map (evalPoly chromo) xss)

-- | Eval a single row from the dataset 
evalPoly :: [(Bool, Int)] -> [Double] -> Vector Double
evalPoly chromo xs = fromList $ foldl' joinTerms [1.0] terms 
  where
    nVars = length xs
    ix    = cycle [0..nVars-1]
    xss   = cycle xs
    terms = zip3 chromo xss ix 

    joinTerms ts   ((False,_), _, 0)   = 1.0 : ts
    joinTerms ts   ((False,_), _, _)   = ts
    joinTerms ts   ((True, k), x, 0)   = x^k : ts
    joinTerms (t:ts) ((True,k), x, ix) = t * x^k : ts


-- | Calculates the mean squared error
mse :: Vector Double -> Vector Double -> Double
mse ys ysHat = s / fromIntegral (size ys)
  where
    s = sumElements $ (ys - ysHat)^2
