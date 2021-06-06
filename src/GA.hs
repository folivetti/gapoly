{-|
Module      : GA
Description : Genetic Algorithm for Poly Regression
Copyright   : (c) Fabricio Olivetti de Franca, 2021
License     : GPL-3
Maintainer  : fabricio.olivetti@gmail.com
Stability   : experimental
Portability : POSIX
|-}

module GA 
  ( select
  , createRndSolution
  , crossover
  , mutate
  , Solution(..) 
  , Population
  , Chromossome
  , Fitness
  ) where

import System.Random
import Numeric.LinearAlgebra (Vector)

import Random
import Control.Monad
import Control.Monad.State

-- | Data type representing a solution 
type Chromossome = [(Bool, Int)]

data Solution = Sol { _chromo    :: Chromossome   -- ^ chromossome representation 
                    , _coeffs    :: Vector Double  -- ^ coefficients of the regression
                    , _fitness   :: Double    -- ^ fitness of an individual 
                    } deriving Show

instance Eq  Solution where
  (Sol _ _ f1) == (Sol _ _ f2) = f1 == f2
instance Ord Solution where
  (Sol _ _ f1) <= (Sol _ _ f2) = f1 <= f2

type Population     = [Solution]
type Fitness        = Chromossome -> Solution

nVars :: Solution -> Int
nVars s = length (_chromo s)

-- | Creates a random solution 
createRndSolution :: Int -> Int -> Fitness -> Rnd Solution
createRndSolution maxK nLen fit = fit <$> replicateM nLen (randomPair (1, maxK))

-- | Tournament between two solutions
tournament :: Population -> Rnd Solution
tournament pop = do s1 <- chooseRndFrom pop 
                    s2 <- chooseRndFrom pop
                    return (min s1 s2)

-- | Creates a new solution from the combination of two solutions 
crossover :: Fitness -> Population -> Rnd Solution
crossover fit pop = do s1 <- tournament pop
                       s2 <- tournament pop
                       ix <- randomInt (0, nVars s1)
                       return (cx fit ix s1 s2)

-- | Performs crossover 
cx :: Fitness -> Int -> Solution -> Solution -> Solution 
cx fit ix sol1 sol2 = fit child
  where
    recombine xs ys = take ix xs ++ drop ix ys
    child           = recombine (_chromo sol1) (_chromo sol2)


-- | Mutates a single solution
mutate :: Int -> Double -> Fitness -> Solution -> Rnd Solution
mutate maxK prob fit sol = fit <$> biRandomsWith prob randomBool (randomInt (1, maxK)) (_chromo sol)
                         
-- | Select the next generation
select :: Int -> Population -> Rnd Population
select n pop = return $ take n pop
