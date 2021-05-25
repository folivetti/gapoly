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
  , Fitness
  ) where

import System.Random
import Numeric.LinearAlgebra (Vector)

import Random
import Control.Monad
import Control.Monad.State

-- | Data type representing a solution 
data Solution = Sol { _vars      :: [Bool]    -- ^ binary vector representing the presence of variables 
                    , _exponents :: [Int]     -- ^ integer vector with exponents
                    , _coeffs    :: Vector Double  -- ^ coefficients of the regression
                    , _fitness   :: Double    -- ^ fitness of an individual 
                    } deriving Show

instance Eq  Solution where
  (Sol _ _ _ f1) == (Sol _ _ _ f2) = f1 == f2
instance Ord Solution where
  (Sol _ _ _ f1) <= (Sol _ _ _ f2) = f1 <= f2

type Population     = [Solution]
type Fitness        = [Bool] -> [Int] -> Solution

nVars :: Solution -> Int
nVars s = length (_vars s)

-- | Creates a random solution 
createRndSolution :: Int -> Fitness -> Rnd Solution
createRndSolution nLen fit = 
  do vars <- replicateM nLen randomBool
     exps <- replicateM nLen (randomInt (1, 5))
     return $ fit vars exps

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
cx fit ix sol1 sol2 = fit childVars childExps
  where
    recombine xs ys = take ix xs ++ drop ix ys
    childVars       = recombine (_vars sol1) (_vars sol2)
    childExps       = recombine (_exponents sol1) (_exponents sol2)

-- | Mutates a single solution
mutate :: Double -> Fitness -> Solution -> Rnd Solution
mutate prob fit sol = do vars <- randomsWith prob randomBool $ _vars sol
                         exps <- randomsWith prob (randomInt (1, 5)) $ _exponents sol
                         return $ fit vars exps

                         
-- | Select the next generation
select :: Int -> Population -> Rnd Population
select n pop = return $ take n pop
