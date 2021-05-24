{-|
Module      : Lib
Description : Main component of your project.
Copyright   : (c) Fabricio Olivetti de Franca, 2021
License     : GPL-3
Maintainer  : fabricio.olivetti@gmail.com
Stability   : experimental
Portability : POSIX
|-}

module Lib
    ( parseFile
    , evalFitness    
    , select 
    , createRndSolution
    , crossover
    , mutate
    , ga
    ) where

import GA
import Fitness
import Dataset

import System.Random

type Crossover      = Population -> StdGen -> (Solution, StdGen)
type Selection      = Population -> StdGen -> (Population, StdGen)
type CreateSolution = StdGen -> (Solution, StdGen)
type Mutation       = Solution -> StdGen -> (Solution, StdGen)

-- | Sequence random actions that generates a population 
sequenceRandom :: (a -> StdGen -> (Solution, StdGen)) -> [a] -> StdGen -> (Population, StdGen)
sequenceRandom f xs g = go xs [] g
  where
    go [] pop g'     = (pop, g')
    go (x:xs) pop g' = let (sol, g'') = f x g'
                       in  go xs (sol:pop) g''

-- | Utility function to ignore first argument
constSnd :: (b -> c) -> a -> b -> c
constSnd f a = f 

-- | Main function for Genetic Algorithm
ga :: Int -> Int -> CreateSolution -> Crossover -> Mutation -> Fitness -> Selection -> StdGen -> (Population, StdGen)
ga it nPop createSol cross mut fit sel g = step it pop0 g1
  where
   (pop0, g1) = sequenceRandom (constSnd createSol) [1..nPop] g 

   step 0 pop g2 = (pop, g2)
   step n pop g2 = let (children, g3)  = sequenceRandom (constSnd $ cross pop) [1..nPop] g2
                       (children', g4) = sequenceRandom mut children g3
                       (pop', g5)      = sel (children' ++ pop) g4
                  in step (n-1) pop' g5
