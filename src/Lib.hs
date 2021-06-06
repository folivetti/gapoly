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
    , runGA
    , generateReports
    ) where

import GA
import Fitness
import Dataset
import Report
import Random

import Control.Monad
import Control.Monad.State
import System.Random
import Numeric.LinearAlgebra (Vector)

type Crossover      = Population -> Rnd Solution 
type Selection      = Population -> Rnd Population 
type CreateSolution = Rnd Solution 
type Mutation       = Solution -> Rnd Solution

-- | Run a genetic algorithm with the provided parameters
runGA :: Int -> Int -> Int -> Int -> Int -> Double -> [[Double]] -> Vector Double -> IO [Population]
runGA nTerms nVars it nPop maxK pm xss ys =
  do g <- newStdGen 
     let fitness   = evalFitness nTerms xss ys
         sel       = select nPop
         createSol = createRndSolution maxK (nVars*nTerms) fitness
         cross     = crossover fitness
         mut       = mutate maxK pm fitness
         stGA      = ga it nPop createSol cross mut fitness sel
     return $ evalState stGA g

-- | Main function for Genetic Algorithm
ga :: Int -> Int -> CreateSolution -> Crossover -> Mutation -> Fitness -> Selection -> Rnd [Population]
ga it nPop createSol cross mut fit sel =
  do pop0 <- replicateM nPop createSol
     step it [pop0]
  where
    step 0 ps = return $ reverse ps
    step n (p:ps) = do children  <- replicateM nPop (cross p)
                       children' <- mapM mut children 
                       p'        <- sel (children' ++ p)
                       step (n-1) (p':p':ps)
