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
import Control.Monad.Extra
import Control.Monad.State
import System.Random
import Numeric.LinearAlgebra (Vector)

type Crossover      = Population -> Rnd Solution 
type Selection      = Population -> Population -> Rnd Population 
type CreateSolution = Rnd Solution 
type Mutation       = Solution -> Rnd Solution

-- | DSL of an evolutionary process 
data Evolution = Select Selection Evolution Evolution
               | Cross  Crossover Evolution 
               | Mutate Mutation  Evolution
               | End

simplify :: Evolution -> Evolution
simplify (Select sel evo1 evo2) = Select sel (simplify evo1) (simplify evo2)
simplify (Cross cross evo) = 
  case evo of
       Mutate _ _ -> let Mutate mut evo' = simplify evo 
                     in  Cross (cross >=> mut) evo'
       _          -> Cross cross (simplify evo)
simplify (Mutate mut evo) =
  case evo of
       Mutate _ _ -> let Mutate mut' evo' = simplify evo
                     in  Mutate (mut >=> mut') evo'
       _          -> Mutate mut (simplify evo)
simplify End = End

-- | Evals a single iteration of an evolutionary process 
eval :: Fitness -> Evolution -> Population -> Rnd Population
eval _ End pop = return pop

eval fit (Select sel evo1 evo2) pop = 
  do pop1 <- eval fit evo1 pop
     pop2 <- eval fit evo2 pop
     sel (map fit pop1) (map fit pop2)

eval fit (Cross cross evo) pop =
  do let nPop = length pop
     pop' <- replicateM nPop $ cross $ map fit pop 
     eval fit evo pop

eval fit (Mutate mut evo) pop =
  do pop' <- mapM mut pop
     eval fit evo pop'

-- | Runs an evolutionary process 
runEvolution :: CreateSolution -> Fitness -> Int -> Int -> Evolution -> Rnd [Population]
runEvolution createSol fit nGens nPop evo =
  do pop0 <- map fit <$> replicateM nPop createSol
     take nGens <$> iterateM (eval fit evo') pop0 
  where
    evo' = simplify evo

-- | Run a genetic algorithm with the provided parameters
runGA :: Int -> Int -> Int -> Int -> Int -> Double -> [[Double]] -> Vector Double -> IO [Population]
runGA nTerms nVars it nPop maxK pm xss ys =
  do g <- newStdGen 
     let fitness   = evalFitness nTerms xss ys
         createSol = createRndSolution maxK (nVars*nTerms) 
         polyGA    = Select generational End (Cross crossover (Mutate (mutate maxK pm) End))
         gens      = runEvolution createSol fitness it nPop polyGA
     return $ evalState gens g

