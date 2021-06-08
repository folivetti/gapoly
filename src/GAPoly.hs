{-|
Module      : Lib
Description : Main component of your project.
Copyright   : (c) Fabricio Olivetti de Franca, 2021
License     : GPL-3
Maintainer  : fabricio.olivetti@gmail.com
Stability   : experimental
Portability : POSIX
|-}

module GAPoly
    ( parseFile
    , runGA
    , generateReports
    , Solution(..)
    , Poly(..)
    ) where

import GA
import Fitness
import Dataset
import Report
import Random
import Evolution

import Control.Monad.State.Strict
import System.Random
import Numeric.LinearAlgebra (Vector)
import GHC.TypeLits

gaPoly :: KnownNat n => Double -> Evolution (Poly n)
gaPoly pm = Select generational End
              (Cross crossover
                (Mutate (mutate pm) End))

-- | Run a genetic algorithm with the provided parameters
runGA :: KnownNat n => Int -> Int -> Int -> Int -> Double -> [[Double]] -> Vector Double -> IO ([Double], Solution (Poly n))
runGA nTerms nVars it nPop pm xss ys =
  do g <- newStdGen 
     let fitness   = evalFitness nTerms xss ys
         createSol = createRndSolution (nVars*nTerms) 
         gens      = runEvolution createSol fitness it nPop (gaPoly pm)
     evalStateT gens g

