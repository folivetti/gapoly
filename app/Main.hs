module Main where

import System.Environment
import System.Random
import Data.List.Split
import Numeric.LinearAlgebra

import Lib

main :: IO ()
main = do args <- getArgs 
          g    <- newStdGen
          let dataname = case args of
                           [] -> error "Usage: stack run dataname"
                           (x:_) -> x 
          dat <- readFile dataname
          let (xss, ys, nVars) = parseFile dat
              nTerms           = 5
              nPop             = 100
              fitness          = evalFitness nTerms xss ys
              sel              = select nPop
              createSol        = createRndSolution (nVars*nTerms) fitness
              cross            = crossover fitness
              mut              = mutate 0.01 fitness

          mapM_ print $ fst $ ga 100 nPop createSol cross mut fitness sel g
