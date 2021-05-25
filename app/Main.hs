module Main where

import System.Environment
import System.Random
import Control.Monad.State

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

              stGA             = ga 100 nPop createSol cross mut fitness sel 
              pops             = evalState stGA g

          mapM_ print (last pops)
          generateReports xss ys pops
