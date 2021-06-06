module Main where

import System.Environment
import System.Random
import Control.Monad.State

import Lib

main :: IO ()
main = do args <- getArgs 
          let dataname = case args of
                           [] -> error "Usage: stack run dataname"
                           (x:_) -> x 
          dat <- readFile dataname
          let (xss, ys, nVars) = parseFile dat
              nTerms           = 5
              nPop             = 100
              nIter            = 100
              maxK             = 5
              pm               = 0.01

          pops <- runGA nTerms nVars nIter nPop maxK pm xss ys
          mapM_ print (last pops)
          generateReports xss ys pops
