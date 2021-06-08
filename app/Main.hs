{-# language DataKinds, ScopedTypeVariables #-}
module Main where

import System.Environment
import System.Random
import Control.Monad.State
import GHC.TypeLits
import Data.Proxy

import GAPoly

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
          case someNatVal maxK of
               Just (SomeNat (_ :: Proxy n)) ->
                 do (avgs, best) <- runGA nTerms nVars nIter nPop pm xss ys
                    print $ _fitness (best :: Solution (Poly n))
                    generateReports xss ys avgs best
               Nothing -> error "Negative maxK"
