module Main where

import System.Environment
import System.Random
import Data.List.Split

import Lib

parseFile :: String -> ([[Double]], [Double])
parseFile css = (xss, ys)
  where 
    zss = map (map read . splitOn ",") $ lines css 
    ys  = map last zss 
    xss = map init zss 

main :: IO ()
main = do args <- getArgs 
          g    <- newStdGen
          let dataname = case args of
                           [] -> error "Usage: stack run dataname"
                           (x:_) -> x 
          dat <- readFile dataname
          let (xss, ys)   = parseFile dat 
              nVars          = length (head xss)
              betas          = replicate nVars 1.0 :: [Double]
              n              = nVars * 3
              fitness bs sol = mse ys $ decode xss bs sol
              (s, g')        = createRndSolution n g
          print $ ga 10 4 nVars 3 g
