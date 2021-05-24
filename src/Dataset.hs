{-|
Module      : Dataset
Description : Utility functions to load the data
Copyright   : (c) Fabricio Olivetti de Franca, 2021
License     : GPL-3
Maintainer  : fabricio.olivetti@gmail.com
Stability   : experimental
Portability : POSIX
|-}

module Dataset (parseFile) where

import Numeric.LinearAlgebra (Vector, fromList)
import Data.List.Split (splitOn)

-- | parse the file and returns the training data, target values and number of vars
parseFile :: String -> ([[Double]], Vector Double, Int)
parseFile css = (xss, fromList ys, nVars)
  where 
    zss   = map (map read . splitOn ",") $ lines css
    ys    = map last zss
    xss   = map init zss
    nVars = length (head xss)
