{-# language OverloadedStrings #-}
module Report (generateReports) where

import Graphics.Vega.VegaLite hiding (Sum)
import Prelude hiding (filter, lookup, repeat)
import Data.Monoid (Sum(..))
import Numeric.LinearAlgebra ((#>), Vector, toList)

import GA
import Fitness 

avgfit :: Population -> Double
avgfit pop = getSum tot / fromIntegral (length pop)
  where
    tot = foldMap (Sum . _fitness) pop

-- | Plot the evolution of average fitness along the generations
plotEvo :: [Population] -> IO ()
plotEvo pops = 
  let avgs = map avgfit pops
      gens = [1.0 .. fromIntegral (length pops)] 
      plotData = dataFromColumns []
               . dataColumn "generation" (Numbers gens)
               . dataColumn "avg" (Numbers avgs)
               $ []
      enc      = encoding
               . position X [ PName "generation", PmType Quantitative]
               . position Y [ PName "avg", PmType Quantitative]
               $ []
  in  toHtmlFile "evolution.html" $ toVegaLite [ plotData, mark Line [], enc, height 800, width 600 ]

plotPoly :: [[Double]] -> Vector Double -> Solution -> IO ()
plotPoly xss ys sol = 
  let x0 = map head xss
      zss = decode xss (_chromo sol)
      ysHat = toList $ zss #> _coeffs sol
      ys'   = toList ys
      clusters = replicate (length x0) "poly" ++ replicate (length x0) "real"
      plotData = dataFromColumns []
               . dataColumn "x0" (Numbers $ x0 ++ x0)
               . dataColumn "y" (Numbers $ ysHat ++ ys')
               . dataColumn "Cluster" (Strings clusters)
               $ []
      enc      = encoding
               . position X [ PName "x0", PmType Quantitative]
               . position Y [ PName "y", PmType Quantitative]
               . color [ MName "Cluster", MmType Nominal ]
               $ []
  in  toHtmlFile "poly.html" $ toVegaLite [ plotData, mark Line [], enc, height 800, width 600 ]

generateReports :: [[Double]] -> Vector Double -> [Population] -> IO ()
generateReports xss ys pops = do plotEvo pops
                                 plotPoly xss ys (minimum $ concat pops)
