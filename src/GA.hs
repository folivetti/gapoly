{-|
Module      : GA
Description : Genetic Algorithm for Poly Regression
Copyright   : (c) Fabricio Olivetti de Franca, 2021
License     : GPL-3
Maintainer  : fabricio.olivetti@gmail.com
Stability   : experimental
Portability : POSIX
|-}

module GA 
  ( select
  , createRndSolution
  , crossover
  , mutate
  , Solution(..) 
  , Population
  , Fitness
  ) where

import System.Random
import Numeric.LinearAlgebra (Vector)

-- | Data type representing a solution 
data Solution = Sol { _vars      :: [Bool]    -- ^ binary vector representing the presence of variables 
                    , _exponents :: [Int]     -- ^ integer vector with exponents
                    , _coeffs    :: Vector Double  -- ^ coefficients of the regression
                    , _fitness   :: Double    -- ^ fitness of an individual 
                    } deriving Show

instance Eq  Solution where
  (Sol _ _ _ f1) == (Sol _ _ _ f2) = f1 == f2
instance Ord Solution where
  (Sol _ _ _ f1) <= (Sol _ _ _ f2) = f1 <= f2

type Population     = [Solution]
type Fitness        = [Bool] -> [Int] -> Solution

-- | Creates a random solution 
createRndSolution :: Int -> Fitness -> StdGen -> (Solution, StdGen)
createRndSolution nLen fit g = (sol, g'')
  where
    (g', g'')   = split g
    (g1', g2')  = split g'
    vecBool     = take nLen $ randomRs (False, True) g1'
    vecInt      = take nLen $ randomRs (1, 5) g2'
    sol         = fit vecBool vecInt 

-- | Tournament between two solutions
tournament :: Population -> StdGen -> (Solution, StdGen)
tournament pop g = let (s1, g1) = chooseRnd g 
                       (s2, g2) = chooseRnd g1 
                   in  (min s1 s2, g2)
  where
    chooseRnd g' = let (ix, g'') = randomR (0, length pop - 1) g'
                   in  (pop !! ix, g'')

-- | Creates a new solution from the combination of two solutions 
crossover :: Fitness -> Population -> StdGen -> (Solution, StdGen)
crossover fit pop g = let (sol1, g1) = tournament pop g
                          (sol2, g2) = tournament pop g1
                      in  cx fit sol1 sol2 g2 
-- | Performs crossover 
cx :: Fitness -> Solution -> Solution -> StdGen -> (Solution, StdGen)
cx fit (Sol p1Bool p1Int _ _) (Sol p2Bool p2Int _ _) g = (child, g')
  where
    (ix, g')    = randomR (0, length p1Bool - 1) g
    childBool   = cx ix p1Bool p2Bool
    childInt    = cx ix p1Int  p2Int
    cx ix xs ys = take ix xs ++ drop ix ys
    child       = fit childBool childInt  


-- | Mutates a single solution
mutate :: Double -> Fitness -> Solution -> StdGen -> (Solution, StdGen)
mutate prob fit (Sol vecBool vecInt _ _) g = 
  let (vecBool', g') = mutateBool vecBool [] g
      (vecInt', g'') = mutateInt  vecInt  [] g'
  in  (fit vecBool' vecInt', g'')
  where
    mutateBool []     bs' g' = (reverse bs', g')
    mutateBool (b:bs) bs' g' = let (p, g'') = randomR (0.0, 1.0) g'
                                   (b', g''') = if p <= prob
                                                 then random g''
                                                 else (b, g'')
                               in  mutateBool bs (b':bs') g''' 
    mutateInt []  is' g'    = (reverse is', g')
    mutateInt (i:is) is' g' = let (p, g'') = randomR (0.0, 1.0) g'
                                  (i', g''') = if p <= prob
                                                  then randomR (1, 5) g''
                                                  else (i, g'')
                              in  mutateInt is (i':is') g'''

-- | Select the next generation
select :: Int -> Population -> StdGen -> (Population, StdGen)
select n pop g = (take n pop, g)
