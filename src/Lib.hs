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
    ( decode
    , createRndSolution
    , mse
    , ga
    ) where

import System.Random
import Data.List.Split hiding (split)

-- | Generates the initial random population 
genInitialPopulation :: Int -> Int -> Int -> StdGen -> ([([Bool], [Int])], StdGen)
genInitialPopulation nPop nVars nTerms g = go nPop [] g
  where
    len          = nVars * nTerms 

    go 0 pop g = (pop, g)
    go n pop g = let (sol, g') = createRndSolution len g
                 in  go (n-1) (sol:pop) g'

-- | Creates a random solution 
createRndSolution :: Int -> StdGen -> (([Bool], [Int]), StdGen)
createRndSolution nLen g = ((vecBool, vecInt), g'')
  where
    (g', g'')  = split g
    (g1', g2') = split g'
    vecBool    = take nLen $ randomRs (False, True) g1'
    vecInt     = take nLen $ randomRs (1, 5) g2'

-- | applies crossover in the population 
applyCrossover :: [([Bool], [Int])] -> StdGen -> ([([Bool], [Int])], StdGen)
applyCrossover pop g = go nPop [] g
-- replicate (length pop) (crossover (choose pop) (choose pop))
  where 
    nPop         = length pop 
    chooseRnd g' = let (ix, g'') = randomR (0, nPop - 1) g'
                   in  (pop !! ix, g'')
    go 0 pop' g' = (pop', g')
    go n pop' g' = let (sol1, g1) = chooseRnd g'
                       (sol2, g2) = chooseRnd g1
                       (sol,  g3) = crossover sol1 sol2 g2
                   in  go (n-1) (sol:pop') g3

-- | Creates a new solution from the combination of two solutions 
crossover :: ([Bool], [Int]) -> ([Bool], [Int]) -> StdGen -> (([Bool], [Int]), StdGen)
crossover (p1Bool, p1Int) (p2Bool, p2Int) g = ((childBool, childInt), g')
  where
    (ix, g')    = randomR (0, length p1Bool - 1) g
    childBool   = cx ix p1Bool p2Bool
    childInt    = cx ix p1Int  p2Int
    cx ix xs ys = take ix xs ++ drop ix ys

p1 = ([True, False, False, True], [1,1,3,5] :: [Int])
p2 = ([False, False, True, False], [3,1,4,2] :: [Int])

-- | Applies mutation in the population 
applyMutation :: [([Bool], [Int])] -> StdGen -> ([([Bool], [Int])], StdGen) 
applyMutation pop g = go pop [] g
  where
    go [] pop' g' = (pop', g')
    go (p:ps) pop' g' = let (p', g'') = mutate 0.01 p g'
                        in  go ps (p':pop') g''

-- | Mutates a single solution
mutate :: Double -> ([Bool], [Int]) -> StdGen -> (([Bool], [Int]), StdGen)
mutate prob (vecBool, vecInt) g = 
  let (vecBool', g') = mutateBool vecBool [] g
      (vecInt', g'') = mutateInt  vecInt  [] g'
  in  ((vecBool', vecInt'), g'')
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

select :: Int -> [([Bool], [Int])] -> StdGen -> ([([Bool], [Int])], StdGen)
select n pop g = (take n pop, g)

-- beta0 + beta1 * t1 + beta2 * t2... 
decode :: [[Double]] -> [Double] -> ([Bool], [Int]) -> [Double]
decode xss betas sol = map (evalPoly sol betas) xss

evalPoly :: ([Bool], [Int]) -> [Double] -> [Double] -> Double
evalPoly (vecBool, vecInt) (b0:bs) xs = b0 + foldr f 0.0 (zip bs terms)
  where
    nVars           = length xs
    termsBools      = chunksOf nVars vecBool
    termsInts       = chunksOf nVars vecInt
    terms           = zip termsBools termsInts
    f (beta, t) acc = beta * evalTerm xs t + acc

evalTerm :: [Double] -> ([Bool], [Int]) -> Double
evalTerm xs' (vecBool, vecInt) = go vecBool vecInt xs' 1
  where
    go [] _ _ acc = acc
    go _ [] _ acc = acc
    go _ _ [] acc = acc
    go (False:bs) (k:ks) (x:xs) acc = go bs ks xs acc
    go (True:bs)  (k:ks) (x:xs) acc = go bs ks xs (acc * x^k)

mse :: [Double] -> [Double] -> Double
mse ys ysHat = go ys ysHat 0.0 0.0
  where
    go [] _ accS accL             = accS / accL
    go _ [] accS accL             = accS / accL
    go (y:ys') (yH:ysH) accS accL = go ys' ysH (accS + (y-yH)^2) (accL + 1.0)

-- | Main function for Genetic Algorithm
ga :: Int -> Int -> Int -> Int -> StdGen -> ([([Bool], [Int])], StdGen)
ga it nPop nVars nTerms g = step it pop0 g1
  where
   (pop0, g1) = genInitialPopulation nPop nVars nTerms g

   step 0 pop g2 = (pop, g2)
   step n pop g2 = let (children, g3)  = applyCrossover pop g2
                       (children', g4) = applyMutation children g3
                       (pop', g5)      = select nPop (children' ++ pop) g4
                  in step (n-1) pop' g5

