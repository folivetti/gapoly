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
    ( someFunc
    ) where

-- Solução: ([Bool], [Int])
-- População: [Solução]
-- nVars*nTerms

-- | Generates the initial random population 
genInitialPopulation :: Int -> Int -> Int -> [([Bool], [Int])]
genInitialPopulation nPop nVars nTerms = [createRndSolution (nVars*nTerms) | n <- [1..nPop]]

-- | Creates a random solution 
createRndSolution :: Int -> ([Bool], [Int])
createRndSolution nLen = (vecBool, vecInt)
  where
    vecBool = take nLen $ cycle [False, True]
    vecInt  = take nLen $ cycle [1 .. 5]

-- | applies crossover in the population 
applyCrossover :: [([Bool], [Int])] -> [([Bool], [Int])]
applyCrossover pop = [crossover (choose pop) (choose pop) | _ <- [1..length pop]]
  where
    choose pop = pop !! (length pop `div` 3)

-- | Creates a new solution from the combination of two solutions 
crossover :: ([Bool], [Int]) -> ([Bool], [Int]) -> ([Bool], [Int])
crossover (p1Bool, p1Int) (p2Bool, p2Int) = (childBool, childInt)
  where
    ix        = length p1Bool `div` 2
    childBool = take ix p1Bool ++ drop ix p2Bool
    childInt  = take ix p1Int  ++ drop ix p2Int

p1 = ([True, False, False, True], [1,1,3,5] :: [Int])
p2 = ([False, False, True, False], [3,1,4,2] :: [Int])

-- | Applies mutation in the population 
applyMutation :: [([Bool], [Int])] -> [([Bool], [Int])]
applyMutation pop = [mutate p | p <- pop]

-- | Mutates a single solution
mutate :: ([Bool], [Int]) -> ([Bool], [Int])
mutate (vecBool, vecInt) = (vecBool', vecInt')
  where
    b  = True -- False: mutates vecBool, True: mutates vecInt 
    ix = length vecBool `div` 2
    vecBool' = if not b then changeAtRndBool ix vecBool else vecBool
    vecInt'  = if b     then changeAtRndInt ix vecInt  else vecInt

-- | Replaces ix-th element with x 
change :: Int -> a -> [a] -> [a]
change ix x xs = take ix xs ++ (x : drop (ix+1) xs)

-- | Replaces in a bool vector 
changeAtRndBool :: Int -> [Bool] -> [Bool]
changeAtRndBool ix vec = change ix x vec
  where x = not (vec !! ix)

-- | Replaces in a Int vector 
changeAtRndInt :: Int -> [Int] -> [Int]
changeAtRndInt ix vec = change ix x vec
  where x = (((vec !! ix) * 10 + 3) `mod` 5) + 1

select :: Int -> [([Bool], [Int])] -> [([Bool], [Int])]
select = take

-- | Main function for Genetic Algorithm
ga :: Int -> Int -> Int -> Int -> [([Bool], [Int])]
ga it nPop nVars nTerms =
  let pop = genInitialPopulation nPop nVars nTerms
  in  step it pop
    where
      step 0 pop = pop
      step n pop = let children  = applyCrossover pop
                       children' = applyMutation children
                       pop'      = select nPop (children' ++ pop)
                    in step (n-1) pop'

someFunc :: IO ()
someFunc = putStrLn "someFunc"
