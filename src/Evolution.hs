{-# language BangPatterns #-}
module Evolution 
  ( runEvolution
  , eval
  , Evolution(..)
  ) where

import Control.Monad
import Control.Monad.Extra
import System.Random
import Data.Monoid
import Data.Maybe
import Control.DeepSeq (force)

import GA
import Fitness
import Random

type Crossover a      = Population a -> Rnd (Solution a)
type Selection a      = Population a -> Population a -> Rnd (Population a)
type CreateSolution a = Rnd (Solution a)
type Mutation a       = Solution a -> Rnd (Solution a)

-- | DSL of an evolutionary process 
data Evolution a = Select (Selection a) (Evolution a) (Evolution a)
               | Cross  (Crossover a) (Evolution a)
               | Mutate (Mutation a)  (Evolution a)
               | End

simplify :: Evolution a -> Evolution a
simplify (Select sel evo1 evo2) = Select sel (simplify evo1) (simplify evo2)
simplify (Cross cross evo) = 
  case evo of
       Mutate _ _ -> let Mutate mut evo' = simplify evo 
                     in  Cross (cross >=> mut) evo'
       _          -> Cross cross (simplify evo)
simplify (Mutate mut evo) =
  case evo of
       Mutate _ _ -> let Mutate mut' evo' = simplify evo
                     in  Mutate (mut >=> mut') evo'
       _          -> Mutate mut (simplify evo)
simplify End = End

-- | Evals a single iteration of an evolutionary process 
eval :: Fitness a -> Evolution a -> Population a -> Rnd (Population a)
eval _ End pop = return pop

eval fit (Select sel evo1 evo2) pop = 
  do pop1 <- eval fit evo1 pop
     pop2 <- eval fit evo2 pop
     sel (map fit pop1) (map fit pop2)

eval fit (Cross cross evo) pop =
  do let nPop = length pop
     pop' <- replicateM nPop $ cross $ map fit pop 
     eval fit evo pop

eval fit (Mutate mut evo) pop =
  do pop' <- mapM mut pop
     eval fit evo pop'

avgfit :: Population a -> Double
avgfit pop = getSum tot / fromIntegral (length pop)
  where
    tot = foldMap (Sum . fromJust . _fitness) pop

-- | Runs an evolutionary process 
runEvolution :: CreateSolution a -> Fitness a -> Int -> Int -> Evolution a -> Rnd ([Double], Solution a)
runEvolution createSol fit nGens nPop evo =
  do pop0 <- map fit <$> replicateM nPop createSol
     go nGens pop0 ([avgfit pop0], minimum pop0)
  where
    evo' = simplify evo

    go 0 pop (!avgs, !best) = return (reverse avgs, best)
    go n pop (!avgs, !best) = do pop' <- eval fit evo' pop
                                 let avgs' = force $ avgfit pop' : avgs
                                     best' = min best (minimum pop')
                                 go (n-1) pop' (avgs', best')

