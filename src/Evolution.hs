{-# language BangPatterns #-}
module Evolution 
  ( runEvolution
  , evalEvoPar
  , evalEvoSeq
  , Evolution(..)
  ) where

import Control.Monad
import Control.Monad.State.Strict
import Control.Monad.Extra
import System.Random
import Data.Monoid
import Data.Maybe
import Control.DeepSeq (force)
import Control.Parallel.Strategies
import Control.Scheduler

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
evalEvoSeq :: Fitness a -> Evolution a -> Population a -> Rnd (Population a)
evalEvoSeq _ End pop = return pop

evalEvoSeq fit (Select sel evo1 evo2) pop = 
  do pop1 <- evalEvoSeq fit evo1 pop
     pop2 <- evalEvoSeq fit evo2 pop
     sel pop1 pop2

evalEvoSeq fit (Cross cross evo) pop =
  do let nPop = length pop
     pop' <- map fit <$> replicateM nPop (cross pop) 
     evalEvoSeq fit evo pop'

evalEvoSeq fit (Mutate mut evo) pop =
  do pop' <- map fit <$> mapM mut pop
     evalEvoSeq fit evo pop'

evalEvoPar :: NFData a => Fitness a -> Evolution a -> Population a -> Rnd (Population a)
evalEvoPar _ End pop = return pop

evalEvoPar fit (Select sel evo1 evo2) pop = 
  do pop1 <- evalEvoPar fit evo1 pop
     pop2 <- evalEvoPar fit evo2 pop
     sel pop1 pop2

evalEvoPar fit (Cross cross evo) pop =
  do g <- get 
     let nPop = length pop
         (gi:gs) = genSeeds (nPop + 1) g 
         f g'    = force . fit <$> evalStateT (cross pop) g' 
     pop' <- liftIO $ traverseConcurrently (ParN 0) f gs
     put gi
     evalEvoPar fit evo pop'

evalEvoPar fit (Mutate mut evo) pop =
  do g <- get
     let (gi:gs)   = genSeeds (length pop + 1) g
         f (p, g') = force . fit <$> evalStateT (mut p) g'
     pop' <- liftIO $ traverseConcurrently (ParN 0) f $ zip pop gs
     put gi
     evalEvoPar fit evo pop' 

-- | split a random seed into n seeds
genSeeds :: Int -> StdGen -> [StdGen]
genSeeds 0 g = []
genSeeds n g = let (g1,g2) = split g
               in  g1 : genSeeds (n-1) g2

-- | Calculates the average fitness of a population 
avgfit :: Population a -> Double
avgfit pop = getSum tot / fromIntegral (length pop)
  where
    tot = foldMap (Sum . fromJust . _fitness) pop

-- | Runs an evolutionary process 
runEvolution :: NFData a => CreateSolution a -> Fitness a -> Int -> Int -> Evolution a -> Rnd ([Double], Solution a)
runEvolution createSol fit nGens nPop evo =
  do pop0 <- map fit <$> replicateM nPop createSol
     go nGens pop0 ([avgfit pop0], minimum pop0)
  where
    evo' = simplify evo

    go 0 pop (!avgs, !best) = return (reverse avgs, best)
    go n pop (!avgs, !best) = do pop' <- evalEvoPar fit evo' pop
                                 let avgs' = force $ avgfit pop' : avgs
                                     best' = min best (minimum pop')
                                 go (n-1) pop' (avgs', best')

