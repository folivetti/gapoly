module Random where 

import System.Random
import Control.Monad.State.Strict
import Control.Monad
import Data.Bool 

type Rnd a = StateT StdGen IO a 

class RandomAlele a where
  randomProb :: Double -> a -> Rnd a
  randomAlele :: Rnd a

randomBool :: Rnd Bool
randomBool = state random

randomInt :: (Int, Int) -> Rnd Int
randomInt (lo, hi) = state (randomR (lo, hi))

randomPair :: (Int, Int) -> Rnd (Bool, Int)
randomPair (lo, hi) = do b <- randomBool
                         k <- randomInt (lo, hi)
                         return (b, k)

chooseRndFrom :: [a] -> Rnd a
chooseRndFrom xs = do idx <- randomInt (0, length xs - 1) 
                      return (xs !! idx)

randomCoin :: Double -> Rnd Bool
randomCoin prob = (<=prob) <$> state (randomR (0.0, 1.0))

randomWith :: Random a => Double -> Rnd a -> a -> Rnd a
randomWith prob rnd x = do p <- randomCoin prob
                           if p then rnd 
                                else return x

randomsWith :: (Random a, Traversable t) => Double -> Rnd a -> t a -> Rnd (t a)
randomsWith prob rnd = traverse (randomWith prob rnd)

biRandomWith :: (Random a, Random b) =>  Double -> Rnd a -> Rnd b -> (a, b) -> Rnd (a, b)
biRandomWith prob rnd1 rnd2 (x1, x2) =
  do x1' <- randomWith prob rnd1 x1 
     x2' <- randomWith prob rnd2 x2
     return (x1', x2')

biRandomsWith :: (Random a, Random b, Traversable t) => Double -> Rnd a -> Rnd b -> t (a, b) -> Rnd (t (a,b))
biRandomsWith prob rnd1 rnd2 = traverse (biRandomWith prob rnd1 rnd2)
