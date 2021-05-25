module Random where 

import System.Random
import Control.Monad.State 
import Control.Monad
import Data.Bool 

type Rnd a = State StdGen a 

randomBool :: Rnd Bool
randomBool = state random

randomInt :: (Int, Int) -> Rnd Int
randomInt (lo, hi) = state (randomR (lo, hi))

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
