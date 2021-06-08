{-# language DeriveGeneric, DeriveAnyClass, StrictData #-}
{-# language KindSignatures, DataKinds, ScopedTypeVariables #-}
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
  ( generational
  , createRndSolution
  , crossover
  , mutate
  , Solution(..) 
  , Poly(..)
  , Population
  , Fitness
  ) where

import System.Random
import Numeric.LinearAlgebra (Vector)

import Random
import Control.Monad
import Control.Monad.State.Strict
import GHC.Generics (Generic)
import Control.DeepSeq
import GHC.TypeLits 
import Data.Proxy

-- | Data type representing a solution 
newtype Poly (n :: Nat) = Poly { _getPoly :: (Bool, Int) } deriving (Show, Generic, NFData)

instance KnownNat n => RandomAlele (Poly n) where
  randomProb p x = Poly <$> biRandomWith p randomBool (randomInt (1, maxK)) (_getPoly x)
    where maxK = fromIntegral $ natVal (Proxy :: Proxy n)
  randomAlele    = Poly <$> randomPair (1, maxK)
    where maxK = fromIntegral $ natVal (Proxy :: Proxy n)

data Solution a = Sol { _chromo    :: [a]                    -- ^ chromossome representation 
                      , _coeffs    :: Maybe (Vector Double)  -- ^ coefficients of the regression
                      , _fitness   :: Maybe Double           -- ^ fitness of an individual 
                      } deriving (Show, Generic, NFData)

instance Eq (Solution a) where
  (Sol _ _ f1) == (Sol _ _ f2) = f1 == f2
instance Ord (Solution a) where
  (Sol _ _ f1) <= (Sol _ _ f2) = f1 <= f2

type Population a   = [Solution a]
type Fitness a      = Solution a -> Solution a

nVars :: Solution a -> Int
nVars s = length (_chromo s)

-- | Creates a random solution 
createRndSolution :: RandomAlele a => Int -> Rnd (Solution a)
createRndSolution nLen = 
  do chromo <- replicateM nLen randomAlele
     return $ Sol chromo Nothing Nothing

-- | Tournament between two solutions
tournament :: Population a -> Rnd (Solution a)
tournament pop = do s1 <- chooseRndFrom pop 
                    s2 <- chooseRndFrom pop
                    return (min s1 s2)

-- | Creates a new solution from the combination of two solutions 
crossover :: Population a -> Rnd (Solution a)
crossover pop = do s1 <- tournament pop
                   s2 <- tournament pop
                   ix <- randomInt (0, nVars s1)
                   return (cx ix s1 s2)

-- | Performs crossover 
cx :: Int -> Solution a -> Solution a -> Solution a 
cx ix sol1 sol2 = Sol child Nothing Nothing
  where
    recombine xs ys = take ix xs ++ drop ix ys
    child           = recombine (_chromo sol1) (_chromo sol2)


-- | Mutates a single solution
mutate :: RandomAlele a => Double -> Solution a -> Rnd (Solution a)
mutate prob sol = do chromo <- traverse (randomProb prob) (_chromo sol)
                     return $ Sol chromo Nothing Nothing

-- | Select the next generation
generational :: Population a -> Population a -> Rnd (Population a)
generational _ = return
