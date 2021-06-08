{-# language DataKinds, ScopedTypeVariables #-}
module Main where

import System.Environment
import GHC.TypeLits
import Data.Proxy
import Data.List
import Data.List.Split (chunksOf)
import Data.Maybe
import Numeric.LinearAlgebra (toList)

import GAPoly 

type Args = (String, Int, Int, Int, Integer, Double)

validateArgs :: [String] -> Args
validateArgs args =
  case args of
       (fname:nGens:nPop:nTerms:maxK:pm:_) -> (fname, read nGens, read nPop, read nTerms, read maxK, read pm)
       _ -> error "missing arguments"

term2str :: [(Bool,Int)] -> String
term2str ts = intercalate "*" $ filter (/="") $ zipWith toStr ts [0..]
  where
    toStr (False,_) _  = ""
    toStr (True, k) ix = "x[:," ++ show ix ++ "]**(" ++ show k ++ ")"

poly2str :: Int -> Solution (Poly n) -> String
poly2str nVars (Sol cs betas _) = let cs'    = map _getPoly cs
                                      terms  = map term2str $ chunksOf nVars cs'
                                      (b:bs) = reverse $ toList $ fromJust betas 
                                      bts    = intercalate " + " $ zipWith toStr bs terms 
                                   in show b ++ " + " ++ bts
  where
    toStr beta term = show beta ++ "*" ++ term

main :: IO ()
main = do args <- getArgs
          let (dataname, nGens, nPop, nTerms, maxK, pm) = validateArgs args 
          dat <- readFile dataname
          let (xss, ys, nVars) = parseFile dat
          case someNatVal maxK of
               Just (SomeNat (_ :: Proxy n)) ->
                 do (avgs, best) <- runGA nTerms nVars nGens nPop pm xss ys 
                    print $ poly2str nVars (best :: Solution (Poly n))
               Nothing -> error "Negative maxK"
