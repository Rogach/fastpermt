module Fastpermt where

import Fastpermt.Stat
import Fastpermt.Util
import Fastpermt.Config
import Fastpermt.Cluster
import Data.List
import System.Random

main :: IO ()
main = do
  -- params, will be loaded via args
  let conf = Conf { nsubj = 16
                  , sz = 10242
                  , tsz = 1
                  , nperm = 100
                  }
      ns = [0..(nsubj conf - 1)]
      cutoff = (>(2.1314::Double)) -- p = 0.05
      graphFile = "graph"
      resultFile = "result"
  -- load data lists
  a <- mapM (\i -> readDoubles (sz conf * tsz conf) ('a' : show i)) ns
  b <- mapM (\i -> readDoubles (sz conf * tsz conf) ('b' : show i)) ns
  -- load cortex graph
  graph <- readGraph graphFile
  let method = maxClusterSizePerTime conf graph cutoff
  -- generate permutations
  let g = mkStdGen 5582031
      pm = grouped (nsubj conf) $ take (nsubj conf * nperm conf) (randoms g :: [Bool])
      origSpm = zipWith ttest (transpose a) (transpose b)
      spms = transpose $ zipWith (permTTest pm) (transpose a) (transpose b)
      corrSpm = method origSpm spms
  readDoubles 1 "a0" >>= print . sum
  --writeDoubles resultFile corrSpm

-- | performs ttests with permutations on two sets of values
-- | returns a list of T values
permTTest :: [[Bool]] -> [Double] -> [Double] -> [Double]
permTTest [] _ _ = []
permTTest pm as bs = let (asp, bsp) = permute (as, bs) (head pm)
                     in abs (ttest asp bsp) : permTTest (tail pm) asp bsp

permute :: ([Double], [Double]) -> [Bool] -> ([Double], [Double])
permute (as, bs) ps = permute' as bs ps [] []
  where permute' [] [] [] accA accB = (reverse accA, reverse accB)
        permute' (a:as') (b:bs') (p:ps') accA accB = if p
                                                  then permute' as' bs' ps' (a:accA) (b:accB)
                                                  else permute' as' bs' ps' (b:accA) (a:accB)
        permute' _ _ _ _ _ = undefined
