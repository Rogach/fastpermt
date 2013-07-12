module Fastpermt.Cluster where

import Fastpermt.Util
import Data.Ord
import Data.List
import Data.Maybe
import qualified Data.Vector.Unboxed as V
import qualified Data.Map as M
import qualified Data.Set as S

-- maxClusterSize :: Conf -> Graph -> (Double -> Bool) -> [Double] -> [[Double]] -> [Double]
-- maxClusterSize conf graph test origSpm spms = undefined

-- maxClusterSizePerTime :: Conf -> Graph -> (Double -> Bool) -> [Double] -> [[Double]] -> [Double]
-- maxClusterSizePerTime conf graph test origSpm spms =
--   let clusterSizes =
--         map (map (length . maximumBy (comparing length) . clusters graph test) . grouped (sz conf)) spms
--       clusterThresholds =
--         map (\clsts -> sort clsts !! (nperm conf `div` 20 + 1)) $ transpose clusterSizes
--       remainingClusters =
--         zipWith (\th -> filter ((>= th) . length)) clusterThresholds (map (clusters graph test) $ grouped (sz conf) origSpm)
--       appliedClusters =
--         zipWith (\rc ospm -> V.toList $ applyClusters (V.fromList ospm) rc) remainingClusters (grouped (sz conf) origSpm)
--   in concat appliedClusters

-- applyClusters :: V.Vector Double -> [[Int]] -> V.Vector Double
-- applyClusters arr = applyClusters' (V.replicate (V.length arr) 0)
--   where applyClusters' narr [] = narr
--         applyClusters' narr (cs:rest) = applyClusters (narr V.// map (\i -> (i, arr V.! i)) cs) rest

-- -- | Find all clusters in a vector, according to given test
-- clusters :: Graph -> (Double -> Bool) -> [Double] -> [[Int]]
-- clusters graph test larr = clusters' (V.fromList larr) S.empty 0
--   where clusters' :: V.Vector Double -> S.Set Int -> Int -> [[Int]]
--         clusters' arr visited n
--           | n >= V.length arr = []
--           | test (arr V.! n) && S.notMember n visited =
--             let (newVisited, ns) = cluster visited [n] []
--                   where cluster vis [] acc = (vis, acc)
--                         cluster vis (q:queue) acc =
--                           if test (arr V.! q) && S.notMember q vis
--                           then cluster (S.insert q vis) (fromMaybe [] (M.lookup q graph) ++ queue) (q:acc)
--                           else cluster vis queue acc
--             in ns : clusters' arr newVisited (n+1)
--           | otherwise = clusters' arr visited (n+1)
