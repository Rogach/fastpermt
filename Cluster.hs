module Fastpermt.Cluster where

import Fastpermt.Util
import qualified Data.Vector.Unboxed as V
import qualified Data.Set as S

applyClusters :: (Show f, Floating f, V.Unbox f) => V.Vector f -> [[Int]] -> V.Vector f
applyClusters arr = applyClusters' (V.replicate (V.length arr) 0)
  where applyClusters' narr [] = narr
        applyClusters' narr (cs:rest) = applyClusters' (narr V.// map (\i -> (i, arr V.! i)) cs) rest

-- | Find all clusters in a vector, according to given test
-- | Returns lists of indices
clusters :: (Floating f, V.Unbox f) => Graph -> (f -> Bool) -> V.Vector f -> [[Int]]
clusters graph test larr = clusters' larr S.empty 0
  where clusters' arr visited n
          | n >= V.length arr = []
          | test (arr V.! n) && S.notMember n visited =
            let (newVisited, ns) = cluster visited [n] []
                  where cluster vis [] acc = (vis, acc)
                        cluster vis (q:queue) acc =
                          if test (arr V.! q) && S.notMember q vis
                          then cluster (S.insert q vis) (grlookup graph q ++ queue) (q:acc)
                          else cluster vis queue acc
            in ns : clusters' arr newVisited (n+1)
          | otherwise = clusters' arr visited (n+1)

clusterThinning :: (Floating f, V.Unbox f) => Graph -> (f -> Bool) -> V.Vector f -> V.Vector f
clusterThinning graph test larr =
  let thin i v = if test v && all test (map (larr V.!) (grlookup graph i))
                 then v
                 else 0
      thick i v = if test v || any test (map (larr V.!) (grlookup graph i))
                  then larr V.! i
                  else 0
  in V.imap thick $ V.imap thin larr
