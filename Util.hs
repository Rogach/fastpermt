module Fastpermt.Util where

import Data.Binary
import Data.Binary.Get
import Data.Binary.Put
import Debug.Trace
import Fastpermt.Config
import Fastpermt.Graph
import Unsafe.Coerce
import qualified Data.Vector.Unboxed as V
import qualified Data.Map as M

onVertices :: ClusterConf -> (V.Vector Float -> a) -> V.Vector Float -> [a]
onVertices conf op vals =
  map (\t -> op $ V.slice (t*(nVerts conf)) (nVerts conf) vals) [0..(nTimes conf)-1]

-- helpers for 32bit big-endian io
getInt32be :: Get Int
getInt32be = fmap unsafeCoerce getWord32be
getFloat32be :: Get Float
getFloat32be = fmap unsafeCoerce getWord32be
putInt32be :: Int -> Put
putInt32be i = putWord32be (unsafeCoerce i)
putFloat32be :: Float -> Put
putFloat32be f = putWord32be (unsafeCoerce f)

grouped :: Int -> [a] -> [[a]]
grouped _ [] = []
grouped n xs = take n xs : grouped n (drop n xs)

pDebug :: (Show b) => String -> (a -> b) -> a -> a
pDebug msg str a = trace (msg ++ (show $ str a)) a

spatioTemporalGraph :: ClusterConf -> Graph -> Graph
spatioTemporalGraph ClusterConf { nVerts = nv, nTimes = nt } graph =
  M.fromList [ (t*nv + v, ng0 ++ ng ++ ng2)
             | v <- [0..nv-1]
             , t <- [0..nt-1]
             , let ng = map (+(t*nv)) $ grlookup graph v
                   ng0 = if t > 0 then [(t-1)*nv+v] else []
                   ng2 = if t < nt - 1 then [(t+1)*nv+v] else []
             ]

spatialGraph :: ClusterConf -> Graph -> Graph
spatialGraph ClusterConf { nVerts = nv, nTimes = nt } graph =
  M.fromList [ (t*nv + v, ng)
             | v <- [0..nv-1]
             , t <- [0..nt-1]
             , let ng = map (+(t*nv)) $ grlookup graph v
             ]
