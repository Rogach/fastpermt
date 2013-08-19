module Fastpermt.Util where

import Foreign.C
import Data.Binary
import Data.Binary.Get
import Data.Binary.Put
import Debug.Trace
import Fastpermt.Config
import Fastpermt.Graph
import Unsafe.Coerce
import qualified Data.Vector.Storable as V
import qualified Data.Map as M

onVertices :: ClusterConf -> (V.Vector CFloat -> a) -> V.Vector CFloat -> [a]
onVertices conf op vals =
  map (\t -> op $ V.slice (t * nVerts conf) (nVerts conf) vals) [0..nTimes conf-1]

-- helpers for 32bit big-endian io
getInt32be :: Get Int
getInt32be = fmap unsafeCoerce getWord32be
getFloat32be :: Get CFloat
getFloat32be = fmap unsafeCoerce getWord32be
putInt32be :: Int -> Put
putInt32be i = putWord32be (unsafeCoerce i)
putFloat32be :: CFloat -> Put
putFloat32be f = putWord32be (unsafeCoerce f)

grouped :: Int -> [a] -> [[a]]
grouped _ [] = []
grouped n xs = take n xs : grouped n (drop n xs)

pDebug :: (Show b) => String -> (a -> b) -> a -> a
pDebug msg str a = trace (msg ++ show (str a)) a

convertGraph :: Bool -> ClusterConf -> ClusterConf
convertGraph isSpatioTemporal cc =
  if isSpatioTemporal
  then cc { graph = spatioTemporalGraph cc }
  else cc { graph = spatialGraph cc }

spatioTemporalGraph :: ClusterConf -> Graph
spatioTemporalGraph ClusterConf { nVerts = nv, nTimes = nt, graph = graph } =
  M.fromList [ (t*nv + v, ng0 ++ ng ++ ng2)
             | v <- [0..nv-1]
             , t <- [0..nt-1]
             , let ng = map (+(t*nv)) $ grlookup graph v
                   ng0 = [ (t-1)*nv+v | t > 0 ]
                   ng2 = [ (t+1)*nv+v | t < nt - 1 ]
             ]

spatialGraph :: ClusterConf -> Graph
spatialGraph ClusterConf { nVerts = nv, nTimes = nt, graph = graph } =
  M.fromList [ (t*nv + v, ng)
             | v <- [0..nv-1]
             , t <- [0..nt-1]
             , let ng = map (+(t*nv)) $ grlookup graph v
             ]
