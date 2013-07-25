module Fastpermt.Util where

import Data.Binary
import Data.Binary.Get
import Data.Binary.Put
import Debug.Trace
import Fastpermt.Config
import Unsafe.Coerce
import qualified Data.Vector.Unboxed as V

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
