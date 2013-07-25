{-# LANGUAGE ExistentialQuantification, NamedFieldPuns #-}
module Fastpermt.Methods ( Method(..)
                         , MaxThreshold(..)
                         , MaxClusterSize(..)
                         , AnyMethod(..)
                         , modAbs
                         , modClusterThinning
                         , onVertices
                         ) where

import qualified Data.Vector.Unboxed as V
import Fastpermt.Cluster
import Fastpermt.Util ()
import Fastpermt.Config

class Method a where
  apply :: a -> V.Vector Float -> Float
  threshold :: a -> Float -> V.Vector Float -> V.Vector Float

data ModifiedMethod m = ModifiedMethod (V.Vector Float -> V.Vector Float) m
instance Method m => Method (ModifiedMethod m) where
  apply (ModifiedMethod modify meth) = apply meth . modify
  threshold (ModifiedMethod modify meth) th = threshold meth th . modify

data AnyMethod = forall m. Method m => AnyMethod m
instance Method AnyMethod where
  apply (AnyMethod m) = apply m
  threshold (AnyMethod m) = threshold m


data MaxThreshold = MaxThreshold deriving (Eq, Show)
instance Method MaxThreshold where
  apply _ = V.maximum
  threshold _ th = V.map (\v -> if v > th then v else 0)

onVertices :: ClusterConf -> (V.Vector Float -> a) -> V.Vector Float -> [a]
onVertices conf op vals =
  map (\t -> op $ V.slice (t*(nVerts conf)) (nVerts conf) vals) [0..(nTimes conf)-1]

data MaxClusterSize = MaxClusterSize ClusterConf deriving (Show)
instance Method MaxClusterSize where
  apply (MaxClusterSize conf) values =
    let cs = concat $ onVertices conf (clusters (graph conf) (>(thresh conf))) values
    in if null cs
       then 0
       else fromIntegral $ maximum $ map length cs
  threshold (MaxClusterSize conf) th values =
    let fcs = filter ((>= th) . fromIntegral . length) . clusters (graph conf) (>(thresh conf))
    in V.concat $ onVertices conf (\piece -> applyClusters piece (fcs piece)) values

modAbs :: Method m => m -> ModifiedMethod m
modAbs = ModifiedMethod (V.map abs)

modClusterThinning :: Method m => ClusterConf -> m -> ModifiedMethod m
modClusterThinning conf =
  ModifiedMethod (V.concat . onVertices conf (clusterThinning (graph conf) (>(thresh conf))))
