{-# LANGUAGE ExistentialQuantification, NamedFieldPuns #-}
module Fastpermt.Methods ( Method(..)
                         , IdMethod(..)
                         , MaxThreshold(..)
                         , MaxClusterSize(..)
                         , MaxClusterMass(..)
                         , AnyMethod(..)
                         , modAbs
                         , modFiltNaN
                         , modClusterThinning
                         , modTFCE
                         , onVertices
                         ) where

import Fastpermt.Cluster
import Fastpermt.Config
import Fastpermt.Graph
import Fastpermt.Util (onVertices)
import qualified Data.Vector.Unboxed as V

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


data IdMethod = IdMethod deriving (Eq, Show)
instance Method IdMethod where
  apply _ _ = 0
  threshold _ _ = id

data MaxThreshold = MaxThreshold deriving (Eq, Show)
instance Method MaxThreshold where
  apply _ = V.maximum
  threshold _ th = V.map (\v -> if v > th then v else 0)

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

data MaxClusterMass = MaxClusterMass ClusterConf deriving (Show)
instance Method MaxClusterMass where
  apply (MaxClusterMass conf) =
    maximum . concat . onVertices conf (
      \piece ->
      map (sum . map (\c -> piece V.! c)) $
      clusters (graph conf) (>(thresh conf)) piece
    )
  threshold (MaxClusterMass conf) th =
    let fcs piece =
          filter ((>= th) . sum . map (\c -> piece V.! c)) $
          clusters (graph conf) (>(thresh conf)) piece
    in V.concat . onVertices conf (\piece -> applyClusters piece (fcs piece))

modAbs :: Method m => m -> ModifiedMethod m
modAbs = ModifiedMethod (V.map abs)

modFiltNaN :: Method m => m -> ModifiedMethod m
modFiltNaN = ModifiedMethod (V.map (\v -> if v /= v then 0 else v))

modClusterThinning :: Method m => ClusterConf -> m -> ModifiedMethod m
modClusterThinning conf =
  ModifiedMethod (V.concat . onVertices conf (clusterThinning (graph conf) (>(thresh conf))))

modTFCE :: Method m => Graph -> m -> ModifiedMethod m
modTFCE graph = ModifiedMethod (tfce graph)
