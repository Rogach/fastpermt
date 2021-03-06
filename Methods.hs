{-# LANGUAGE ExistentialQuantification, NamedFieldPuns #-}
module Fastpermt.Methods ( Method(..)
                         , IdMethod(..)
                         , SumMethod(..)
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

import Foreign.C
import Fastpermt.Cluster
import Fastpermt.Config
import Fastpermt.Graph
import Fastpermt.Util (onVertices)
import qualified Data.Vector.Storable as V

class Method a where
  apply :: a -> V.Vector CFloat -> CFloat
  threshold :: a -> CFloat -> V.Vector CFloat -> V.Vector CFloat

data ModifiedMethod m = ModifiedMethod (V.Vector CFloat -> V.Vector CFloat) m
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

-- simple method, that sums all values in input array to get threshold
-- useful for testing
data SumMethod = SumMethod deriving (Eq, Show)
instance Method SumMethod where
  apply _ = V.sum
  threshold _ _ = id

data MaxThreshold = MaxThreshold ClusterConf
instance Method MaxThreshold where
  apply (MaxThreshold conf) values =
    case dir conf of
      Both -> V.maximum values
      Positive -> V.maximum values
      Negative -> - V.minimum values
  threshold (MaxThreshold conf) th values =
    case dir conf of
      Both -> V.map (\v -> if v > th then 1 else 0) values
      Positive -> V.map (\v -> if v > th then 1 else 0) values
      Negative -> V.map (\v -> if v < (-th) then 1 else 0) values

data MaxClusterSize = MaxClusterSize ClusterConf
instance Method MaxClusterSize where
  apply (MaxClusterSize conf) values =
    let cs = clusters (graph conf) (test conf) values
    in if null cs
       then 0
       else fromIntegral $ maximum $ map length cs
  threshold (MaxClusterSize conf) th values =
    let fcs = filter ((>= th) . fromIntegral . length) . clusters (graph conf) (test conf)
    in applyClusters values (fcs values)

data MaxClusterMass = MaxClusterMass ClusterConf
instance Method MaxClusterMass where
  apply (MaxClusterMass conf) values =
    let cs = clusters (graph conf) (test conf) values
    in if null cs
       then 0
       else maximum $ map (abs . sum . map (\c -> values V.! c)) cs
  threshold (MaxClusterMass conf) th values =
    let fcs = filter ((>= th) . abs . sum . map (\c -> values V.! c)) . clusters (graph conf) (test conf)
    in applyClusters values (fcs values)

modAbs :: Method m => m -> ModifiedMethod m
modAbs = ModifiedMethod (V.map abs)

modFiltNaN :: Method m => m -> ModifiedMethod m
modFiltNaN = ModifiedMethod (V.map (\v -> if v /= v then 0 else v))

modClusterThinning :: Method m => ClusterConf -> m -> ModifiedMethod m
modClusterThinning conf =
  ModifiedMethod (clusterThinning (graph conf) (test conf))

modTFCE :: Method m => CGraph -> m -> ModifiedMethod m
modTFCE graph = ModifiedMethod (fastTfce graph)
