{-# LANGUAGE ExistentialQuantification, NamedFieldPuns #-}
module Fastpermt.Methods ( Method(..)
                         , MaxThreshold(..)
                         , MaxClusterSize(..)
                         , AnyMethod(..)
                         , modAbs
                         , modClusterThinning
                         ) where

import qualified Data.Vector.Unboxed as V
import Fastpermt.Cluster
import Fastpermt.Util

class Method a where
  apply :: a -> V.Vector Float -> Float
  threshold :: a -> Float -> V.Vector Float -> V.Vector Float

data AnyMethod = forall m. Method m => AnyMethod m
instance Method AnyMethod where
  apply (AnyMethod m) values = apply m values
  threshold (AnyMethod m) th values = threshold m th values

data MaxThreshold = MaxThreshold deriving (Eq, Show)
instance Method MaxThreshold where
  apply _ values = V.maximum $ V.map abs $ values
  threshold _ th values = V.map (\v -> if (abs v > th) then abs v else 0) values

data MaxClusterSize = MaxClusterSize { grph :: Graph, times :: Int, vertices :: Int, cutoff :: Float } deriving (Eq, Show)
instance Method MaxClusterSize where
  apply (MaxClusterSize { grph, times, vertices, cutoff }) values =
    let cs = concat $ map (\t -> clusters grph (>cutoff) (V.slice (t*vertices) vertices values)) [0..times-1]
    in if null cs
       then 0
       else fromIntegral $ maximum $ map length cs
  threshold (MaxClusterSize { grph, times, vertices, cutoff }) th values =
    pDebug "tcc" V.sum $ V.concat $ map (\t -> let piece = V.slice (t*vertices) vertices values
                                                   cs = clusters grph (>cutoff) piece
                                                   fcs = filter ((>= th) . fromIntegral . length) cs
                                               in applyClusters piece fcs) [0..times-1]

data ModifiedMethod m = ModifiedMethod (V.Vector Float -> V.Vector Float) m
instance Method m => Method (ModifiedMethod m) where
  apply (ModifiedMethod modify meth) values = apply meth (modify values)
  threshold (ModifiedMethod modify meth) th values = threshold meth th (modify values)

modAbs :: Method m => m -> ModifiedMethod m
modAbs meth = ModifiedMethod (V.map abs) meth

modClusterThinning :: Method m => Graph -> Float -> Int -> Int -> m -> ModifiedMethod m
modClusterThinning graph cutoff times vertices meth =
  let cth values t = V.slice (t*vertices) vertices values
  in ModifiedMethod (\values -> V.concat $ map (clusterThinning graph (>cutoff) . cth values) [0..times-1]) meth
