{-# LANGUAGE ForeignFunctionInterface #-}
module Fastpermt.Cluster where

import System.IO.Unsafe
import Foreign (newForeignPtr, finalizerFree, touchForeignPtr, Ptr)
import Foreign.C
import Foreign.ForeignPtr.Unsafe (unsafeForeignPtrToPtr)
import Fastpermt.Graph
import qualified Data.Set as S
import qualified Data.Vector.Storable as V

applyClusters :: (Floating f, V.Storable f) => V.Vector f -> [[Int]] -> V.Vector f
applyClusters arr = foldl (\narr cs -> narr V.// map (\i -> (i, 1)) cs) (V.replicate (V.length arr) 0)

-- | Find all clusters in a vector, according to given test
-- | Returns lists of indices
clusters :: (Floating f, V.Storable f) => Graph -> (f -> Bool) -> V.Vector f -> [[Int]]
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

clusterThinning :: (Floating f, V.Storable f) => Graph -> (f -> Bool) -> V.Vector f -> V.Vector f
clusterThinning graph test larr =
  let shrink arr = V.imap (\i v ->
                            if test v && all test (map (arr V.!) (grlookup graph i))
                            then v else 0) arr
      expand arr = V.imap (\i v ->
                            if test v || any test (map (arr V.!) (grlookup graph i))
                            then larr V.! i else 0) arr
  in expand $ shrink larr

fastTfce :: CGraph -> V.Vector CFloat -> V.Vector CFloat
fastTfce graph arr = unsafePerformIO $ do
  let n = V.length arr
      arrP = fst $ V.unsafeToForeignPtr0 arr
  res <- fast_tfce (fromIntegral n) (unsafeForeignPtrToPtr $ arrP) graph
  p <- newForeignPtr finalizerFree res
  touchForeignPtr arrP -- ensure that haskell doesn't garbage collect the data while we are computing
  return $ V.unsafeFromForeignPtr0 p n

foreign import ccall "fast_tfce" fast_tfce
  :: CInt -> Ptr CFloat -> CGraph -> IO (Ptr CFloat)

tfce :: (Ord f, Enum f, Floating f, V.Storable f) => Graph -> V.Vector f -> V.Vector f
tfce graph larr =
  let max = V.maximum larr
      delta = max/50
      ts = takeWhile (<max) [delta/2,delta*3/2..]
  in foldl (\arr t ->
             let cs = clusters graph (>t) larr
             in foldl (\arr ci ->
                        let v = (fromIntegral (length ci) ** (2/3)) * (t ** 2) * delta
                        in arr V.// map (\i -> (i, arr V.! i + v)) ci) arr cs
             ) (V.replicate (V.length larr) 0) ts
