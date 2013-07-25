module Fastpermt.Util ( readGraph
                      , Graph
                      , grouped
                      , Stc(..)
                      , readStc
                      , writeStc
                      , grlookup
                      , pDebug
                      ) where

import qualified Data.ByteString.Lazy as BS
import qualified Data.Map as M
import qualified Data.Vector.Unboxed as V
import Data.Maybe
import Data.Binary
import Data.Binary.Get
import Data.Binary.Put
import Unsafe.Coerce
import Control.Monad
import Debug.Trace

data Stc = Stc { tmin :: Float
               , tstep :: Float
               , n_vertices :: Int
               , n_times :: Int
               , stc_vertices :: [Int]
               , stc_data :: V.Vector Float
               } deriving (Show)

readStc :: FilePath -> IO Stc
readStc f = liftM decode (BS.readFile f)

writeStc :: Stc -> BS.ByteString
writeStc stc = encode stc

instance Binary Stc where
  get = do
    tMin <- getFloat32be
    tStep <- getFloat32be
    nVertices <- getInt32be
    sourceVector <- replicateM nVertices getInt32be
    nTimes <- getInt32be
    values <- V.replicateM (nVertices * nTimes) getFloat32be
    return $ Stc tMin tStep nVertices nTimes sourceVector values
  put (Stc tMin tStep nVertices nTimes sourceVector values) = do
    putFloat32be tMin
    putFloat32be tStep
    putInt32be nVertices
    mapM_ putInt32be sourceVector
    putInt32be nTimes
    V.mapM_ putFloat32be values

-- helpers for 32bit big-endian io
getInt32be :: Get Int
getInt32be = fmap unsafeCoerce getWord32be
getFloat32be :: Get Float
getFloat32be = fmap unsafeCoerce getWord32be
putInt32be :: Int -> Put
putInt32be i = putWord32be (unsafeCoerce i)
putFloat32be :: Float -> Put
putFloat32be f = putWord32be (unsafeCoerce f)

type Graph = M.Map Int [Int]

readGraph :: FilePath -> IO Graph
readGraph f = liftM (runGet getGraph) (BS.readFile f)

getGraph :: Get Graph
getGraph = do
  empty <- isEmpty
  if empty
    then return M.empty
    else do
      i <- get
      n <- get
      vs <- replicateM n get
      rest <- getGraph
      return $ M.insert i vs rest

grlookup :: Graph -> Int -> [Int]
grlookup graph i = fromMaybe [] (M.lookup i graph)

grouped :: Int -> [a] -> [[a]]
grouped _ [] = []
grouped n xs = take n xs : grouped n (drop n xs)

pDebug :: (Show b) => String -> (a -> b) -> a -> a
pDebug msg str a = trace (msg ++ (show $ str a)) a
