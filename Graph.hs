{-# LANGUAGE ForeignFunctionInterface #-}
module Fastpermt.Graph (Graph, readGraph, getGraph, grlookup, emptyGraph, CGraph, toCGraph) where

import System.IO.Unsafe
import Foreign (Ptr, withArray)
import Foreign.C
import Control.Monad
import Data.Binary
import Data.Binary.Get
import Data.Maybe
import qualified Data.ByteString.Lazy as BS
import qualified Data.Map as M

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

emptyGraph :: M.Map Int [Int]
emptyGraph = M.empty

type CGraph = Ptr ()
toCGraph :: Graph -> CGraph
toCGraph graph = unsafePerformIO $ do
  cg <- create_graph
  forM_ (M.toList graph) $ \(k,vals) -> do
    withArray (map fromIntegral vals) $ \pvals ->
      add_to_graph cg (fromIntegral k) (fromIntegral $ length vals) pvals
  return cg

foreign import ccall "create_graph" create_graph
  :: IO CGraph

foreign import ccall "add_to_graph" add_to_graph
  :: CGraph -> CInt -> CInt -> Ptr CInt -> IO ()