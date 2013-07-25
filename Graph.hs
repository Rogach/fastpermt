module Fastpermt.Graph(Graph, readGraph, getGraph, grlookup, emptyGraph) where

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