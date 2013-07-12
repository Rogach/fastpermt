module Fastpermt.Util ( readDoubles
                      , writeDoubles
                      , readGraph
                      , Graph
                      , grouped
                      ) where

import qualified Data.ByteString.Lazy as BS
import qualified Data.Map as M
import Data.Binary
import Data.Binary.Get
import Data.Binary.Put
import Unsafe.Coerce
import Control.Monad

readDoubles :: Int -> FilePath -> IO [Double]
readDoubles n f = liftM (runGet $ getDoubles n) (BS.readFile f)

getDoubles :: Int -> Get [Double]
{-# INLINE getDoubles #-}
getDoubles = go []
 where
   go xs 0 = return $! reverse xs
   go xs i = do
     x <- fmap unsafeCoerce getWord64le
     x `seq` go (x:xs) (i-1)

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

grouped :: Int -> [a] -> [[a]]
grouped _ [] = []
grouped i xs = take i xs : grouped i (drop i xs)

writeDoubles :: FilePath -> [Double] -> IO ()
writeDoubles f xs = BS.writeFile f (runPut $ mapM_ (putWord64be . unsafeCoerce) xs)