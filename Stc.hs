module Fastpermt.Stc(Stc(..), readStc, writeStc) where

import Control.Monad
import Data.Binary
import Fastpermt.Util
import qualified Data.ByteString.Lazy as BS
import qualified Data.Vector.Unboxed as V

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
