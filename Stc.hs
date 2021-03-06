module Fastpermt.Stc(Stc(..), readStc, writeStc, truncateTime) where

import Foreign.C
import Control.Monad
import Data.Binary
import Fastpermt.Util
import qualified Data.ByteString.Lazy as BS
import qualified Data.Vector.Storable as V

data Stc = Stc { tmin :: CFloat
               , tstep :: CFloat
               , n_vertices :: Int
               , n_times :: Int
               , stc_vertices :: [Int]
               , stc_data :: V.Vector CFloat
               } deriving (Show)

readStc :: FilePath -> IO Stc
readStc f = liftM decode (BS.readFile f)

writeStc :: Stc -> BS.ByteString
writeStc = encode

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

truncateTime :: Maybe CFloat -> Maybe CFloat -> Stc -> Stc
truncateTime Nothing to stc@Stc { tmin = tm } = truncateTime (Just tm) to stc
truncateTime from Nothing stc@Stc { tmin = tm, tstep = step, n_times = nt } =
  truncateTime from (Just $ tm + step * fromIntegral (nt-1)) stc
truncateTime (Just from) (Just to) stc@Stc { tmin = tm, tstep = step, n_vertices = nv, n_times = nt } =
  let tmax = tm + step * fromIntegral (nt - 1)
      (before, after) = span (<from) [tm,(tm+step)..tmax]
      (inside, _) = span (<=to) after
  in stc { tmin = head after
         , n_times = length inside
         , stc_data = V.force $ V.slice (length before * nv) (length inside * nv) (stc_data stc)
         }