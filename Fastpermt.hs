module Fastpermt where

import Control.Monad
import Fastpermt.Stat
import Fastpermt.Util
import Fastpermt.Methods
import Fastpermt.Cluster
import Data.List
import System.Random (mkStdGen, randoms)
import System.Environment (getArgs)
import System.IO (stdout, stderr, hPutStrLn)
import qualified Data.ByteString.Lazy as BS
import qualified Data.Vector.Unboxed as V

main :: IO ()
main = do
  (meth:sCount:fnames) <- getArgs
  let nperm = read sCount :: Int
      [condAfnames,condBfnames] = transpose $ grouped 2 fnames

  -- load stc files for both conditions
  a <- mapM readStc condAfnames
  b <- mapM readStc condBfnames
  let nTimes = n_times $ head a
      nVert = n_vertices $ head a
  hPutStrLn stderr ("nTimes = " ++ show nTimes)
  hPutStrLn stderr ("nTimes = " ++ show nVert)

  -- read graph
  graph <- readGraph "aux/graph"

  -- select permutation method
  let method = case meth of
        "maxt" -> AnyMethod (MaxThreshold)
        "maxclust05" -> AnyMethod (modAbs $ MaxClusterSize graph nTimes nVert 2.1314)
        "maxclust05thin" -> AnyMethod (modAbs $ modClusterThinning graph 2.1314 nTimes nVert $ MaxClusterSize graph nTimes nVert 2.1314)
        m -> error ("Unknown permutation method: " ++ show m)

  -- meat of the algo
  let g = mkStdGen 5582031 -- pre-generated random seed, to ensure stable results
      pm = grouped (length a) $ take (length a * nperm) (randoms g :: [Bool])
      op as bs = apply method (vectorTTest as bs)
      distribution = applyPermutation op pm (map stc_data a) (map stc_data b)
      thresh = sort distribution !! (floor $ fromIntegral (length distribution) * (0.95::Double))

      origSpm = vectorTTest (map stc_data a) (map stc_data b)
      corrSpm = threshold method thresh origSpm
      outStc = (head a) { stc_data = corrSpm }

  hPutStrLn stderr "orig"
  forM_ [0..nTimes-1] $ \t ->
    hPutStrLn stderr $ show $ filter (>100) $ map length $ clusters graph (>2.1314) $ V.slice (t*nVert) nVert $ (V.map abs origSpm)
  hPutStrLn stderr "corr"
  forM_ [0..nTimes-1] $ \t ->
    hPutStrLn stderr $ show $ map length $ clusters graph (>2.1314) $ V.slice (t*nVert) nVert $ (V.map abs corrSpm)


  hPutStrLn stderr (show distribution)
  hPutStrLn stderr (show thresh)

  BS.hPut stdout (writeStc outStc)

applyPermutation :: Floating f => ([a] -> [a] -> f) -> [[Bool]] -> [a] -> [a] -> [f]
applyPermutation _ [] _ _ = []
applyPermutation op pm as bs = let (asp, bsp) = permute (as, bs) (head pm)
                               in op asp bsp : applyPermutation op (tail pm) asp bsp

permute :: ([a],[a]) -> [Bool] -> ([a],[a])
permute (as, bs) ps = permute' as bs ps [] []
  where permute' [] [] [] accA accB = (reverse accA, reverse accB)
        permute' (a:as') (b:bs') (p:ps') accA accB = if p
                                                     then permute' as' bs' ps' (a:accA) (b:accB)
                                                     else permute' as' bs' ps' (b:accA) (a:accB)
        permute' _ _ _ _ _ = undefined
