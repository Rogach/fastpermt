module Fastpermt (main) where

import Control.Monad
import Fastpermt.Stat
import Fastpermt.Util
import Fastpermt.Methods
import Fastpermt.Config
import Fastpermt.Cluster
import Data.List
import Data.Maybe
import System.Random (mkStdGen, randoms)
import System.IO (stdout, stderr, hPutStrLn)
import System.Console.CmdArgs
import Text.Printf
import qualified Data.ByteString.Lazy as BS
import qualified Data.Vector.Unboxed as V

main :: IO ()
main = do
  config <- cmdArgs confModes
  case config of
    TestRun -> print "test"
    conf@GetClusters{} -> do
      stc <- readStc $ gcStc conf
      mesh <- readGraph $ gcGraphFile conf
      let nt = fromMaybe (n_times stc) (gcNTimesOpt conf)
          nv = fromMaybe (n_vertices stc) (gcNVertsOpt conf)
          cc = ClusterConf { thresh = 0
                           , graph = mesh
                           , nVerts = nv
                           , nTimes = nt
                           }
          thin = if gcThinClusters conf
                 then clusterThinning mesh (>(thresh cc))
                 else id
          gc = clusters mesh (>(thresh cc))
      forM_ (zip [(1::Int)..] (onVertices cc (gc . thin . V.map abs) (stc_data stc))) $ \(t, cs) -> do
          hPutStrLn stderr $ printf "t = %3d: %s" t (intercalate "," $ map (show . length) cs)
    conf@Conf{} -> do
      -- load stc files for both conditions
      [a, b] <- fmap (transpose . grouped 2) $ mapM readStc (stcs conf)

      let nt = fromMaybe (n_times $ head a) (nTimesOpt conf)
          nv = fromMaybe (n_vertices $ head a) (nVertsOpt conf)

      -- select permutation method
      meth <- case (method conf) of
        "maxt" -> return $ AnyMethod (modAbs $ MaxThreshold)
        "maxclust" -> do
          mesh <- readGraph (graphFile conf)
          let cc = ClusterConf { thresh = (clusterThreshold conf)
                               , graph = mesh
                               , nVerts = nv
                               , nTimes = nt
                               }
          return $ if not (thinClusters conf)
                   then AnyMethod $ modAbs $ MaxClusterSize cc
                   else AnyMethod $ modAbs $ modClusterThinning cc $ MaxClusterSize cc
        m -> error ("Unknown permutation method: " ++ show m)

      -- meat of the algo
      let g = mkStdGen 5582031 -- pre-generated random seed, to ensure stable results
          pm = grouped (length a) $ take (length a * (count conf)) (randoms g :: [Bool])
          op as bs = apply meth (vectorTTest as bs)
          distribution = applyPermutation op pm (map stc_data a) (map stc_data b)
          cutoff = sort distribution !! (floor $ fromIntegral (length distribution) * (0.95::Double))
          origSpm = vectorTTest (map stc_data a) (map stc_data b)
          corrSpm = threshold meth cutoff origSpm
          outStc = (head a) { stc_data = corrSpm }

      forM_ (zip distribution pm) $ \(v, r) -> do
        hPutStrLn stderr $ printf "%7.2f (%s)" v (map (\f -> if f then '-' else '/') r)

      hPutStrLn stderr ("thresh: " ++ show cutoff)
      hPutStrLn stderr ("orig: " ++ show (apply meth (vectorTTest (map stc_data a) (map stc_data b))))

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
