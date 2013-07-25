module Fastpermt (main) where

import Control.Monad
import Data.List
import Data.Maybe
import Fastpermt.Cluster
import Fastpermt.Config
import Fastpermt.Graph
import Fastpermt.Methods
import Fastpermt.Stat
import Fastpermt.Stc
import Fastpermt.Util
import System.Console.CmdArgs
import System.IO (stdout, stderr, hPutStrLn)
import System.Random (mkStdGen, randoms)
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
          cc = ClusterConf { thresh = (gcClusterThreshold conf)
                           , graph = mesh
                           , nVerts = nv
                           , nTimes = nt
                           }
          thin = if gcThinClusters conf
                 then clusterThinning mesh (>(thresh cc))
                 else id
          gc = filter ((> (gcMinClusterSize conf)) . length) . clusters mesh (>(thresh cc))
      forM_ (zip [(1::Int)..] (onVertices cc (gc . thin . V.map abs) (stc_data stc))) $ \(t, cs) -> do
        when (length cs > 0) $
          hPutStrLn stderr $ printf "t = %3d: %s" t (intercalate "," $ map (show . length) cs)
    conf@Conf{} -> do
      -- load stc files for both conditions
      [a, b] <- fmap (transpose . grouped 2) $ mapM readStc (stcs conf)

      let nt = fromMaybe (n_times $ head a) (nTimesOpt conf)
          nv = fromMaybe (n_vertices $ head a) (nVertsOpt conf)

      -- select permutation method
      meth <- case (method conf) of
        "maxt" -> return $ AnyMethod (modFiltNaN $ modAbs $ MaxThreshold)
        m | m `elem` ["maxclust", "maxmass"] -> do
          mesh <- readGraph (graphFile conf)
          let cc = ClusterConf { thresh = (clusterThreshold conf)
                               , graph = mesh
                               , nVerts = nv
                               , nTimes = nt
                               }
          return $ case m of
            "maxclust" | not (thinClusters conf) -> AnyMethod $ modFiltNaN $ modAbs $ MaxClusterSize cc
            "maxclust" -> AnyMethod $ modFiltNaN $ modAbs $ modClusterThinning cc $ MaxClusterSize cc
            "maxmass" | not (thinClusters conf) -> AnyMethod $ modFiltNaN $ modAbs $ MaxClusterMass cc
            "maxmass" -> AnyMethod $ modFiltNaN $ modAbs $ modClusterThinning cc $ MaxClusterMass cc
            _ -> error ("Unknown permutation method: " ++ show m)
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
                               in op asp bsp : applyPermutation op (tail pm) as bs

permute :: ([a],[a]) -> [Bool] -> ([a],[a])
permute (as, bs) ps = permute' as bs ps [] []
  where permute' [] [] [] accA accB = (reverse accA, reverse accB)
        permute' (a:as') (b:bs') (p:ps') accA accB = if p
                                                     then permute' as' bs' ps' (a:accA) (b:accB)
                                                     else permute' as' bs' ps' (b:accA) (a:accB)
        permute' _ _ _ _ _ = undefined
