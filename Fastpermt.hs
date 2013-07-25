module Fastpermt (main) where

import Control.Monad
import Data.List
import Fastpermt.Cluster
import Fastpermt.Config
import Fastpermt.Graph
import Fastpermt.Labels
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
  reject <- maybe (return id) (fmap applyIgnoreLabel . readLabel) (ignoreLabelFile config)
  mesh <- maybe (return emptyGraph) (readGraph) (graphFile config)
  case config of
    conf@GetClusters{} -> do
      stc <- fmap reject $ readStc $ gcStc conf
      let cc = ClusterConf { thresh = (clusterThreshold conf)
                           , graph = mesh
                           , nVerts = n_vertices stc
                           , nTimes = n_times stc
                           }
          thin = if noThinClusters conf
                 then id
                 else clusterThinning mesh (>(thresh cc))
          gc = filter ((> (gcMinClusterSize conf)) . length) . clusters mesh (>(thresh cc))
      forM_ (zip [(1::Int)..] (onVertices cc (gc . thin . V.map abs) (stc_data stc))) $ \(t, cs) -> do
        when (length cs > 0) $
          hPutStrLn stderr $ printf "t = %3d: %s" t (intercalate "," $ map (show . length) cs)
    conf@Conf{} -> do
      -- load stc files for both conditions
      [a, b] <- fmap (transpose . grouped 2) $ mapM (fmap reject . readStc) (stcs conf)

      -- select permutation method
      meth <- case (method conf) of
        "maxt" -> return $ AnyMethod (modFiltNaN $ modAbs $ MaxThreshold)
        m | m `elem` ["maxclust", "maxmass"] -> do
          let cc = ClusterConf { thresh = (clusterThreshold conf)
                               , graph = mesh
                               , nVerts = n_vertices $ head a
                               , nTimes = n_times $ head a
                               }
          return $ case m of
            "maxclust" | noThinClusters conf -> AnyMethod $ modFiltNaN $ modAbs $ MaxClusterSize cc
            "maxclust" -> AnyMethod $ modFiltNaN $ modAbs $ modClusterThinning cc $ MaxClusterSize cc
            "maxmass" | noThinClusters conf -> AnyMethod $ modFiltNaN $ modAbs $ MaxClusterMass cc
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
