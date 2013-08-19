module Fastpermt (main) where

import Control.Monad
import Data.List
import Data.Maybe
import Fastpermt.Cluster
import Fastpermt.Config
import Fastpermt.Graph
import Fastpermt.Labels
import Fastpermt.Methods
import Fastpermt.Stat
import Fastpermt.Stc
import Fastpermt.Util
import System.Console.CmdArgs
import System.Random (mkStdGen, randoms)
import Text.Printf
import qualified Data.ByteString.Lazy as BS
import qualified Data.Vector.Storable as V

main :: IO ()
main = do
  config <- cmdArgs confModes
  reject <- maybe (return id) (fmap applyIgnoreLabel . readLabel) (ignoreLabelFile config)
  mesh <- maybe (return emptyGraph) readGraph (graphFile config)
  case config of
    conf@Conf{} -> do
      -- load stc files for both conditions
      [a, b] <- fmap (transpose . grouped 2) $
                mapM (fmap (reject . truncateTime (tMin conf) (tMax conf)) . readStc) (stcs conf)

      -- select permutation method
      let cc = convertGraph (spatioTemporal conf) $
               ClusterConf { thresh = fromMaybe (p2t (length a) 0.05) (clusterThreshold conf)
                           , graph = mesh
                           , nVerts = n_vertices $ head a
                           , nTimes = n_times $ head a
                           }
          meth = getMethod conf cc

      -- meat of the algo
      let g = mkStdGen 5582031 -- pre-generated random seed, to ensure stable results
          pm = grouped (length a) $ take (length a * count conf) (randoms g :: [Bool])
          op as bs = apply meth (vectorTTest as bs)
          distribution = applyPermutation op pm (map stc_data a) (map stc_data b)
          cutoff = sort distribution !! floor (fromIntegral (length distribution) * (0.95::Double))
          origSpm = vectorTTest (map stc_data a) (map stc_data b)
          corrSpm = threshold meth cutoff origSpm
          outStc = (head a) { stc_data = corrSpm }

      forM_ (zip3 [(1::Int)..] distribution pm) $ \(i, v, r) ->
         printf "%05d: %7.2f (%s)\n" i v (map (\f -> if f then '-' else '/') r)

      putStrLn ("thresh: " ++ show cutoff)
      putStrLn ("orig: " ++ show (apply meth (vectorTTest (map stc_data a) (map stc_data b))))

      BS.writeFile (outputFile conf) (writeStc outStc)

    conf@GetClusters{} -> do
      stc <- fmap (reject . truncateTime (tMin conf) (tMax conf)) $ readStc $ gcStc conf
      let cc = ClusterConf { thresh = fromMaybe 0 (clusterThreshold conf)
                           , graph = mesh
                           , nVerts = n_vertices stc
                           , nTimes = n_times stc
                           }
          gc = filter ((> gcMinClusterSize conf) . length) . clusters mesh (> thresh cc)
          clsts = onVertices cc (gc . V.map abs) (stc_data stc)
          times = [(round $ tmin stc),(round $ tmin stc + tstep stc)..] :: [Int]
      if shortFormat conf
        then putStrLn $ unwords $ map (show . fst) $
             filter (not . null . snd) $ zip times clsts
        else forM_ (zip times clsts) $ \(t, cs) ->
          when (length cs > 0) $
            printf "t = %3d: %s\n" t (intercalate "," $ map (show . length) cs)

getMethod :: Config -> ClusterConf -> AnyMethod
getMethod conf cc =
  let meth = case method conf of
        "id" -> AnyMethod IdMethod
        "maxt" -> AnyMethod MaxThreshold
        "maxclust" -> AnyMethod $ MaxClusterSize cc
        "maxmass" -> AnyMethod $ MaxClusterMass cc
        _ -> undefined
      thin' = if thinClusters conf then AnyMethod . modClusterThinning cc else id
      tfce' = if applyTFCE conf then AnyMethod . modTFCE (graph cc) else id
  in AnyMethod $ modFiltNaN $ modAbs $ tfce' $ thin' meth

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
