module Fastpermt (main) where

import Fastpermt.Stat
import Fastpermt.Util
import Fastpermt.Methods
import Fastpermt.Config
import Data.List
import Data.Maybe
import System.Random (mkStdGen, randoms)
import System.IO (stdout, stderr, hPutStrLn)
import System.Console.CmdArgs
import qualified Data.ByteString.Lazy as BS

main :: IO ()
main = do
  config <- cmdArgs confModes
  case config of
    TestRun -> print "test"
    conf@Conf{} -> do
      let [condAfnames,condBfnames] = transpose $ grouped 2 (stcs conf)

      -- load stc files for both conditions
      a <- mapM readStc condAfnames
      b <- mapM readStc condBfnames
      let nt = fromMaybe (n_times $ head a) (nTimesOpt conf)
          nv = fromMaybe (n_vertices $ head a) (nVertsOpt conf)
      hPutStrLn stderr ("nTimes = " ++ show nt)
      hPutStrLn stderr ("nVerts = " ++ show nv)

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
                   then AnyMethod (modAbs $ MaxClusterSize cc)
                   else AnyMethod (modAbs $ modClusterThinning cc $ MaxClusterSize cc)
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

      hPutStrLn stderr ("distribution: " ++ show distribution)
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
