{-# LANGUAGE DeriveDataTypeable #-}
module Fastpermt.Config (confModes, Config(..), ClusterConf(..), emptyCC, ThresholdDir(..)) where

import Foreign.C
import Fastpermt.Graph
import System.Console.CmdArgs

data ThresholdDir = Positive | Negative | Both deriving (Data, Typeable, Show, Eq)

data Config = Conf { method :: String
                   , stcs :: [FilePath]
                   , count :: Int
                   , clusterThreshold :: Maybe Float
                   , clusterThresholdDir :: ThresholdDir
                   , pValue :: Float
                   , graphFile :: Maybe FilePath
                   , thinClusters :: Bool
                   , applyTFCE :: Bool
                   , spatioTemporal :: Bool
                   , ignoreLabelFile :: Maybe FilePath
                   , tMin :: Maybe Float
                   , tMax :: Maybe Float
                   , outputFile :: FilePath
                   }
            | GetClusters { gcMinClusterSize :: Int
                          , shortFormat :: Bool
                          , clusterThreshold :: Maybe Float
                          , graphFile :: Maybe FilePath
                          , ignoreLabelFile :: Maybe FilePath
                          , tMin :: Maybe Float
                          , tMax :: Maybe Float
                          , gcStc :: FilePath
                          }
            deriving (Show, Data, Typeable)

defConf :: Config
defConf =
  Conf { method = "maxt" &= typ "NAME" &= help "permutation statistic to use"
       , stcs = [] &= args &= typ "STC"
       , count = 1000 &= help "number of permutations"
       , clusterThreshold = Nothing &= explicit &= name "cluster-threshold" &= name "t" &=
                            help "voxel value cut-off threshold"
       , clusterThresholdDir = Both &= explicit &= name "cluster-threshold-dir" &= name "d" &= typ "DIR" &=
                               help "direction of voxel value cut-off threshold - accept positive, negative, or both"
       , pValue = 0.05 &= typ "FLOAT" &= explicit &= name "p-value" &= name "p" &=
                  help "p-value at which we should reject the null hypothesis"
       , graphFile = Nothing &= typFile &= explicit &= name "graph-file" &= name "g" &=
                     help "graph file for cluster algorithms"
       , thinClusters = False &= explicit &= name "thin-clusters" &=
                          help "apply cluster thinning?"
       , applyTFCE = False &= explicit &= name "tfce" &=
                     help "apply tfce preprocessing to data"
       , spatioTemporal = False &= explicit &= name "spatio-temporal" &=
                          help "when searching for clusters, do not limit cluster to single time point"
       , ignoreLabelFile = Nothing &= typFile &= explicit &= name "ignore-label" &=
                           help "mne label file with vertices to ignore"
       , tMin = Nothing &= typ "MS" &= help "starting time"
       , tMax = Nothing &= typ "MS" &= help "ending time"
       , outputFile = "" &= typ "STC" &= explicit &= name "output" &= name "o"
       } &= name "run"

getClustersConf :: Config
getClustersConf =
  GetClusters { gcMinClusterSize = 0 &= explicit &= name "min-cluster-size" &= name "m" &=
                                   help "don't output clusters smaller than this"
              , shortFormat = False &= explicit &= name "short" &= name "s" &=
                              help "only output space-separated times for clusters"
              , clusterThreshold = Nothing &= explicit &= name "cluster-threshold" &= name "t" &=
                                   help "voxel value cut-off threshold"
              , graphFile = Nothing &= typFile &= explicit &= name "graph-file" &= name "g" &=
                            help "graph file for cluster algorithms"
              , ignoreLabelFile = Nothing &= typ "FILE" &= explicit &= name "ignore-label" &=
                                    help "mne label file with vertices to ignore"
              , tMin = Nothing &= typ "MS" &= help "starting time"
              , tMax = Nothing &= typ "MS" &= help "ending time"
              , gcStc = "" &= typ "STC" &= argPos 0
              } &= name "clusters"

confModes :: Config
confModes =
  modes [defConf, getClustersConf] &=
  program "fastpermt" &=
  summary "" &=
  versionArg [ignore]

data ClusterConf =
  ClusterConf { test :: CFloat -> Bool
              , dir :: ThresholdDir
              , graph :: Graph
              , nVerts :: Int
              , nTimes :: Int
              }

emptyCC :: ClusterConf
emptyCC = ClusterConf (\_ -> True) Both emptyGraph 0 0