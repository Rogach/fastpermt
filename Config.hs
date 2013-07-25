{-# LANGUAGE DeriveDataTypeable #-}
module Fastpermt.Config (confModes, Config(..), ClusterConf(..)) where

import Fastpermt.Graph
import System.Console.CmdArgs

data Config = TestRun
            | Conf { method :: String
                  , stcs :: [FilePath]
                  , count :: Int
                  , clusterThreshold :: Float
                  , graphFile :: FilePath
                  , thinClusters :: Bool
                  , nVertsOpt :: Maybe Int
                  , nTimesOpt :: Maybe Int
                  }
            | GetClusters { gcMinClusterSize :: Int
                          , gcClusterThreshold :: Float
                          , gcThinClusters :: Bool
                          , gcGraphFile :: FilePath
                          , gcStc :: FilePath
                          , gcNVertsOpt :: Maybe Int
                          , gcNTimesOpt :: Maybe Int
                          }
            deriving (Show, Data, Typeable)

testRunConf :: Config
testRunConf = TestRun

defConf :: Config
defConf = Conf { method = "maxt" &= typ "NAME" &= help "permutation statistic to use"
               , stcs = [] &= args &= typ "STC"
               , count = 300 &= help "number of permutations"
               , clusterThreshold = 2.1314 &= explicit &= name "cluster-threshold" &=
                                    help "voxel t-statistic value cut-off threshold"
               , graphFile = "" &= typFile &= explicit &= name "graph-file" &=
                             help "graph file for cluster algorithms"
               , thinClusters = False &= explicit &= name "thin-clusters" &=
                                help "apply cluster thinning?"
               , nVertsOpt = Nothing &= explicit &= name "nverts"
               , nTimesOpt = Nothing &= explicit &= name "ntimes"
               } &= name "run"

getClustersConf :: Config
getClustersConf = GetClusters { gcMinClusterSize = 0 &= explicit &= name "min-cluster-size" &=
                                                   help "don't output clusters smaller than this"
                              , gcClusterThreshold = 0 &= explicit &= name "cluster-threshold" &=
                                                     help "voxel value cut-off threshold"
                              , gcThinClusters = False &= explicit &= name "thin-clusters" &=
                                                 help "apply cluster thinning?"
                              , gcGraphFile = "" &= typFile &= explicit &= name "graph-file" &=
                                              help "graph file for cluster algorithms"
                              , gcStc = "" &= typ "STC" &= argPos 0
                              , gcNVertsOpt = Nothing &= explicit &= name "nverts"
                              , gcNTimesOpt = Nothing &= explicit &= name "ntimes"
                              } &= name "clusters"

confModes :: Config
confModes =
  modes [defConf, getClustersConf, testRunConf] &=
  program "fastpermt" &=
  summary "" &=
  versionArg [ignore]

data ClusterConf = ClusterConf { thresh :: Float
                               , graph :: Graph
                               , nVerts :: Int
                               , nTimes :: Int
                               } deriving Show
