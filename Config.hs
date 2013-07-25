{-# LANGUAGE DeriveDataTypeable #-}
module Fastpermt.Config (confModes, Config(..), ClusterConf(..), emptyCC) where

import Fastpermt.Graph
import System.Console.CmdArgs

data Config =
  TestRun
  | Conf { method :: String
         , stcs :: [FilePath]
         , count :: Int
         , clusterThreshold :: Float
         , graphFile :: FilePath
         , noThinClusters :: Bool
         , ignoreLabelFile :: Maybe FilePath
         }
  | GetClusters { gcMinClusterSize :: Int
                , gcClusterThreshold :: Float
                , gcNoThinClusters :: Bool
                , gcGraphFile :: FilePath
                , gcIgnoreLabelFile :: Maybe FilePath
                , gcStc :: FilePath
                }
  deriving (Show, Data, Typeable)

testRunConf :: Config
testRunConf = TestRun

defConf :: Config
defConf =
  Conf { method = "maxt" &= typ "NAME" &= help "permutation statistic to use"
       , stcs = [] &= args &= typ "STC"
       , count = 1000 &= help "number of permutations"
       , clusterThreshold = 2.1314 &= explicit &= name "cluster-threshold" &= name "t" &=
                            help "voxel t-statistic value cut-off threshold"
       , graphFile = "" &= typFile &= explicit &= name "graph-file" &= name "g" &=
                     help "graph file for cluster algorithms"
       , noThinClusters = False &= explicit &= name "no-thin-clusters" &=
                          help "supress cluster thinning?"
       , ignoreLabelFile = Nothing &= typ "FILE" &= explicit &= name "ignore-label" &=
                           help "mne label file with vertices to ignore"
       } &= name "run"

getClustersConf :: Config
getClustersConf =
  GetClusters { gcMinClusterSize = 0 &= explicit &= name "min-cluster-size" &= name "m" &=
                                   help "don't output clusters smaller than this"
              , gcClusterThreshold = 0 &= explicit &= name "cluster-threshold" &= name "t" &=
                                     help "voxel value cut-off threshold"
              , gcNoThinClusters = False &= explicit &= name "no-thin-clusters" &=
                                   help "suppress cluster thinning?"
              , gcGraphFile = "" &= typFile &= explicit &= name "graph-file" &= name "g" &=
                              help "graph file for cluster algorithms"
              , gcIgnoreLabelFile = Nothing &= typ "FILE" &= explicit &= name "ignore-label" &=
                                    help "mne label file with vertices to ignore"
              , gcStc = "" &= typ "STC" &= argPos 0
              } &= name "clusters"

confModes :: Config
confModes =
  modes [defConf, getClustersConf, testRunConf] &=
  program "fastpermt" &=
  summary "" &=
  versionArg [ignore]

data ClusterConf =
  ClusterConf { thresh :: Float
              , graph :: Graph
              , nVerts :: Int
              , nTimes :: Int
              } deriving Show

emptyCC :: ClusterConf
emptyCC = ClusterConf 0 emptyGraph 0 0