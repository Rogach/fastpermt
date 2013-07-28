{-# LANGUAGE DeriveDataTypeable #-}
module Fastpermt.Config (confModes, Config(..), ClusterConf(..), emptyCC) where

import Fastpermt.Graph
import System.Console.CmdArgs

data Config = Conf { method :: String
                   , stcs :: [FilePath]
                   , count :: Int
                   , clusterThreshold :: Float
                   , graphFile :: Maybe FilePath
                   , noThinClusters :: Bool
                   , ignoreLabelFile :: Maybe FilePath
                   , outputFile :: FilePath
                   }
            | GetClusters { gcMinClusterSize :: Int
                          , shortFormat :: Bool
                          , clusterThreshold :: Float
                          , noThinClusters :: Bool
                          , graphFile :: Maybe FilePath
                          , ignoreLabelFile :: Maybe FilePath
                          , gcStc :: FilePath
                          }
            | ModifyMode { method :: String
                         , inputFile :: FilePath
                         , outputFile :: FilePath
                         , ignoreLabelFile :: Maybe FilePath
                         , graphFile :: Maybe FilePath
                         , clusterThreshold :: Float
                         , noThinClusters :: Bool
                         , methodThresh :: Float
                         }
            deriving (Show, Data, Typeable)

defConf :: Config
defConf =
  Conf { method = "maxt" &= typ "NAME" &= help "permutation statistic to use"
       , stcs = [] &= args &= typ "STC"
       , count = 1000 &= help "number of permutations"
       , clusterThreshold = 2.1314 &= explicit &= name "cluster-threshold" &= name "t" &=
                            help "voxel value cut-off threshold"
       , graphFile = Nothing &= typFile &= explicit &= name "graph-file" &= name "g" &=
                     help "graph file for cluster algorithms"
       , noThinClusters = False &= explicit &= name "no-thin-clusters" &=
                          help "supress cluster thinning?"
       , ignoreLabelFile = Nothing &= typFile &= explicit &= name "ignore-label" &=
                           help "mne label file with vertices to ignore"
       , outputFile = "" &= typ "STC" &= explicit &= name "output" &= name "o"
       } &= name "run"

getClustersConf :: Config
getClustersConf =
  GetClusters { gcMinClusterSize = 0 &= explicit &= name "min-cluster-size" &= name "m" &=
                                   help "don't output clusters smaller than this"
              , shortFormat = False &= explicit &= name "short" &= name "s" &=
                              help "only output space-separated times for clusters"
              , clusterThreshold = 0 &= explicit &= name "cluster-threshold" &= name "t" &=
                                   help "voxel value cut-off threshold"
              , noThinClusters = False &= explicit &= name "no-thin-clusters" &=
                                   help "supress cluster thinning?"
              , graphFile = Nothing &= typFile &= explicit &= name "graph-file" &= name "g" &=
                            help "graph file for cluster algorithms"
              , ignoreLabelFile = Nothing &= typ "FILE" &= explicit &= name "ignore-label" &=
                                    help "mne label file with vertices to ignore"
              , gcStc = "" &= typ "STC" &= argPos 0
              } &= name "clusters"

modifyConf :: Config
modifyConf =
  ModifyMode { method = "maxt" &= typ "NAME" &= help "permutation statistic to use"
             , inputFile = "" &= typ "STCIN" &= argPos 0
             , outputFile = "" &= typ "STCOUT" &= argPos 1
             , clusterThreshold = 0 &= explicit &= name "cluster-threshold" &= name "t" &=
                                  help "voxel value cut-off threshold"
             , noThinClusters = False &= explicit &= name "no-thin-clusters" &=
                                help "supress cluster thinning?"
             , graphFile = Nothing &= typFile &= explicit &= name "graph-file" &= name "g" &=
                           help "graph file for cluster algorithms"
             , ignoreLabelFile = Nothing &= typ "FILE" &= explicit &= name "ignore-label" &=
                                 help "mne label file with vertices to ignore"
             , methodThresh = 0 &= explicit &= name "method-threshold" &= name "m" &=
                              help "threshold for method application"
             } &= name "mod"

confModes :: Config
confModes =
  modes [defConf, getClustersConf, modifyConf] &=
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