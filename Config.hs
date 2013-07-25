{-# LANGUAGE DeriveDataTypeable #-}
module Fastpermt.Config (confModes, Config(..), ClusterConf(..)) where

import System.Console.CmdArgs
import Fastpermt.Util

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
            deriving (Show, Data, Typeable)

testRunConf :: Config
testRunConf = TestRun

defConf :: Config
defConf = Conf { method = "maxt" &= typ "NAME" &= help "permutation statistic to use"
               , stcs = [] &= args &= typ "STC"
               , count = 300 &= help "number of permutations"
               , clusterThreshold = 2.1314 &= explicit &= name "cluster-threshold" &=
                                    help "cluster t-statistic value cut-off threshold"
               , graphFile = "" &= typFile &= explicit &= name "graph-file" &=
                             help "graph file for cluster algorithms"
               , thinClusters = False &= explicit &= name "thin-clusters" &=
                                help "apply cluster thinning"
               , nVertsOpt = Nothing &= explicit &= name "nverts"
               , nTimesOpt = Nothing &= explicit &= name "ntimes"
               } &= name "run"

confModes :: Config
confModes =
  modes [defConf, testRunConf] &=
  program "fastpermt" &=
  summary "" &=
  versionArg [ignore]

data ClusterConf = ClusterConf { thresh :: Float
                               , graph :: Graph
                               , nVerts :: Int
                               , nTimes :: Int
                               } deriving Show
