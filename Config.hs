module Fastpermt.Config (Conf(..)) where

data Conf = Conf { nsubj :: Int -- amount of subjects
                 , sz :: Int -- spatial size of data
                 , tsz :: Int -- temporal size of data
                 , nperm :: Int -- number of permutations
                 }
