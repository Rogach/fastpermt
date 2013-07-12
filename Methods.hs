module Fastpermt.Methods ( Method(..)
                         , MaxThreshold(..)
                         ) where

class Method a where
  apply :: (Floating f, Ord f) => a -> [f] -> f
  threshold :: (Floating f, Ord f) => a -> f -> [f] -> [f]

data MaxThreshold = MaxThreshold deriving (Eq, Show)

instance Method MaxThreshold where
  apply _ values = maximum $ map abs $ values
  threshold _ th values = map (\v -> if (abs v > th) then abs v else 0) values
