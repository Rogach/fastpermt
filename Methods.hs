module Fastpermt.Methods ( Method(..)
                         , MaxThreshold(..)
                         ) where

import qualified Data.Vector.Unboxed as V

class Method a where
  apply :: (Floating f, Ord f, V.Unbox f) => a -> V.Vector f -> f
  threshold :: (Floating f, Ord f, V.Unbox f) => a -> f -> V.Vector f -> V.Vector f

data MaxThreshold = MaxThreshold deriving (Eq, Show)

instance Method MaxThreshold where
  apply _ values = V.maximum $ V.map abs $ values
  threshold _ th values = V.map (\v -> if (abs v > th) then abs v else 0) values

  --     cutoff = (>(2.1314::Double)) -- p = 0.05
