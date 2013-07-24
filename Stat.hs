module Fastpermt.Stat (vectorTTest) where

import qualified Data.Vector.Unboxed as V

ttest :: Floating f => [f] -> [f] -> f
ttest xs ys = let diffs = zipWith (-) xs ys
                  l = fromIntegral $ length xs
                  meanDiff = sum diffs / l
              in sum diffs / sqrt (sum (map ((**2) . (meanDiff-)) diffs) / (l - 1)) / sqrt l

vectorTTest :: (Floating f, V.Unbox f) => [V.Vector f] -> [V.Vector f] -> V.Vector f
vectorTTest xs ys = let l = V.length $ head xs
                    in V.generate l (\i -> ttest (map (\v -> v V.! i) xs) (map (\v -> v V.! i) ys))
