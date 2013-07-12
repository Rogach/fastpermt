module Fastpermt.Stat (ttest) where

ttest :: Floating f => [f] -> [f] -> f
ttest xs ys = let diffs = zipWith (-) xs ys
                  l = fromIntegral $ length xs
                  meanDiff = sum diffs / l
              in sum diffs / sqrt (sum (map ((**2) . (meanDiff-)) diffs) / (l - 1)) / sqrt l
