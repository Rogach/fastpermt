module Fastpermt.Stat (vectorTTest, t2p, p2t) where

import GHC.Float
import Statistics.Distribution
import Statistics.Distribution.StudentT
import qualified Data.Vector.Unboxed as V

ttest :: Floating f => [f] -> [f] -> f
ttest xs ys = let diffs = zipWith (-) xs ys
                  l = fromIntegral $ length xs
                  meanDiff = sum diffs / l
              in sum diffs / sqrt (sum (map ((**2) . (meanDiff-)) diffs) / (l - 1)) / sqrt l

vectorTTest :: (Floating f, V.Unbox f) => [V.Vector f] -> [V.Vector f] -> V.Vector f
vectorTTest xs ys = let l = V.length $ head xs
                    in V.generate l (\i -> ttest (map (\v -> v V.! i) xs) (map (\v -> v V.! i) ys))

t2p :: Int -> Float -> Float
t2p dg t = double2Float $ (1 - cumulative (studentT $ fromIntegral dg) (float2Double t)) * 2

p2t :: Int -> Float -> Float
p2t dg p = double2Float $ quantile (studentT $ fromIntegral dg) (1 - (float2Double p) / 2)