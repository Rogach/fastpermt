module Fastpermt.Labels(MneLabel(..), readLabel, applyIgnoreLabel) where

import qualified Data.Vector.Storable as V
import Fastpermt.Config (emptyCC, nVerts, nTimes)
import Fastpermt.Util
import Fastpermt.Stc
import Data.List

data MneLabel = MneLabel [Int]

readLabel :: FilePath -> IO MneLabel
readLabel = fmap (MneLabel . map (fst . head . reads) . drop 2 . lines) . readFile

applyIgnoreLabel :: MneLabel -> Stc -> Stc
applyIgnoreLabel (MneLabel lbl) stc@Stc { n_vertices = nv
                                        , n_times = nt
                                        , stc_data = dt
                                        , stc_vertices = sourceVector
                                        } =
  let op = (V.// zip (intersect' (sort lbl) (sort sourceVector)) (repeat 0))
  in stc { stc_data = V.concat $ onVertices (emptyCC { nVerts = nv, nTimes = nt }) op dt }
  where intersect' [] _ = []
        intersect' _ [] = []
        intersect' (a:as) (b:bs)
          | a == b = a : intersect' as bs
          | a > b = intersect' (a:as) bs
          | otherwise = intersect' as (b:bs)
