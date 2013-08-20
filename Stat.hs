{-# LANGUAGE ForeignFunctionInterface #-}
module Fastpermt.Stat (vectorTTest, t2p, p2t, fastTTest) where

import System.IO.Unsafe
import Foreign (withArray, newForeignPtr, finalizerFree, Ptr)
import Foreign.C
import Foreign.ForeignPtr.Unsafe (unsafeForeignPtrToPtr)
import Statistics.Distribution
import Statistics.Distribution.StudentT
import qualified Data.Vector.Storable as V

ttest :: Floating f => [f] -> [f] -> f
ttest xs ys = let l = fromIntegral $ length xs
                  ttest' (x:xs) (y:ys) sum sum2 = ttest' xs ys (sum+(x-y)) (sum2+(x-y)*(x-y))
                  ttest' _ _ sum sum2 = sum / sqrt ((l*sum2 - sum*sum)/(l-1))
               in ttest' xs ys 0 0

vectorTTest :: (Floating f, V.Storable f) => [V.Vector f] -> [V.Vector f] -> V.Vector f
vectorTTest xs ys = let l = V.length $ head xs
                    in V.generate l (\i -> ttest (map (V.! i) xs) (map (V.! i) ys))

t2p :: Int -> CFloat -> CFloat
t2p dg t = realToFrac $ (1 - cumulative (studentT $ fromIntegral dg) (realToFrac t)) * 2

p2t :: Int -> CFloat -> CFloat
p2t dg p = realToFrac $ quantile (studentT $ fromIntegral dg) (1 - realToFrac p / 2)

foreign import ccall "fast_ttest" fast_ttest
  :: Ptr (Ptr CFloat) -> Ptr (Ptr CFloat) -> CInt -> CInt -> IO (Ptr CFloat)

fastTTest :: [V.Vector CFloat] -> [V.Vector CFloat] -> V.Vector CFloat
fastTTest xs ys = unsafePerformIO $ do
  let n = V.length $ head xs
  -- we can safely forget about `unsafeForeignPtrToPtr` here,
  -- because reference to data is kept in main code
  withArray (map (unsafeForeignPtrToPtr . fst . V.unsafeToForeignPtr0) xs) $ \xsp -> do
    withArray (map (unsafeForeignPtrToPtr . fst . V.unsafeToForeignPtr0) ys) $ \ysp -> do
      res <- fast_ttest xsp ysp (fromIntegral n) (fromIntegral (length xs))
      p <- newForeignPtr finalizerFree res
      return $ V.unsafeFromForeignPtr0 p n
