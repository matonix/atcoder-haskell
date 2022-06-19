-- Note: if you use this for same array (e.g. multiply xs xs),
-- consider "cumulative sum" algorithm.

-- O(n log n) multiply polynomials by Cooley-Tukey FFT （Mutable Vector)
-- 448 ms in atc001-c
-- cf. http://wwwa.pikara.ne.jp/okojisan/stockham/cooley-tukey.html
module Math.Algorithms.FFT where

import qualified Data.Vector.Unboxed           as VU
import qualified Data.Vector.Unboxed.Mutable   as VUM
import           Data.Complex
import           Data.Function
import           Control.Monad
import           Control.Monad.Primitive
import           Control.Monad.ST

-- This function computes polynomial coefficient of (f * g)(x)
-- f & g are polynomial coefficient array of f(x) & g(x)
multiply :: VU.Vector Int -> VU.Vector Int -> VU.Vector Int
multiply f g = runST $ do
  let len = VU.length f + VU.length g - 1
  let sz  = fix (\rec k -> if k >= len then k else rec (k * 2)) 1
  tf <- VU.thaw (VU.map fromIntegral f :: VU.Vector (Complex Double))
  nf <- VUM.grow tf (sz - VU.length f)
  tg <- VU.thaw (VU.map fromIntegral g :: VU.Vector (Complex Double))
  ng <- VUM.grow tg (sz - VU.length g)
  fft sz nf
  fft sz ng
  forM_ [0 .. sz - 1] $ \i -> do
    y <- VUM.read ng i
    VUM.modify nf (* y) i
  ifft sz nf
  ret <- VU.freeze nf
  return $ VU.map (round . realPart) $ VU.take len ret
{-# INLINE multiply #-}

fft
  :: PrimMonad m
  => Int -- length
  -> VUM.MVector (PrimState m) (Complex Double) -- target data of FFT
  -> m ()
fft n = cooleyTukey n 1 0 0
{-# INLINE fft #-}

ifft
  :: PrimMonad m
  => Int -- length
  -> VUM.MVector (PrimState m) (Complex Double) -- target data of FFT
  -> m ()
ifft n x = do
  forM_ [0 .. n - 1] $ VUM.modify x conjugate
  cooleyTukey n 1 0 0 x
  forM_ [0 .. n - 1] $ VUM.modify x ((/ fromIntegral n) . conjugate)
{-# INLINE ifft #-}

cooleyTukey
  :: PrimMonad m
  => Int -- length
  -> Int -- stride
  -> Int -- start position of block
  -> Int -- temp var for bit reverse position in start position of block
  -> VUM.MVector (PrimState m) (Complex Double) -- target data of FFT
  -> m ()
cooleyTukey n s q d x
  | n > 1 = do
    let m  = n `div` 2
    let θ0 = 2 * pi / fromIntegral n
    forM_ [0 .. m - 1] $ \p -> do
      let wp = cos (fromIntegral p * θ0) :+ (-sin (fromIntegral p * θ0))
      a <- VUM.read x (q + p + 0)
      b <- VUM.read x (q + p + m)
      VUM.write x (q + p + 0) (a + b)
      VUM.write x (q + p + m) ((a - b) * wp)
    cooleyTukey m (2 * s) (q + 0) (d + 0) x
    cooleyTukey m (2 * s) (q + m) (d + s) x
  | q > d = VUM.swap x q d
  | otherwise = return ()
{-# INLINE cooleyTukey #-}