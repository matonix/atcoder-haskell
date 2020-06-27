{-# LANGUAGE BangPatterns #-}
module Main where

import qualified Data.Vector                   as V
import qualified Data.Vector.Mutable           as VM
import qualified Data.Vector.Unboxed           as VU
import qualified Data.Vector.Unboxed.Mutable   as VUM
import qualified Data.Vector.Generic           as VG
import qualified Data.Vector.Generic.Mutable   as VGM
import qualified Data.ByteString.Char8         as BS
import qualified Data.ByteString.Lazy.Char8    as BSL
import qualified Data.ByteString.Builder       as BSB
import           Data.Char                      ( isSpace )
import           Data.List                      ( unfoldr )
import           Data.Function
import           Data.Complex
import           Control.Monad
import           Control.Monad.Primitive
import           Control.Monad.ST

main :: IO ()
main = do
  n   <- readLn :: IO Int
  abs <-
    VU.replicateM n
    $   (\v -> (v VU.! 0, v VU.! 1))
    .   VU.unfoldr (BS.readInt . BS.dropWhile isSpace)
    <$> BS.getLine
  let v = uncurry multiplyCT $ VU.unzip abs
  print 0
  VU.mapM_ print v

-- O(n^2) multiply polynomials
naiveMultiply :: VU.Vector Int -> VU.Vector Int -> VU.Vector Int
naiveMultiply f g = VU.create $ do
  let len = VU.length f + VU.length g - 1
  h <- VUM.new len
  flip VU.imapM_ f $ \i x ->
    flip VU.imapM_ g $ \j y -> VUM.modify h (\z -> z + x * y) (i + j)
  return h

-- O(n log n) multiply polynomials by FFT （Immutable Vector)
-- 859 ms in atc001-c
-- c.f. https://qiita.com/ageprocpp/items/0d63d4ed80de4a35fe79
multiply :: VU.Vector Int -> VU.Vector Int -> VU.Vector Int
{-# INLINE multiply #-}
multiply !f !g =
  let !len = VU.length f + VU.length g - 1
      !sz  = fix (\rec !k -> if k >= len then k else rec (k * 2)) 1
      !f'  = VU.map fromIntegral f
        VU.++ VU.replicate (sz - VU.length f) (0 :: Complex Double)
      !g' = VU.map fromIntegral g
        VU.++ VU.replicate (sz - VU.length g) (0 :: Complex Double)
  in  VU.map (round . magnitude . (/ fromIntegral sz))
        . VU.take len
        . dft (-1)
        $ VU.zipWith (*) (dft 1 f') (dft 1 g')

-- DFT algorithm
-- if sign == 1 then DFT
-- if sign == -1 then inverse DFT
dft :: Double -> VU.Vector (Complex Double) -> VU.Vector (Complex Double)
{-# INLINE dft #-}
dft !sign !f
  | VU.length f == 1
  = f
  | otherwise
  = let !n    = VU.length f
        !nd2  = n `div` 2
        !f0   = dft sign $ VU.map (f VU.!) $ VU.enumFromStepN 0 2 nd2
        !f1   = dft sign $ VU.map (f VU.!) $ VU.enumFromStepN 1 2 nd2
        -- f0 = dft sign $ VU.ifilter (\i _ -> i `mod` 2 == 0) f
        -- f1 = dft sign $ VU.ifilter (\i _ -> i `mod` 2 == 1) f
        !zeta = cis (sign * 2 * acos (-1) / fromIntegral n) :: Complex Double
    in  VU.imap (\i z -> (f0 VU.! (i `mod` nd2)) + z * (f1 VU.! (i `mod` nd2)))
          $ VU.iterateN n (* zeta) 1

-- O(n log n) multiply polynomials by FFT 
-- c.f. https://qiita.com/ageprocpp/items/0d63d4ed80de4a35fe79
-- multiply :: VU.Vector Int -> VU.Vector Int -> VU.Vector Int
-- multiply f g = runST $ do
--   let len = VU.length f + VU.length g - 1
--   let sz = fix (\rec k -> if k >= len then k else rec (k*2)) 1
--   tf <- VU.thaw (VU.map fromIntegral f :: VU.Vector (Complex Double))
--   nf <- VUM.grow tf (sz - VU.length f)
--   tg <- VU.thaw (VU.map fromIntegral g :: VU.Vector (Complex Double))
--   ng <- VUM.grow tg (sz - VU.length g)
--   dft nf 1 sz
--   dft ng 1 sz
--   forM_ [0..sz-1] $ \i -> do
--     y <- VUM.read ng i
--     VUM.modify nf (* y) i
--   dft nf (-1) sz
--   ret <- VU.freeze nf
--   return $ VU.map ((`div` sz) . round . realPart) $ VU.take len ret

-- DFT algorithm
-- if sign == 1 then DFT
-- if sign == -1 then inverse DFT
-- dft :: PrimMonad m => VUM.MVector (PrimState m) (Complex Double) -> Double -> Int -> m ()
-- {-# INLINE dft #-}
-- dft f sign 1 = return ()
-- dft f sign n = do
--   let nd2 = n `div` 2
--   f0 <- VUM.new nd2
--   f1 <- VUM.new nd2
--   forM_ [0..nd2-1] $ \i -> do
--     x0 <- VUM.read f (2*i)
--     VUM.write f0 i x0
--     x1 <- VUM.read f (2*i+1)
--     VUM.write f1 i x1
--   dft f0 sign nd2
--   dft f1 sign nd2
--   let zeta = cis (sign * 2 * acos (-1) / fromIntegral n)
--   let zetas = VU.iterateN n (*zeta) 1
--   flip VU.imapM_ zetas $ \i z -> do
--     x0 <- VUM.read f0 (i `mod` nd2)
--     x1 <- VUM.read f1 (i `mod` nd2)
--     VUM.write f i (x0 + z * x1)


-- O(n log n) multiply polynomials by Cooley-Tukey FFT （Mutable Vector)
-- 448 ms in atc001-c
-- c.f. http://wwwa.pikara.ne.jp/okojisan/stockham/cooley-tukey.html

-- This function computes polynomial coefficient of (f * g)(x)
-- f & g are polynomial coefficient array of f(x) & g(x)
multiplyCT :: VU.Vector Int -> VU.Vector Int -> VU.Vector Int
multiplyCT f g = runST $ do
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
{-# INLINE multiplyCT #-}

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