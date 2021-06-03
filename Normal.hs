{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE GADTs                      #-}

module Normal (Normal(..)) where

import           RVar

import           Control.Monad.Trans (MonadTrans (..))
import           Control.Monad.Trans.Free.Church (FT, liftF, iterT)

import           Control.Monad.ST (ST, runST)
import           Control.Monad.Trans.Reader (ReaderT, ask, runReaderT)
import qualified Control.Monad.Trans.Class as T
import qualified Control.Monad.IO.Class as T

import           System.Random.Stateful (StatefulGen, StdGen, STGenM
                                        , uniformWord32, uniformShortByteString
                                        , uniformRM, uniformM, uniformDoublePositive01M
                                        , mkStdGen, newSTGenM)

import qualified Data.Vector.Unboxed as I
import           Control.Monad (liftM)
import           Data.Bits ((.&.))

import           Data.Word(Word32)
data T = T {-# UNPACK #-} !Double {-# UNPACK #-} !Double

normal :: StatefulGen g m => Double -> Double -> g -> m Double
normal m s gen = do
  x <- stdNormal gen
  return $! m + s * x

{-# INLINE stdNormal #-}
stdNormal :: StatefulGen g m => g -> m Double
stdNormal gen = loop
  where
    loop = do
      u  <- (subtract 1 . (*2)) `liftM` uniformRM (0.0, 1.0) gen
      ri <- uniformM gen
      let i  = fromIntegral ((ri :: Word32) .&. 127)
          bi = I.unsafeIndex blocks i
          bj = I.unsafeIndex blocks (i+1)
      case () of
        _| abs u < I.unsafeIndex ratios i -> return $! u * bi
         | i == 0                         -> normalTail (u < 0)
         | otherwise                      -> do
             let x  = u * bi
                 xx = x * x
                 d  = exp (-0.5 * (bi * bi - xx))
                 e  = exp (-0.5 * (bj * bj - xx))
             c <- uniformRM (0.0, 1.0) gen
             if e + c * (d - e) < 1
               then return x
               else loop
    normalTail neg  = tailing
      where tailing  = do
              x <- ((/rNorm) . log) `liftM` uniformRM (0.0, 1.0) gen
              y <- log              `liftM` uniformRM (0.0, 1.0) gen
              if y * (-2) < x * x
                then tailing
                else return $! if neg then x - rNorm else rNorm - x

blocks :: I.Vector Double
blocks = (`I.snoc` 0) . I.cons (v/f) . I.cons rNorm . I.unfoldrN 126 go $! T rNorm f
  where
    go (T b g) = let !u = T h (exp (-0.5 * h * h))
                     h  = sqrt (-2 * log (v / b + g))
                 in Just (h, u)
    v = 9.91256303526217e-3
    f = exp (-0.5 * rNorm * rNorm)
{-# NOINLINE blocks #-}

rNorm :: Double
rNorm = 3.442619855899

ratios :: I.Vector Double
ratios = I.zipWith (/) (I.tail blocks) blocks
{-# NOINLINE ratios #-}

data Normal a
    = StdNormal
    | Normal a a

instance Distribution Normal Double where
  rvar StdNormal    = stdNormal RGen
  rvar (Normal m s) = normal m s RGen
