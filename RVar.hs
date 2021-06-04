{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE RankNTypes                 #-}

module RVar where

import           Control.Monad.Trans (MonadTrans (..))
import           Control.Monad.Trans.Free.Church (FT, liftF, iterT)

import           Control.Monad.ST (ST, runST)
import           Control.Monad.Trans.Reader (ReaderT, ask, runReaderT)

import           System.Random.Stateful (StatefulGen, StdGen, STGenM
                                        , uniformWord32, uniformShortByteString
                                        , uniformDoublePositive01M
                                        , mkStdGen, newSTGenM)

import           Data.Word (Word32)


-- | Random sampling functor.
newtype SamF a = Random (Double -> a)

instance Functor SamF where
  fmap f (Random k) = Random (f . k)

-- | Free monad transformer over random sampling.
--
-- Uses the Church-encoded version of the free monad for efficiency.
newtype RVarT m a = RVarT {_runRVarT :: FT SamF m a}
  deriving newtype (Functor, Applicative, Monad, MonadTrans)

-- | Monads that can draw random variables.
class Monad m => MonadSample m where
  -- | Draw from a uniform distribution.
  random' ::
    -- | \(\sim \mathcal{U}(0, 1)\)
    m Double

-- | An 'ST' based random sampler using the @random@ package.
newtype SamplerST a = SamplerST (forall s . ReaderT (STGenM StdGen s) (ST s) a)

runSamplerST :: SamplerST a -> ReaderT (STGenM StdGen s) (ST s) a
runSamplerST (SamplerST s) = s

instance Functor SamplerST where
  fmap f (SamplerST s) = SamplerST $ fmap f s

instance Applicative SamplerST where
  pure x = SamplerST $ pure x
  (SamplerST f) <*> (SamplerST x) = SamplerST $ f <*> x

instance Monad SamplerST where
  (SamplerST x) >>= f = SamplerST $ x >>= runSamplerST . f

-- | Run the sampler with a fixed random seed.
sampleSTfixed :: SamplerST a -> a
sampleSTfixed (SamplerST s) = runST $ do
  gen <- newSTGenM (mkStdGen 42)
  runReaderT s gen

-- | Convert a distribution supplied by @random@.
fromRandom :: (forall s . (STGenM StdGen s) -> ST s a) -> SamplerST a
fromRandom s = SamplerST $ ask >>= lift . s

instance MonadSample SamplerST where
  random' = fromRandom $ uniformDoublePositive01M

class Distribution d t where
    -- |Return a random variable with this distribution.
    rvar :: d t -> RVarT m t

data RGen = RGen

instance StatefulGen RGen (RVarT m) where
  uniformWord32 RGen            = RVarT $ liftF (Random f)
    where
      f x = floor $ x * fromIntegral (maxBound :: Word32)
  uniformShortByteString _n RGen = RVarT $ error "ShortByteString"

-- | Execute random sampling in the transformed monad.
interpret :: MonadSample m => RVarT m a -> m a
interpret (RVarT m) = iterT f m
  where
    f (Random k) = random' >>= k
