{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleInstances #-}

module Prim where

import Data.Word
import Data.ByteString.Short

import qualified Control.Monad.IO.Class as T
import qualified Data.Functor.Identity as I
import Control.Monad.Prompt (MonadPrompt(..), PromptT, runPromptT, runPromptTM, runPromptTM', runPromptM)
import Control.Monad.Trans.Free.Church (FT, MonadFree (..), hoistFT, iterT, iterTM, liftF,)
import Control.Monad.Trans.Free (foldFreeT, FreeT)
import Control.Monad.Free (foldFree, Free(..))
import Control.Monad.Reader as MTL
import Control.Monad.State as MTL
import qualified Control.Monad.Trans.Class as T

import System.Random.Stateful


data Prim a where
    -- | An unsigned byte, uniformly distributed from 0 to 0xff
    PrimWord8           :: Prim Word8
    -- | An unsigned 16-bit word, uniformly distributed from 0 to 0xffff
    PrimWord16          :: Prim Word16
    -- | An unsigned 32-bit word, uniformly distributed from 0 to 0xffffffff
    PrimWord32          :: Prim Word32
    -- | An unsigned 64-bit word, uniformly distributed from 0 to 0xffffffffffffffff
    PrimWord64          :: Prim Word64
    -- | A uniformly distributed `ShortByteString` of length @n@ bytes
    PrimShortByteString :: !Int -> Prim ShortByteString

instance Show (Prim a) where
    showsPrec _p PrimWord8         = showString "PrimWord8"
    showsPrec _p PrimWord16        = showString "PrimWord16"
    showsPrec _p PrimWord32        = showString "PrimWord32"
    showsPrec _p PrimWord64        = showString "PrimWord64"
    showsPrec  p (PrimShortByteString n) =
      showParen (p > 10) (showString "PrimShortByteString " . showsPrec 11 n)

type RVar = RVarT I.Identity

-- | Sample random variable using `RandomGen` generator as source of entropy
pureRVar :: RandomGen g => RVar a -> g -> (a, g)
pureRVar rvar g = runStateGen g (runRVar rvar)

-- |\"Run\" an 'RVar' - samples the random variable from the provided
-- source of entropy.
runRVar :: StatefulGen g m => RVar a -> g -> m a
runRVar = runRVarTWith (return . I.runIdentity)

-- |@sampleRVar x@ is equivalent to @runRVar x 'StdRandom'@.
sampleReaderRVar :: (StatefulGen g m, MonadReader g m) => RVar a -> m a
sampleReaderRVar = sampleReaderRVarTWith (return . I.runIdentity)

sampleStateRVar :: (RandomGen g, MonadState g m) => RVar a -> m a
sampleStateRVar = sampleStateRVarTWith (return . I.runIdentity)

newtype RVarT m a = RVarT { unRVarT :: PromptT Prim m a }

newtype RVarNewT m a = RVarNewT { unRVarNewT :: FreeT Prim m a }

newtype RVarNewerT m a = RVarAdamNewT { unRVarAdamNewT :: FT Prim m a }

runRVarT :: StatefulGen g m => RVarT m a -> g -> m a
runRVarT = runRVarTWith id

sampleStateRVarT :: (RandomGen g, MonadState g m) => RVarT m a -> m a
sampleStateRVarT rvar = runRVarT rvar StateGenM

sampleReaderRVarT :: (StatefulGen g m, MonadReader g m) => RVarT m a -> m a
sampleReaderRVarT rvar = ask >>= runRVarT rvar

{-# INLINE runRVarTWith #-}
runRVarTWith :: forall m n g a. StatefulGen g m =>
                (forall t. n t -> m t) -> RVarT n a -> g -> m a
runRVarTWith liftN (RVarT m) gen = runPromptT return bindP bindN m
    where
        bindP :: forall t. (Prim t -> (t -> m a) -> m a)
        bindP prim cont = uniformPrimM prim gen >>= cont

        bindN :: forall t. n t -> (t -> m a) -> m a
        bindN nExp cont = liftN nExp >>= cont

-- runRVarNewTWith :: forall t m n g a . ( StatefulGen g m
--                                       , MonadTrans t
--                                       , Monad (t m)
--                                       , Monad n) =>
--                    (forall t . n t -> m t) -> RVarNewT n a -> g -> t m a
-- runRVarNewTWith f (RVarNewT m) gen = foldFreeT baz (undefined :: FreeT n m a)
--   where
--     -- foo :: Prim t0 -> m t0
--     -- foo = \prim -> uniformPrimM prim gen
--     bar :: n a0 -> t m a0
--     bar = lift . f
--     baz :: forall (n0 :: * -> *) x. Monad n => n x -> t n0 x
--     baz x = undefined
--     urk :: Prim x -> t m x
--     urk prim = lift $ uniformPrimM prim gen

{-# INLINE uniformPrimM #-}
uniformPrimM :: StatefulGen g m => Prim t -> g -> m t
uniformPrimM prim g =
    case prim of
        PrimWord8             -> uniformWord8 g
        PrimWord16            -> uniformWord16 g
        PrimWord32            -> uniformWord32 g
        PrimWord64            -> uniformWord64 g
        PrimShortByteString n -> uniformShortByteString n g


-- |@sampleRVarTWith lift x@ is equivalent to @runRVarTWith lift x 'StdRandom'@.
{-# INLINE sampleReaderRVarTWith #-}
sampleReaderRVarTWith ::
       forall m n a g. (StatefulGen g m, MonadReader g m)
    => (forall t. n t -> m t)
    -> RVarT n a
    -> m a
sampleReaderRVarTWith liftN (RVarT m) = runPromptT return bindP bindN m
    where
        bindP :: forall t. (Prim t -> (t -> m a) -> m a)
        bindP prim cont = ask >>= uniformPrimM prim >>= cont

        bindN :: forall t. n t -> (t -> m a) -> m a
        bindN nExp cont = liftN nExp >>= cont


-- |@sampleRVarTWith lift x@ is equivalent to @runRVarTWith lift x 'StdRandom'@.
{-# INLINE sampleStateRVarTWith #-}
sampleStateRVarTWith ::
       forall m n a g. (RandomGen g, MonadState g m)
    => (forall t. n t -> m t)
    -> RVarT n a
    -> m a
sampleStateRVarTWith liftN (RVarT m) = runPromptT return bindP bindN m
    where
        bindP :: forall t. (Prim t -> (t -> m a) -> m a)
        bindP prim cont = uniformPrimM prim StateGenM >>= cont

        bindN :: forall t. n t -> (t -> m a) -> m a
        bindN nExp cont = liftN nExp >>= cont

instance Functor (RVarT n) where
    fmap (f :: a -> b) = (liftM f) :: RVarT n a -> RVarT n b

instance Monad (RVarT n) where
    return x = RVarT (return $! x)
    (RVarT m) >>= k = RVarT (m >>= \x -> x `seq` unRVarT (k x))

instance Applicative (RVarT n) where
    pure  = return
    (<*>) = ap

instance MonadPrompt Prim (RVarT n) where
    prompt = RVarT . prompt

instance T.MonadTrans RVarT where
    lift m = RVarT (MTL.lift m)

instance T.MonadIO m => T.MonadIO (RVarT m) where
    liftIO = T.lift . T.liftIO

data RGen = RGen

instance StatefulGen RGen (RVarT m) where
    uniformWord8 RGen = RVarT $ prompt PrimWord8
    {-# INLINE uniformWord8 #-}
    uniformWord16 RGen = RVarT $ prompt PrimWord16
    {-# INLINE uniformWord16 #-}
    uniformWord32 RGen = RVarT $ prompt PrimWord32
    {-# INLINE uniformWord32 #-}
    uniformWord64 RGen = RVarT $ prompt PrimWord64
    {-# INLINE uniformWord64 #-}
    uniformShortByteString n RGen = RVarT $ prompt (PrimShortByteString n)
    {-# INLINE uniformShortByteString #-}


uniformRVarT :: Uniform a => RVarT m a
uniformRVarT = uniformM RGen
{-# INLINE uniformRVarT #-}

uniformRangeRVarT :: UniformRange a => (a, a) -> RVarT m a
uniformRangeRVarT r = uniformRM r RGen
{-# INLINE uniformRangeRVarT #-}

class Distribution d t where
    -- |Return a random variable with this distribution.
    rvar :: d t -> RVarT m t

