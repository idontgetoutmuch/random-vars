{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE FunctionalDependencies    #-}
{-# LANGUAGE ImportQualifiedPost       #-}
{-# LANGUAGE TypeOperators             #-}

import Control.Monad
import Data.Functor.Sum
import Data.Word
import System.Random.Stateful
import Control.Monad.Prompt (PromptT, MonadPrompt(..), runPromptTM)


-- N.B. the "T" in `PromptT` suggests this is a transformer but it is
-- actually a sum type (for higher order types).
newtype RVarT m a = RVarT { unRVarT :: PromptT Prim m a }

instance Functor (RVarT n) where
    fmap (f :: a -> b) = (liftM f) :: RVarT n a -> RVarT n b

instance Monad (RVarT n) where
    return x = RVarT (return $! x)
    (RVarT m) >>= k = RVarT (m >>= \x -> x `seq` unRVarT (k x))

instance Applicative (RVarT n) where
    pure  = return
    (<*>) = ap

data Prim a where
  PrimWord8           :: Prim Word8
  PrimWord16          :: Prim Word16
  PrimWord32          :: Prim Word32
  PrimWord64          :: Prim Word64

instance MonadPrompt Prim (RVarT n) where
    prompt = RVarT . prompt

uniformPrimM :: StatefulGen g m => Prim t -> g -> m t
uniformPrimM prim g =
    case prim of
        PrimWord8             -> uniformWord8  g
        PrimWord16            -> uniformWord16 g
        PrimWord32            -> uniformWord32 g
        PrimWord64            -> uniformWord64 g

-- `runPromptTM` is a sum version of the free monad essence: given a
-- natural transformation you get a monad homomorphism
runRVarT :: StatefulGen g m => RVarT m a -> g -> m a
runRVarT (RVarT m) gen = runPromptTM (\p -> uniformPrimM p gen) id m

data RGen = RGen

instance StatefulGen RGen (RVarT m) where
    uniformWord8  RGen = RVarT $ prompt PrimWord8
    uniformWord16 RGen = RVarT $ prompt PrimWord16
    uniformWord32 RGen = RVarT $ prompt PrimWord32
    uniformWord64 RGen = RVarT $ prompt PrimWord64
    uniformShortByteString = error "uniformShortByteString"

uniformRVarT :: Uniform a => RVarT m a
uniformRVarT = uniformM RGen

main :: IO ()
main = do
  let r = runStateGen_ (mkStdGen 43) (runRVarT uniformRVarT) :: Word8
  print r
