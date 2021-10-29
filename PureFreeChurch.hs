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
import Control.Monad.Free (MonadFree(..) , liftF)
import Control.Monad.Trans.Class qualified as T
import Control.Monad.IO.Class    qualified as T
import Data.Functor.Identity     qualified as T
import Control.Monad.Free.Church (F(..), foldF)


type f :-> g = forall i. f i -> g i

newtype RVarT m a = RVarT { unRVarT :: F (Sum Prim m) a }

instance Functor (RVarT n) where
    fmap = liftM

instance Monad (RVarT n) where
    return x = RVarT (return $! x)
    (RVarT m) >>= k = RVarT (m >>= \x -> x `seq` unRVarT (k x))

instance Applicative (RVarT n) where
    pure  = return
    (<*>) = ap

instance T.MonadTrans RVarT where
  lift = RVarT . wrap . InR . fmap unRVarT . fmap return

instance T.MonadIO m => T.MonadIO (RVarT m) where
    liftIO = T.lift . T.liftIO

data Prim a = Prim (Word32 -> a)

instance Functor Prim where
  fmap f (Prim k) = Prim (f . k)

instance Functor n => MonadFree Prim (RVarT n) where
  wrap = RVarT . wrap . InL . fmap unRVarT

uniformPrimM :: StatefulGen g m => Prim t -> g -> m t
uniformPrimM (Prim f) g = uniformWord32 g >>= return . f

-- A sum version of the free monad essence: given a natural
-- transformation you get a monad homomorphism
runSum :: forall m n p r . (Monad n) => (p :-> n) -> (m :-> n) -> F (Sum p m) r -> n r
runSum prm lft e = foldF alg e
  where
    alg :: Sum p m i -> n i
    alg (InL x) = prm x
    alg (InR y) = lft y

runRVarT :: StatefulGen g m => RVarT m a -> g -> m a
runRVarT (RVarT m) gen = runSum (\p -> uniformPrimM p gen) id m

data RGen = RGen

instance Functor m => StatefulGen RGen (RVarT m) where
    uniformWord32 RGen             = liftF (Prim id)
    uniformShortByteString _n RGen = RVarT $ error "ShortByteString"

uniformRVarT :: (Functor m, Uniform a) => RVarT m a
uniformRVarT = uniformM RGen

main :: IO ()
main = do
  let r = runStateGen_ (mkStdGen 43) (runRVarT uniformRVarT) :: Word8
  print r
