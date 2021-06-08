module MainPrim (main) where

import Prim
import NormalPrim
import Control.Monad.Reader (runReaderT)
import qualified Data.Vector.Storable as VS
import Control.Monad.Reader.Class (MonadReader)
import System.Random.Stateful
import Control.Monad.ST (ST, runST)

import System.Random.MWC (create, initialize)


-- test :: Double
-- test = sampleSTfixed $
--   do x <- interpret (rvar StdNormal)
--      y <- interpret (rvar StdNormal)
--      return $ x + y

-- testRvar :: (StatefulGen g m, MonadReader g m) => m [Double]
-- testRvar = do
--   x <- sample StdNormal
--   y <- sample StdNormal
--   return [x, y]

testRvar :: RVarT m [Double]
testRvar = do
  x <- rvar StdNormal
  y <- rvar StdNormal
  return [x, y]

-- foo  :: (StatefulGen g m, MonadReader g m) => m [Double]
-- foo = sampleReaderRVarT testRvar

foo :: IO [Double]
foo = initialize (VS.singleton 2021) >>= runReaderT (sampleReaderRVar testRvar)

main :: IO ()
main = return ()
-- main = do
--   print test
--   print $ sampleSTfixed $ interpret testRvar
--   return ()
