{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE GADTs                      #-}

module Main (main) where

import RVar
import Normal

test :: Double
test = sampleSTfixed $
  do x <- interpret (rvar StdNormal)
     y <- interpret (rvar StdNormal)
     return $ x + y

testRvar :: RVarT m [Double]
testRvar = do
  x <- rvar StdNormal
  y <- rvar StdNormal
  return [x, y]

main :: IO ()
main = do
  print test
  print $ sampleSTfixed $ interpret testRvar
  return ()
