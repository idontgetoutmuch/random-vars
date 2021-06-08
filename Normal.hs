{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE BangPatterns               #-}

module Normal (Normal(..)) where

import           RVar
import           PreNormal

instance Distribution Normal Double where
  rvar StdNormal    = stdNormal RGen
  rvar (Normal m s) = normal m s RGen
