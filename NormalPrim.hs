{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE BangPatterns               #-}

module NormalPrim (Normal(..)) where

import           Prim
import           PreNormal

instance Distribution Normal Double where
  rvar StdNormal    = stdNormal RGen
  rvar (Normal m s) = normal m s RGen
