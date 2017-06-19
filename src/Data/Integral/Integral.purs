module Integral
  ( class Integral
  , fromIntegral
  ) where

import Data.BigInt

class Integral where
    fromBigInt :: forall a . BigInt -> a
    toBigInt :: forall a . a -> BigInt

fromIntegral :: forall a b. Integral a => Integral b => a -> b
fromIntegral = fromBigInt . toBigInt
