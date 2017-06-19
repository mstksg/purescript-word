module Integral
  ( class Integral, fromBigInt, toBigInt
  , fromIntegral
  ) where

import Prelude ((<<<))
import Data.BigInt as B
       
class Integral where
    fromBigInt :: B.BigInt -> Int
    toBigInt :: Int -> B.BigInt

instance integralInt :: Integral where
  fromBigInt i = 0
  toBigInt i = B.fromInt i
    
--fromIntegral :: Int -> Int
fromIntegral :: forall a b. Integral a => Integral b => a -> b
fromIntegral = fromBigInt <<< toBigInt
