module Integral
  ( class Integral
  , fromIntegral, fromBigInt, toBigInt
  ) where

import Prelude ((<<<))
import Data.BigInt as B
       
class Integral a where
    fromBigInt :: B.BigInt -> a
    toBigInt :: a -> B.BigInt

--foreign import data BigInt :: Type
    
fromIntegral :: forall a b. Integral a => Integral b => a -> b
fromIntegral = fromBigInt <<< toBigInt
