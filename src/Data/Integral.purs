module Data.Integral
  ( class Integral
  , fromIntegral, fromBigInt, toBigInt
  ) where

import Prelude ((<<<), ($))
import Data.BigInt as BI
import Data.Int as I
import Data.Maybe (fromMaybe)

-- | Law: fromBigInt >>> toBigInt == id
--        toBigInt >>> fromBigInt == id
-- Both laws fail if the sizes are not equal
class Integral a where
    fromBigInt :: BI.BigInt -> a
    toBigInt :: a -> BI.BigInt

fromIntegral :: forall a b. Integral a => Integral b => a -> b
fromIntegral = fromBigInt <<< toBigInt

instance integralInt :: Integral Int where
    fromBigInt bi = fromMaybe 0 $ (I.fromNumber <<< BI.toNumber) bi
    toBigInt i = BI.fromInt i
