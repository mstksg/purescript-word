-- | An `Integral` typeclass that supports integral conversions.
module Data.Integral
  ( class Integral
  , fromIntegral, fromBigInt, toBigInt
  ) where

import Prelude ((<<<), ($))
import Data.BigInt as BI
import Data.Int as I
import Data.Maybe (fromMaybe)

-- | The `Integral` class is used for conversions between integral types.
-- | `BitInt` is used for conversions because it is a type with sign and
-- | and infinite width.
-- |
-- | Laws:
-- | - fromBigInt >>> toBigInt == id
-- | - toBigInt >>> fromBigInt == id
-- |
-- | These laws may fail when converting from larger types to smaller types,
-- | or from signed types to unsigned types. In these cases the behavior
-- | should match intuition, such as preserving leading 1's of an integer
-- | when converting to a Word.
class Integral a where
    fromBigInt :: BI.BigInt -> a
    toBigInt :: a -> BI.BigInt

-- | A helper function for general conversion between `Integral` values.
fromIntegral :: forall a b. Integral a => Integral b => a -> b
fromIntegral = fromBigInt <<< toBigInt

-- | `Integral` instance for `Int`.
instance integralInt :: Integral Int where
    fromBigInt bi = fromMaybe 0 $ (I.fromNumber <<< BI.toNumber) bi
    toBigInt i = BI.fromInt i
