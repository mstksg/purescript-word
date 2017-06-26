-- | This module defines bitwise operations for the `BigInt` type.
module Data.BigInt.Bits
    ( and, (.&.)
    , or, (.|.)
    , xor, (.^.)
    , shl
    , shr
    , zshr
    , complement
    ) where

import Data.BigInt as BI
import Data.Int as I
import Data.UInt as U
       
-- | Bitwise AND.
and :: BI.BigInt -> BI.BigInt -> BI.BigInt
and a b = BI.and a b

infixl 10 and as .&.

-- | Bitwise OR.
or :: BI.BigInt -> BI.BigInt -> BI.BigInt
or a b = BI.or a b
   
infixl 10 or as .|.

-- | Bitwise XOR.
xor :: BI.BigInt -> BI.BigInt -> BI.BigInt
xor a b = BI.xor a b
    
infixl 10 xor as .^.

-- | Bitwise shift left.
shl :: BI.BigInt -> U.UInt -> BI.BigInt
shl a s = BI.shl a (I.toNumber (U.toInt s))
    
-- | Bitwise shift right.
shr :: BI.BigInt -> U.UInt -> BI.BigInt
shr a s = BI.shr a (I.toNumber (U.toInt s))
    
-- | Bitwise zero-fill shift right.
zshr :: BI.BigInt -> U.UInt -> BI.BigInt
zshr a s = BI.shr a (I.toNumber (U.toInt s))

-- | Bitwise NOT.
complement :: BI.BigInt -> BI.BigInt
complement a = BI.not a
