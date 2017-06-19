-- | This module defines bitwise operations typeclas.
module Data.Bits
    ( class Bits, and, or, xor, shl, shr, zshr, complement, (.&.), (.|.), (.^.)
    ) where

import Data.UInt
    
class Bits a where
    -- | Bitwise AND.
    and :: a -> a -> a

    -- | Bitwise OR.
    or :: a -> a -> a

    -- | Bitwise XOR.
    xor :: a -> a -> a

    -- | Bitwise shift left.
    shl :: a -> UInt -> a

    -- | Bitwise shift right.
    shr :: a -> UInt -> a

    -- | Bitwise zero-fill shift right.
    zshr :: a -> UInt -> a

    -- | Bitwise NOT.
    complement :: a -> a
   
infixl 10 and as .&.
infixl 10 or as .|.
infixl 10 xor as .^.
