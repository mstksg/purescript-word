-- | This module defines bitwise operations for the `UInt` type.
module Data.UInt.Bits
    ( and, (.&.)
    , or, (.|.)
    , xor, (.^.)
    , shl
    , shr
    , zshr
    , complement
    ) where

import Data.UInt

-- | Bitwise AND.
foreign import and :: UInt -> UInt -> UInt

infixl 10 and as .&.

-- | Bitwise OR.
foreign import or :: UInt -> UInt -> UInt

infixl 10 or as .|.

-- | Bitwise XOR.
foreign import xor :: UInt -> UInt -> UInt

infixl 10 xor as .^.

-- | Bitwise shift left.
foreign import shl :: UInt -> UInt -> UInt

-- | Bitwise shift right.
foreign import shr :: UInt -> UInt -> UInt

-- | Bitwise zero-fill shift right.
foreign import zshr :: UInt -> UInt -> UInt

-- | Bitwise NOT.
foreign import complement :: UInt -> UInt
   
