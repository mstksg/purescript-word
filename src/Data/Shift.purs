-- | This module defines bitwise shifting operations typeclass.
module Data.Shift
    (class Shift, shr, oshr, shl, cshr, cshl ) where

-- | Laws:
-- |
-- | - a `shr` 0 == a
-- | - a `oshr` 0 == a
-- | - a `shl` 0 == a
-- | - a `cshr` 0 == a
-- | - a `cshl` 0 == a
-- |
-- | - bottom <= a `shr` n <= top
-- | - bottom <= a `shl` n <= top
-- |
-- | - bottom <= a `cshr` n <= top 
-- | - bottom <= a `cshl` n <= top

-- | Other:
-- | 
-- | n > width of representation should be well behaved by shifting
-- | in one/zero accordingly.
   
import Data.Bounded (class Bounded)
import Data.Ord (class Ord)
import Data.UInt (UInt)
    
class (Bounded a, Ord a) <= Shift a where
    -- | Shift bits right, zero fill
    shr :: a -> UInt -> a

    -- | Shift bits right, one fill
    oshr :: a -> UInt -> a

    -- | Shift bits left, zero fill
    shl :: a -> UInt -> a

    -- | Shift bits right, circular
    cshr :: a -> UInt -> a

    -- | Shift bits left, circular
    cshl :: a -> UInt -> a
