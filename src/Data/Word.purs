module Data.Word
       ( Word
       , Word32
       , Word8
       , (.&.)
       , (.|.)
       ) where
       
import Prelude
import Data.String (take)
import Data.BigInt as BI

import Data.Bits (class Bits)
import Data.Shift (class Shift)
import Data.Integral (class Integral)
import Data.UInt (UInt, fromInt, fromNumber, toInt) as U
import Data.UInt.Bits as B

-- Inelegant brute force conversion
showHex :: U.UInt -> String
showHex b | b < (U.fromInt 10) = take 1 $ show b
showHex b | b == (U.fromInt 10) = "A"
showHex b | b == (U.fromInt 11) = "B"
showHex b | b == (U.fromInt 12) = "C"
showHex b | b == (U.fromInt 13) = "D"
showHex b | b == (U.fromInt 14) = "E"
showHex b | b == (U.fromInt 15) = "F"
showHex b = "#" <> show b <> "#"

-- | A generic Word32
newtype Word32 = Word32 U.UInt
type Word = Word32

instance showWord32 :: Show Word32 where
    show (Word32 a) = "Word32 0x" <> showHex (B.and (B.shr a (U.fromInt 28)) (U.fromInt 0xF))
                                  <> showHex (B.and (B.shr a (U.fromInt 24)) (U.fromInt 0xF))  
                                  <> showHex (B.and (B.shr a (U.fromInt 20)) (U.fromInt 0xF))  
                                  <> showHex (B.and (B.shr a (U.fromInt 16)) (U.fromInt 0xF))  
                                  <> showHex (B.and (B.shr a (U.fromInt 12)) (U.fromInt 0xF))  
                                  <> showHex (B.and (B.shr a (U.fromInt 8)) (U.fromInt 0xF))  
                                  <> showHex (B.and (B.shr a (U.fromInt 4)) (U.fromInt 0xF))  
                                  <> showHex (B.and a (U.fromInt 0xF))
                                  <> " (" <> show a <> ")"

instance eqWord32 :: Eq Word32 where
    eq (Word32 a) (Word32 b) = a == b

instance ordWord32 :: Ord Word32 where
    compare (Word32 a) (Word32 b) = compare a b

instance boundedWord32 :: Bounded Word32 where
    bottom = Word32 $ U.fromInt 0
    top = Word32 $ B.complement (U.fromInt 0)

instance semiringWord32 :: Semiring Word32 where
    zero = bottom
    one = Word32 $ U.fromInt 1
    add (Word32 a) (Word32 b) = Word32 (a+b)
    mul (Word32 a) (Word32 b) = Word32 (a*b)

instance word32Integral :: Integral Word32 where
    fromBigInt bi = Word32 $ U.fromNumber <<< BI.toNumber $ bi
    toBigInt (Word32 a) = BI.fromInt <<< U.toInt $ a

instance word32Bits :: Bits Word32 where
    and (Word32 a) (Word32 b) = Word32 (B.and a b)
    or (Word32 a) (Word32 b) = Word32 (B.or a b)
    xor (Word32 a) (Word32 b) = Word32 (B.xor a b)
    complement (Word32 a) = Word32 $ B.complement a
    shl (Word32 a) s = Word32 $ B.shl a s
    shr (Word32 a) s  = Word32 $ B.shr a s
    zshr (Word32 a) s = Word32 $ B.zshr a s

instance heytingAlgebraWord32 :: HeytingAlgebra Word32 where
    ff = Word32 bottom
    tt = Word32 top
    implies (Word32 a) (Word32 b) = Word32 $ B.or (B.complement a) b
    conj (Word32 a) (Word32 b) = Word32 $ B.and a b
    disj (Word32 a) (Word32 b) = Word32 $ B.or a b
    not (Word32 a) = Word32 $ B.complement a

instance booleanAlgebra32 :: BooleanAlgebra Word32

infixl 10 conj as .&.
infixl 10 disj as .|.

instance shift32 :: Shift Word32 where
    shr (Word32 a) s = Word32 $ B.shr a s
    zshr (Word32 a) s = Word32 $ B.zshr a s
    shl (Word32 a) s = Word32 $ B.shl a s
    cshr (Word32 a) s = Word32 $ B.or (B.shr a s) (B.shl a ((U.fromInt 32) - s)) 
    cshl (Word32 a) s = Word32 $ B.or (B.shl a s) (B.shr a ((U.fromInt 32) - s))

-- | A generic Word8
newtype Word8 = Word8 U.UInt

instance showWord8 :: Show Word8 where
    show (Word8 a) = "Word8 0x" <> showHex (B.and (B.shr a (U.fromInt 4)) (U.fromInt 0xF))  
                                <> showHex (B.and a (U.fromInt 0xF))
                                <> " (" <> show a <> ")"

instance eqWord8 :: Eq Word8 where
    eq (Word8 a) (Word8 b) = (B.and a (U.fromInt 0xFF)) == (B.and b (U.fromInt 0xFF))

instance ordWord8 :: Ord Word8 where
    compare (Word8 a) (Word8 b) = compare (B.and a (U.fromInt 0xFF))  (B.and b (U.fromInt 0xFF))

instance boundedWord8 :: Bounded Word8 where
    bottom = Word8 $ U.fromInt 0
    top = Word8 $ B.and (B.complement (U.fromInt 0)) (U.fromInt 0xFF)

instance semiringWord8 :: Semiring Word8 where
    zero = bottom
    one = Word8 $ U.fromInt 1
    add (Word8 a) (Word8 b) = Word8 $ B.and (a+b) (U.fromInt 0xFF)
    mul (Word8 a) (Word8 b) = Word8 $ B.and (a*b) (U.fromInt 0xFF)

instance word8Integral :: Integral Word8 where
    fromBigInt bi = Word8 $ U.fromNumber <<< BI.toNumber $ bi
    toBigInt (Word8 a) = BI.fromInt <<< U.toInt $ a

instance word8Bits :: Bits Word8 where
    and (Word8 a) (Word8 b) = Word8 (B.and a b)
    or (Word8 a) (Word8 b) = Word8 (B.or a b)
    xor (Word8 a) (Word8 b) = Word8 (B.xor a b)
    complement (Word8 a) = Word8 $ B.and (B.complement a) (U.fromInt 0xFF)
    shl (Word8 a) s = Word8 $ B.and (B.shl a s) (U.fromInt 0xFF)
    shr (Word8 a) s  = Word8 $ B.shr a s
    zshr (Word8 a) s = Word8 $ B.zshr a s






--fromInt :: Int -> Word32
--fromInt = Word32 <<< ((add (U.fromInt 0)) :: U.UInt -> U.UInt) <<< U.fromInt

--fromUInt :: U.UInt -> Word32
--fromUInt = Word32

--toInt :: Word32 -> Int
--toInt (Word32 a) = U.toInt a

--toUInt :: Word32 -> U.UInt
--toUInt (Word32 a) = a

