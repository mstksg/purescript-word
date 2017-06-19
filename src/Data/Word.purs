module Data.Word
       ( Word
       , Word32
       ) where
       
import Prelude
import Data.String (take)
import Data.BigInt as BI

import Data.Bits (class Bits)
import Data.Integral (class Integral)
import Data.UInt (UInt, fromInt, fromNumber, toInt) as U
import Data.UInt.Bits as B
       
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
        where
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

--fromInt :: Int -> Word32
--fromInt = Word32 <<< ((add (U.fromInt 0)) :: U.UInt -> U.UInt) <<< U.fromInt

--fromUInt :: U.UInt -> Word32
--fromUInt = Word32

--toInt :: Word32 -> Int
--toInt (Word32 a) = U.toInt a

--toUInt :: Word32 -> U.UInt
--toUInt (Word32 a) = a

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
