module Data.Word
       ( Word
       , fromInt, fromUInt, toInt, toUInt
       , and, (.&.)
       , or, (.|.)
       , xor, (.^.)
       , complement
       , shl, shr, zshr) where
       
import Prelude
import Data.String (take)
import Data.UInt (UInt, fromInt, toInt) as U
import Data.UInt.Bits as B

-- | A generic Word
newtype Word = Word U.UInt

instance showWord :: Show Word where
    show (Word a) = "Word 0x" <> showHex (B.and (B.shr a (U.fromInt 28)) (U.fromInt 0xF))
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

instance eqWord :: Eq Word where
    eq (Word a) (Word b) = a == b

instance ordWord :: Ord Word where
    compare (Word a) (Word b) = compare a b

instance boundedWord :: Bounded Word where
    bottom = Word $ U.fromInt 0
    top = Word $ B.complement (U.fromInt 0)

instance semiringWord :: Semiring Word where
    zero = bottom
    one = Word $ U.fromInt 1
    add (Word a) (Word b) = Word (a+b)
    mul (Word a) (Word b) = Word (a*b)

fromInt :: Int -> Word
fromInt = Word <<< ((add (U.fromInt 0)) :: U.UInt -> U.UInt) <<< U.fromInt

fromUInt :: U.UInt -> Word
fromUInt = Word

toInt :: Word -> Int
toInt (Word a) = U.toInt a

toUInt :: Word -> U.UInt
toUInt (Word a) = a
        
-- | Bitwise AND.
and :: Word -> Word -> Word
and (Word a) (Word b) = Word (B.and a b)

infixl 10 and as .&.

-- | Bitwise OR.
or :: Word -> Word -> Word
or (Word a) (Word b) = Word (B.or a b)

infixl 10 or as .|.

-- | Bitwise XOR.
xor :: Word -> Word -> Word
xor (Word a) (Word b) = Word (B.xor a b)

infixl 10 xor as .^.

-- | Bitwise ~.
complement :: Word -> Word
complement (Word a) = Word $ B.complement a

-- | Shift left
shl :: Word -> Word -> Word
shl (Word a) (Word s) = Word $ B.shl a s

-- | Shift Right
shr :: Word -> Word -> Word
shr (Word a) (Word s)  = Word $ B.shr a s

-- | Zero shift Right
zshr :: Word -> Word -> Word
zshr (Word a) (Word s) = Word $ B.zshr a s
