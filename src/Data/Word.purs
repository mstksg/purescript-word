module Data.Word

       ( Word
       , Word64
       , Word32
       , Word16
       , Word8
       , (.&.)
       , (.|.)
       ) where
       
import Prelude
import Data.String (take, drop)
import Data.BigInt as BI
import Data.Int as I
import Data.Maybe (fromMaybe)
       
import Data.Shift (class Shift)
import Data.Integral (class Integral)
import Data.UInt as U
       
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

showBigHex :: BI.BigInt -> String
showBigHex b | b < (BI.fromInt 10) = take 1 $ drop 12 $ show b
showBigHex b | b == (BI.fromInt 10) = "A"
showBigHex b | b == (BI.fromInt 11) = "B"
showBigHex b | b == (BI.fromInt 12) = "C"
showBigHex b | b == (BI.fromInt 13) = "D"
showBigHex b | b == (BI.fromInt 14) = "E"
showBigHex b | b == (BI.fromInt 15) = "F"
showBigHex b = "#" <> show b <> "#"

-- | A default Word
type Word = Word32

infixl 10 conj as .&.
infixl 10 disj as .|.

-- | A generic Word64
newtype Word64 = Word64 BI.BigInt

instance showWord64 :: Show Word64 where
show (Word64 a) = "Word64 0x" <> showBigHex (BI.and (BI.shr a ((I.toNumber 60))) (BI.fromInt 0xF))
                                  <> showBigHex (BI.and (BI.shr a (I.toNumber 56)) (BI.fromInt 0xF))  
                                  <> showBigHex (BI.and (BI.shr a (I.toNumber 52)) (BI.fromInt 0xF))  
                                  <> showBigHex (BI.and (BI.shr a (I.toNumber 48)) (BI.fromInt 0xF))  
                                  <> showBigHex (BI.and (BI.shr a (I.toNumber 44)) (BI.fromInt 0xF))  
                                  <> showBigHex (BI.and (BI.shr a (I.toNumber 40)) (BI.fromInt 0xF))  
                                  <> showBigHex (BI.and (BI.shr a (I.toNumber 36)) (BI.fromInt 0xF))  
                                  <> showBigHex (BI.and (BI.shr a (I.toNumber 32)) (BI.fromInt 0xF))  
                                  <> showBigHex (BI.and (BI.shr a (I.toNumber 28)) (BI.fromInt 0xF))  
                                  <> showBigHex (BI.and (BI.shr a (I.toNumber 24)) (BI.fromInt 0xF))  
                                  <> showBigHex (BI.and (BI.shr a (I.toNumber 20)) (BI.fromInt 0xF))  
                                  <> showBigHex (BI.and (BI.shr a (I.toNumber 16)) (BI.fromInt 0xF))  
                                  <> showBigHex (BI.and (BI.shr a (I.toNumber 12)) (BI.fromInt 0xF))  
                                  <> showBigHex (BI.and (BI.shr a (I.toNumber 8)) (BI.fromInt 0xF))  
                                  <> showBigHex (BI.and (BI.shr a (I.toNumber 4)) (BI.fromInt 0xF))  
                                  <> showBigHex (BI.and a (BI.fromInt 0xF))
                                  <> " (" <> show a <> ")"

instance eqWord64 :: Eq Word64 where
    eq (Word64 a) (Word64 b) = a == b

instance ordWord64 :: Ord Word64 where
    compare (Word64 a) (Word64 b) = compare a b

instance boundedWord64 :: Bounded Word64 where
    bottom = Word64 $ BI.fromInt 0
    top = Word64 $ BI.not (BI.fromInt 0)

instance semiringWord64 :: Semiring Word64 where
    zero = bottom
    one = Word64 $ BI.fromInt 1
    add (Word64 a) (Word64 b) = Word64 (a+b)
    mul (Word64 a) (Word64 b) = Word64 (a*b)

instance ring64 :: Ring Word64 where
    sub (Word64 a) (Word64 b) = Word64 (a-b)
    
instance word64Integral :: Integral Word64 where
    fromBigInt bi = Word64 bi
    toBigInt (Word64 a) = a

instance heytingAlgebraWord64 :: HeytingAlgebra Word64 where
    ff = bottom
    tt = top
    implies (Word64 a) (Word64 b) = Word64 $ BI.or (BI.not a) b
    conj (Word64 a) (Word64 b) = Word64 $ BI.and a b
    disj (Word64 a) (Word64 b) = Word64 $ BI.or a b
    not (Word64 a) = Word64 $ BI.not a

instance booleanAlgebra64 :: BooleanAlgebra Word64

instance shift64 :: Shift Word64 where
    shr (Word64 a) s = Word64 $ if (BI.and a (fromMaybe (BI.fromInt 0) (BI.fromString "92e17")) > (BI.fromInt 0))
        then if s >= (U.fromInt 64)
            then (BI.not (BI.fromInt 0))
            else BI.or (BI.shr a (I.toNumber (U.toInt s))) ((BI.not (BI.fromInt 0)) - ((BI.shl (BI.fromInt 1) ((I.toNumber 64) - (I.toNumber (U.toInt s)))) - (BI.fromInt 1)))
        else BI.shr a (I.toNumber (U.toInt s))
    zshr (Word64 a) s = Word64 $ if s >= (U.fromInt 64)
                                 then (BI.fromInt 0)
                                 else BI.and (BI.shr a (I.toNumber (U.toInt s))) ((BI.shl (BI.fromInt 1) ((I.toNumber 64) - (I.toNumber (U.toInt s)))) - (BI.fromInt 1))
    shl (Word64 a) s = Word64 $ BI.shl a (I.toNumber (U.toInt s))
    cshr (Word64 a) s = Word64 $ BI.or (BI.shr a (I.toNumber (U.toInt s))) (BI.shl a ((I.toNumber 64) - (I.toNumber (U.toInt s)))) 
    cshl (Word64 a) s = Word64 $ BI.or (BI.shl a (I.toNumber (U.toInt s))) (BI.shr a ((I.toNumber 64) - (I.toNumber (U.toInt s))))

-- | A generic Word32
newtype Word32 = Word32 U.UInt

instance showWord32 :: Show Word32 where
    show (Word32 a) = "Word32 0x" <> showHex (U.and (U.shr a (U.fromInt 28)) (U.fromInt 0xF))
                                  <> showHex (U.and (U.shr a (U.fromInt 24)) (U.fromInt 0xF))  
                                  <> showHex (U.and (U.shr a (U.fromInt 20)) (U.fromInt 0xF))  
                                  <> showHex (U.and (U.shr a (U.fromInt 16)) (U.fromInt 0xF))  
                                  <> showHex (U.and (U.shr a (U.fromInt 12)) (U.fromInt 0xF))  
                                  <> showHex (U.and (U.shr a (U.fromInt 8)) (U.fromInt 0xF))  
                                  <> showHex (U.and (U.shr a (U.fromInt 4)) (U.fromInt 0xF))  
                                  <> showHex (U.and a (U.fromInt 0xF))
                                  <> " (" <> show a <> ")"

instance eqWord32 :: Eq Word32 where
    eq (Word32 a) (Word32 b) = a == b

instance ordWord32 :: Ord Word32 where
    compare (Word32 a) (Word32 b) = compare a b

instance boundedWord32 :: Bounded Word32 where
    bottom = Word32 $ U.fromInt 0
    top = Word32 $ U.complement (U.fromInt 0)

instance semiringWord32 :: Semiring Word32 where
    zero = bottom
    one = Word32 $ U.fromInt 1
    add (Word32 a) (Word32 b) = Word32 (a+b)
    mul (Word32 a) (Word32 b) = Word32 (a*b)

instance ring32 :: Ring Word32 where
    sub (Word32 a) (Word32 b) = Word32 (a-b)
    
instance word32Integral :: Integral Word32 where
    fromBigInt bi = Word32 $ U.fromNumber <<< BI.toNumber $ bi
    toBigInt (Word32 a) = BI.fromInt <<< U.toInt $ a

instance heytingAlgebraWord32 :: HeytingAlgebra Word32 where
    ff = Word32 bottom
    tt = Word32 top
    implies (Word32 a) (Word32 b) = Word32 $ U.or (U.complement a) b
    conj (Word32 a) (Word32 b) = Word32 $ U.and a b
    disj (Word32 a) (Word32 b) = Word32 $ U.or a b
    not (Word32 a) = Word32 $ U.complement a

instance booleanAlgebra32 :: BooleanAlgebra Word32

instance shift32 :: Shift Word32 where
    shr (Word32 a) s = Word32 $ if (U.and a (U.fromInt 0x8000000) > (U.fromInt 0))
        then if s >= (U.fromInt 32)
            then (U.complement (U.fromInt 0))
            else U.or (U.shr a s) ((U.complement (U.fromInt 0)) - ((U.shl (U.fromInt 1) ((U.fromInt 32) - s)) - (U.fromInt 1)))
        else U.shr a s
    zshr (Word32 a) s = Word32 $ if s >= (U.fromInt 32) then (U.fromInt 0) else U.zshr a s
    shl (Word32 a) s = Word32 $ U.shl a s
    cshr (Word32 a) s = Word32 $ U.or (U.shr a s) (U.shl a ((U.fromInt 32) - s)) 
    cshl (Word32 a) s = Word32 $ U.or (U.shl a s) (U.shr a ((U.fromInt 32) - s))

    -- | A generic Word16
newtype Word16 = Word16 U.UInt

instance showWord16 :: Show Word16 where
    show (Word16 a) = "Word16 0x"
                                  <> showHex (U.and (U.shr a (U.fromInt 12)) (U.fromInt 0xF))  
                                  <> showHex (U.and (U.shr a (U.fromInt 8))  (U.fromInt 0xF))  
                                  <> showHex (U.and (U.shr a (U.fromInt 4))  (U.fromInt 0xF))  
                                  <> showHex (U.and a (U.fromInt 0xF))
                                  <> " (" <> show a <> ")"

instance eqWord16 :: Eq Word16 where
    eq (Word16 a) (Word16 b) = (U.and a (U.fromInt 0xFFFF)) == (U.and b (U.fromInt 0xFFFF))

instance ordWord16 :: Ord Word16 where
    compare (Word16 a) (Word16 b) = compare (U.and a (U.fromInt 0xFFFF))  (U.and b (U.fromInt 0xFFFF))

instance boundedWord16 :: Bounded Word16 where
    bottom = Word16 $ U.fromInt 0
    top = Word16 $ U.and (U.complement (U.fromInt 0)) (U.fromInt 0xFFFF)

instance semiringWord16 :: Semiring Word16 where
    zero = bottom
    one = Word16 $ U.fromInt 1
    add (Word16 a) (Word16 b) = Word16 $ U.and (a+b) (U.fromInt 0xFFFF)
    mul (Word16 a) (Word16 b) = Word16 $ U.and (a*b) (U.fromInt 0xFFFF)

instance ring16 :: Ring Word16 where
    sub (Word16 a) (Word16 b) = Word16 (a-b)

instance word16Integral :: Integral Word16 where
    fromBigInt bi = Word16 $ U.fromNumber <<< BI.toNumber $ bi
    toBigInt (Word16 a) = BI.fromInt <<< U.toInt $ a

instance heytingAlgebraWord16 :: HeytingAlgebra Word16 where
    ff = Word16 bottom
    tt = Word16 top
    implies (Word16 a) (Word16 b) = Word16 $ U.or (U.complement a) b
    conj (Word16 a) (Word16 b) = Word16 $ U.and a b
    disj (Word16 a) (Word16 b) = Word16 $ U.or a b
    not (Word16 a) = Word16 $ U.complement a

instance booleanAlgebra16 :: BooleanAlgebra Word16

--infixl 10 conj as .&.
--infixl 10 disj as .|.

instance shift16 :: Shift Word16 where
    shr (Word16 a) s = Word16 $ if (U.and a (U.fromInt 0x8000) > (U.fromInt 0))
        then if s >= (U.fromInt 16)
            then (U.fromInt 0xFFFF)
            else U.or (U.shr a s) ((U.fromInt 0xFFFF) - ((U.shl (U.fromInt 1) ((U.fromInt 16) - s)) - (U.fromInt 1)))
        else U.shr a s
    zshr (Word16 a) s = Word16 $ U.zshr a s
    shl (Word16 a) s = Word16 $ U.shl a s
    cshr (Word16 a) s = Word16 $ U.or (U.shr a s) (U.shl a ((U.fromInt 16) - s)) 
    cshl (Word16 a) s = Word16 $ U.or (U.shl a s) (U.shr a ((U.fromInt 16) - s))

-- | A generic Word8
newtype Word8 = Word8 U.UInt

instance showWord8 :: Show Word8 where
    show (Word8 a) = "Word8 0x" <> showHex (U.and (U.shr a (U.fromInt 4)) (U.fromInt 0xF))  
                                <> showHex (U.and a (U.fromInt 0xF))
                                <> " (" <> show a <> ")"

instance eqWord8 :: Eq Word8 where
    eq (Word8 a) (Word8 b) = (U.and a (U.fromInt 0xFF)) == (U.and b (U.fromInt 0xFF))

instance ordWord8 :: Ord Word8 where
    compare (Word8 a) (Word8 b) = compare (U.and a (U.fromInt 0xFF))  (U.and b (U.fromInt 0xFF))

instance boundedWord8 :: Bounded Word8 where
    bottom = Word8 $ U.fromInt 0
    top = Word8 $ U.and (U.complement (U.fromInt 0)) (U.fromInt 0xFF)

instance semiringWord8 :: Semiring Word8 where
    zero = bottom
    one = Word8 $ U.fromInt 1
    add (Word8 a) (Word8 b) = Word8 $ U.and (a+b) (U.fromInt 0xFF)
    mul (Word8 a) (Word8 b) = Word8 $ U.and (a*b) (U.fromInt 0xFF)

instance ring8 :: Ring Word8 where
    sub (Word8 a) (Word8 b) = Word8 (a-b)

instance word8Integral :: Integral Word8 where
    fromBigInt bi = Word8 $ U.fromNumber <<< BI.toNumber $ bi
    toBigInt (Word8 a) = BI.fromInt <<< U.toInt $ a

instance heytingAlgebraWord8 :: HeytingAlgebra Word8 where
    ff = Word8 bottom
    tt = Word8 top
    implies (Word8 a) (Word8 b) = Word8 $ U.or (U.complement a) b
    conj (Word8 a) (Word8 b) = Word8 $ U.and a b
    disj (Word8 a) (Word8 b) = Word8 $ U.or a b
    not (Word8 a) = Word8 $ U.complement a

instance booleanAlgebra8 :: BooleanAlgebra Word8

--infixl 10 conj as .&.
--infixl 10 disj as .|.

instance shift8 :: Shift Word8 where
    shr (Word8 a) s = Word8 $ if (U.and a (U.fromInt 0x80) > (U.fromInt 0))
        then if s >= (U.fromInt 8)
            then (U.fromInt 0xFF)
            else U.or (U.shr a s) ((U.fromInt 0xFF) - ((U.shl (U.fromInt 1) ((U.fromInt 8) - s)) - (U.fromInt 1)))
        else U.shr a s
    zshr (Word8 a) s = Word8 $ U.zshr a s
    shl (Word8 a) s = Word8 $ U.shl a s
    cshr (Word8 a) s = Word8 $ U.or (U.shr a s) (U.shl a ((U.fromInt 8) - s)) 
    cshl (Word8 a) s = Word8 $ U.or (U.shl a s) (U.shr a ((U.fromInt 8) - s))
