-- | A `Word` typeclass that supports fixed width values up to 64 bits in multiples of 8.
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
       
-- | Inelegant brute force conversions
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

-- | A default `Word`
type Word = Word32

-- | Conjunction
infixl 10 conj as .&.

-- | Disjunction
infixl 10 disj as .|.

-- | A generic `Word64`
newtype Word64 = Word64 BI.BigInt

-- | Instance of `Show` for `Word64` that displays the internal value in hex.
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

-- | Instance of `Eq` for `Word64` for comparing values.
instance eqWord64 :: Eq Word64 where
    eq (Word64 a) (Word64 b) = a == b

-- | Instance of `Ord` for `Word64` for ordering values.
instance ordWord64 :: Ord Word64 where
    compare (Word64 a) (Word64 b) = compare a b

-- | Instance of `Bounded` for `Word64` for bounding values to 64 bits.
instance boundedWord64 :: Bounded Word64 where
    bottom = Word64 $ BI.fromInt 0
    top = Word64 $ BI.not (BI.fromInt 0)

-- | Instance of `Semiring` for `Word64` for addition and multiplication.
instance semiringWord64 :: Semiring Word64 where
    zero = bottom
    one = Word64 $ BI.fromInt 1
    add (Word64 a) (Word64 b) = Word64 (a+b)
    mul (Word64 a) (Word64 b) = Word64 (a*b)

-- | Instance of `Ring` for `Word64` for subtraction.
instance ring64 :: Ring Word64 where
    sub (Word64 a) (Word64 b) = Word64 (a-b)
    
-- | Instance of `Integral` for `Word64` for conversions between numbers.
instance word64Integral :: Integral Word64 where
    fromBigInt bi = Word64 bi
    toBigInt (Word64 a) = a

-- | Instance of `HeytingAlgebra` for `Word64` for bitwise logical operations.
instance heytingAlgebraWord64 :: HeytingAlgebra Word64 where
    ff = bottom
    tt = top
    implies (Word64 a) (Word64 b) = Word64 $ BI.or (BI.not a) b
    conj (Word64 a) (Word64 b) = Word64 $ BI.and a b
    disj (Word64 a) (Word64 b) = Word64 $ BI.or a b
    not (Word64 a) = Word64 $ BI.not a

-- | Instance of `BooleanAlgebra` for `Word64`.
instance booleanAlgebra64 :: BooleanAlgebra Word64

-- | Instance of `Shift` for `Word64` for shifting values.
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

-- | Instance of `Show` for `Word32` that displays the internal value in hex.
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

-- | Instance of `Eq` for `Word32` for comparing values.
instance eqWord32 :: Eq Word32 where
    eq (Word32 a) (Word32 b) = a == b

-- | Instance of `Ord` for `Word32` for ordering values.
instance ordWord32 :: Ord Word32 where
    compare (Word32 a) (Word32 b) = compare a b

-- | Instance of `Bounded` for `Word32` for bounding values to 32 bits.
instance boundedWord32 :: Bounded Word32 where
    bottom = Word32 $ U.fromInt 0
    top = Word32 $ U.complement (U.fromInt 0)

-- | Instance of `Semiring` for `Word32` for addition and multiplication.
instance semiringWord32 :: Semiring Word32 where
    zero = bottom
    one = Word32 $ U.fromInt 1
    add (Word32 a) (Word32 b) = Word32 (a+b)
    mul (Word32 a) (Word32 b) = Word32 (a*b)

-- | Instance of `Ring` for `Word32` for subtraction.
instance ring32 :: Ring Word32 where
    sub (Word32 a) (Word32 b) = Word32 (a-b)
    
-- | Instance of `Integral` for `Word32` for conversions between numbers.
instance word32Integral :: Integral Word32 where
    fromBigInt bi = Word32 $ U.fromNumber <<< BI.toNumber $ bi
    toBigInt (Word32 a) = BI.fromInt <<< U.toInt $ a

-- | Instance of `HeytingAlgebra` for `Word32` for bitwise logical operations.
instance heytingAlgebraWord32 :: HeytingAlgebra Word32 where
    ff = Word32 bottom
    tt = Word32 top
    implies (Word32 a) (Word32 b) = Word32 $ U.or (U.complement a) b
    conj (Word32 a) (Word32 b) = Word32 $ U.and a b
    disj (Word32 a) (Word32 b) = Word32 $ U.or a b
    not (Word32 a) = Word32 $ U.complement a

-- | Instance of `BooleanAlgebra` for `Word32`.
instance booleanAlgebra32 :: BooleanAlgebra Word32

-- | Instance of `Shift` for `Word32` for shifting values.
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

-- | Instance of `Show` for `Word16` that displays the internal value in hex.
instance showWord16 :: Show Word16 where
    show (Word16 a) = "Word16 0x"
                                  <> showHex (U.and (U.shr a (U.fromInt 12)) (U.fromInt 0xF))  
                                  <> showHex (U.and (U.shr a (U.fromInt 8))  (U.fromInt 0xF))  
                                  <> showHex (U.and (U.shr a (U.fromInt 4))  (U.fromInt 0xF))  
                                  <> showHex (U.and a (U.fromInt 0xF))
                                  <> " (" <> show a <> ")"

-- | Instance of `Eq` for `Word16` for comparing values.
instance eqWord16 :: Eq Word16 where
    eq (Word16 a) (Word16 b) = (U.and a (U.fromInt 0xFFFF)) == (U.and b (U.fromInt 0xFFFF))

-- | Instance of `Ord` for `Word16` for ordering values.
instance ordWord16 :: Ord Word16 where
    compare (Word16 a) (Word16 b) = compare (U.and a (U.fromInt 0xFFFF))  (U.and b (U.fromInt 0xFFFF))

-- | Instance of `Bounded` for `Word16` for bounding values to 16 bits.
instance boundedWord16 :: Bounded Word16 where
    bottom = Word16 $ U.fromInt 0
    top = Word16 $ U.and (U.complement (U.fromInt 0)) (U.fromInt 0xFFFF)

-- | Instance of `Semiring` for `Word16` for addition and multiplication.
instance semiringWord16 :: Semiring Word16 where
    zero = bottom
    one = Word16 $ U.fromInt 1
    add (Word16 a) (Word16 b) = Word16 $ U.and (a+b) (U.fromInt 0xFFFF)
    mul (Word16 a) (Word16 b) = Word16 $ U.and (a*b) (U.fromInt 0xFFFF)

-- | Instance of `Ring` for `Word16` for subtraction.
instance ring16 :: Ring Word16 where
    sub (Word16 a) (Word16 b) = Word16 (a-b)

-- | Instance of `Integral` for `Word16` for conversions between numbers.
instance word16Integral :: Integral Word16 where
    fromBigInt bi = Word16 $ U.fromNumber <<< BI.toNumber $ bi
    toBigInt (Word16 a) = BI.fromInt <<< U.toInt $ a

-- | Instance of `HeytingAlgebra` for `Word16` for bitwise logical operations.
instance heytingAlgebraWord16 :: HeytingAlgebra Word16 where
    ff = Word16 bottom
    tt = Word16 top
    implies (Word16 a) (Word16 b) = Word16 $ U.or (U.complement a) b
    conj (Word16 a) (Word16 b) = Word16 $ U.and a b
    disj (Word16 a) (Word16 b) = Word16 $ U.or a b
    not (Word16 a) = Word16 $ U.complement a

-- | Instance of `BooleanAlgebra` for `Word16`.
instance booleanAlgebra16 :: BooleanAlgebra Word16

-- | Instance of `Shift` for `Word16` for shifting values.
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

-- | Instance of `Show` for `Word8` that displays the internal value in hex.
instance showWord8 :: Show Word8 where
    show (Word8 a) = "Word8 0x" <> showHex (U.and (U.shr a (U.fromInt 4)) (U.fromInt 0xF))  
                                <> showHex (U.and a (U.fromInt 0xF))
                                <> " (" <> show a <> ")"

-- | Instance of `Eq` for `Word8` for comparing values.
instance eqWord8 :: Eq Word8 where
    eq (Word8 a) (Word8 b) = (U.and a (U.fromInt 0xFF)) == (U.and b (U.fromInt 0xFF))

-- | Instance of `Ord` for `Word8` for ordering values.
instance ordWord8 :: Ord Word8 where
    compare (Word8 a) (Word8 b) = compare (U.and a (U.fromInt 0xFF))  (U.and b (U.fromInt 0xFF))

-- | Instance of `Bounded` for `Word8` for bounding values to 8 bits.
instance boundedWord8 :: Bounded Word8 where
    bottom = Word8 $ U.fromInt 0
    top = Word8 $ U.and (U.complement (U.fromInt 0)) (U.fromInt 0xFF)

-- | Instance of `Semiring` for `Word8` for addition and multiplication.
instance semiringWord8 :: Semiring Word8 where
    zero = bottom
    one = Word8 $ U.fromInt 1
    add (Word8 a) (Word8 b) = Word8 $ U.and (a+b) (U.fromInt 0xFF)
    mul (Word8 a) (Word8 b) = Word8 $ U.and (a*b) (U.fromInt 0xFF)

-- | Instance of `Ring` for `Word8` for subtraction.
instance ring8 :: Ring Word8 where
    sub (Word8 a) (Word8 b) = Word8 (a-b)

-- | Instance of `Integral` for `Word8` for conversions between numbers.
instance word8Integral :: Integral Word8 where
    fromBigInt bi = Word8 $ U.fromNumber <<< BI.toNumber $ bi
    toBigInt (Word8 a) = BI.fromInt <<< U.toInt $ a

-- | Instance of `HeytingAlgebra` for `Word8` for bitwise logical operations.
instance heytingAlgebraWord8 :: HeytingAlgebra Word8 where
    ff = Word8 bottom
    tt = Word8 top
    implies (Word8 a) (Word8 b) = Word8 $ U.or (U.complement a) b
    conj (Word8 a) (Word8 b) = Word8 $ U.and a b
    disj (Word8 a) (Word8 b) = Word8 $ U.or a b
    not (Word8 a) = Word8 $ U.complement a

-- | Instance of `BooleanAlgebra` for `Word8`.
instance booleanAlgebra8 :: BooleanAlgebra Word8

-- | Instance of `Shift` for `Word8` for shifting values.
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
