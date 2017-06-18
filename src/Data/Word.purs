module Data.Word
       ( Word
       , fromInt
       , and, (.&.)
       , or, (.|.)
       , xor, (.^.)
       , complement) where
       
import Prelude
import Data.Generic (class Generic, gEq, gCompare, gShow)
import Data.Int.Bits as B
       
newtype Word = Word Int

derive instance genericWord :: Generic Word

instance showWord :: Show Word where
    show = gShow

instance eqWord :: Eq Word where
    eq = gEq

instance ordWord :: Ord Word where
    compare = gCompare

--instance boundedWord :: Bounded Word where
--    bottom = fromInt 0
--    top = fromInt 0xFFFFFFFF
    
instance semiringWord :: Semiring Word where
    zero = Word 0
    one = Word 1
    add (Word a) (Word b) = Word (a+b)
    mul (Word a) (Word b) = Word (a*b)

fromInt :: Int -> Word
fromInt = Word <<< add 0

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
complement (Word a) = Word $ B.and (B.complement a) 0xFFFF
