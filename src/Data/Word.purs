module Data.Word
       ( Word
       , fromInt) where
       
import Prelude
import Data.Generic (class Generic, gEq, gCompare, gShow)
       
newtype Word = Word Int

derive instance genericWord :: Generic Word

instance showWord :: Show Word where
    show = gShow

instance eqWord :: Eq Word where
    eq = gEq

instance ordWord :: Ord Word where
    compare = gCompare
    
instance semiringWord :: Semiring Word where
    zero = Word 0
    one = Word 1
    add (Word a) (Word b) = Word (a+b)
    mul (Word a) (Word b) = Word (a*b)

fromInt :: Int -> Word
fromInt = Word <<< add 0
