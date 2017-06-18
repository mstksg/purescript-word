module Test.Main where

import Prelude (Unit, discard, top, bottom, ($), (+), (==), show, (<>))
--import Data.Bounded
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)

import Data.Word (Word, fromInt, (.&.), (.|.), (.^.), complement, shl, shr, zshr)

import Test.Assert (ASSERT, assert')
       
main :: forall e. Eff (console :: CONSOLE, assert :: ASSERT | e) Unit
main = do
    log "Run Tests"
    assert' "Word 4  +  Word 2 == Word 6" $ fromInt 4  +  fromInt 2 ==   fromInt 6 :: Word
    assert' "Word 4 .&. Word 4 == Word 4" $ fromInt 4 .&. fromInt 4 == fromInt 4 :: Word
    assert' "Word 4 .|. Word 2 == Word 6" $ fromInt 4 .|. fromInt 2 == fromInt 6 :: Word
    assert' "Word 4 .^. Word 2 == Word 6" $ fromInt 4 .^. fromInt 2 == fromInt 6 :: Word
--    log $ "top " <> (show (top :: Word))
--    log $ "bottom " <> (show (bottom :: Word))
--    log $ "complenent top " <> (show (complement(top::Word)))
--    log $ "complenent bottom " <> (show (complement(bottom::Word)))
    assert' "complement Word 0x00000000 = complement Word 0xFFFFFFFF" $ complement(fromInt 0x00000000) == top :: Word
    assert' "shl (Word 4)  == (Word 8)" $ shl (fromInt 4)  (fromInt 1) == (fromInt 8) :: Word
    assert' "shr (Word 4)  == (Word 2)" $ shr (fromInt 4)  (fromInt 1) == (fromInt 2) :: Word
    assert' "zshr (Word 4) == (Word 2)" $ zshr (fromInt 4) (fromInt 1) == (fromInt 2) :: Word
    log "Done"
