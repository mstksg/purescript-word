module Test.Main where

import Prelude (Unit, discard, top, ($), (+), (==))

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Data.BooleanAlgebra (not)
import Data.UInt (fromInt)
       
import Data.Integral (fromIntegral)
import Data.Shift (shl, shr, zshr)
import Data.Word (Word, Word32, Word16, Word8, (.&.), (.|.))

import Test.Assert (ASSERT, assert')
    
main :: forall e. Eff (console :: CONSOLE, assert :: ASSERT | e) Unit
main = do
    log "Run Tests"
    assert' "Word 4  +  Word 2 == Word 6" $ fromIntegral 4  +  fromIntegral 2 == fromIntegral 6 :: Word
    assert' "Word 4 .&. Word 4 == Word 4" $ fromIntegral 4 .&. fromIntegral 4 == fromIntegral 4 :: Word
    assert' "Word 4 .|. Word 2 == Word 6" $ fromIntegral 4 .|. fromIntegral 2 == fromIntegral 6 :: Word
    assert' "not Word 0x00000000 = complement Word 0xFFFFFFFF" $ not(fromIntegral 0x00000000) == top :: Word
    assert' "shl (Word 4)  == (Word 8)" $ shl (fromIntegral 4)  (fromInt 1) == (fromIntegral 8) :: Word
    assert' "shr (Word 4)  == (Word 2)" $ shr (fromIntegral 4)  (fromInt 1) == (fromIntegral 2) :: Word
    assert' "zshr (Word 4) == (Word 2)" $ zshr (fromIntegral 4) (fromInt 1) == (fromIntegral 2) :: Word

    assert' "Word32 4  +  Word32 2 == Word32 6" $ fromIntegral 4  +  fromIntegral 2 == fromIntegral 6 :: Word32
    assert' "Word32 4 .&. Word32 4 == Word32 4" $ fromIntegral 4 .&. fromIntegral 4 == fromIntegral 4 :: Word32
    assert' "Word32 4 .|. Word32 2 == Word32 6" $ fromIntegral 4 .|. fromIntegral 2 == fromIntegral 6 :: Word32
    assert' "not Word32 0x00000000 = complement Word32 0xFFFFFFFF" $ not(fromIntegral 0x00000000) == top :: Word32
    assert' "shl (Word32 4)  == (Word32 8)" $ shl (fromIntegral 4)  (fromInt 1) == (fromIntegral 8) :: Word32
    assert' "shr (Word32 4)  == (Word32 2)" $ shr (fromIntegral 4)  (fromInt 1) == (fromIntegral 2) :: Word32
    assert' "zshr (Word32 4) == (Word32 2)" $ zshr (fromIntegral 4) (fromInt 1) == (fromIntegral 2) :: Word32

    assert' "Word16 4  +  Word16 2 == Word16 6" $ fromIntegral 4  +  fromIntegral 2 == fromIntegral 6 :: Word16
    assert' "Word16 4 .&. Word16 4 == Word16 4" $ fromIntegral 4 .&. fromIntegral 4 == fromIntegral 4 :: Word16
    assert' "Word16 4 .|. Word16 2 == Word16 6" $ fromIntegral 4 .|. fromIntegral 2 == fromIntegral 6 :: Word16
    assert' "not Word16 0x00000000 = complement Word16 0xFFFFFFFF" $ not(fromIntegral 0x00000000) == top :: Word16
    assert' "shl (Word16 4)  == (Word16 8)" $ shl (fromIntegral 4)  (fromInt 1) == (fromIntegral 8) :: Word16
    assert' "shr (Word16 4)  == (Word16 2)" $ shr (fromIntegral 4)  (fromInt 1) == (fromIntegral 2) :: Word16
    assert' "zshr (Word16 4) == (Word16 2)" $ zshr (fromIntegral 4) (fromInt 1) == (fromIntegral 2) :: Word16

    assert' "Word8 4  +  Word8 2 == Word8 6" $ fromIntegral 4  +  fromIntegral 2 == fromIntegral 6 :: Word8
    assert' "Word8 4 .&. Word8 4 == Word8 4" $ fromIntegral 4 .&. fromIntegral 4 == fromIntegral 4 :: Word8
    assert' "Word8 4 .|. Word8 2 == Word8 6" $ fromIntegral 4 .|. fromIntegral 2 == fromIntegral 6 :: Word8
    assert' "not Word8 0x00000000 = complement Word8 0xFFFFFFFF" $ not(fromIntegral 0x00000000) == top :: Word8
    assert' "shl (Word8 4)  == (Word8 8)" $ shl (fromIntegral 4)  (fromInt 1) == (fromIntegral 8) :: Word8
    assert' "shr (Word8 4)  == (Word8 2)" $ shr (fromIntegral 4)  (fromInt 1) == (fromIntegral 2) :: Word8
    assert' "zshr (Word8 4) == (Word8 2)" $ zshr (fromIntegral 4) (fromInt 1) == (fromIntegral 2) :: Word8

    log "Done"
