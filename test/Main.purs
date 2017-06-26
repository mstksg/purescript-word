module Test.Main where

import Prelude (Unit, show, (<>), discard, top, bottom, ($), (+), (-), (==))

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Data.BooleanAlgebra (not)
import Data.UInt (fromInt)
       
import Data.Integral (fromIntegral)
import Data.Shift (shl, shr, zshr)
import Data.Word (Word, Word64, Word32, Word16, Word8, (.&.), (.|.))

import Test.Assert (ASSERT, assert')
    
main :: forall e. Eff (console :: CONSOLE, assert :: ASSERT | e) Unit
main = do
    log "Run Tests"
    -- Some simple obvious tests.
    assert' "Word 4  +  Word 2 == Word 6" $ fromIntegral 4  +  fromIntegral 2 == fromIntegral 6 :: Word
    assert' "Word 6 .&. Word 4 == Word 4" $ fromIntegral 6 .&. fromIntegral 4 == fromIntegral 4 :: Word
    assert' "Word 4 .|. Word 2 == Word 6" $ fromIntegral 4 .|. fromIntegral 2 == fromIntegral 6 :: Word
    assert' "not Word 0x00000000 = complement Word 0xFFFFFFFF" $ not(fromIntegral 0x00000000) == top :: Word
    assert' "shl (Word 4)  == (Word 8)" $ shl (fromIntegral 4)  (fromInt 1) == (fromIntegral 8) :: Word
    assert' "shr (Word 4)  == (Word 2)" $ shr (fromIntegral 4)  (fromInt 1) == (fromIntegral 2) :: Word
    assert' "zshr (Word 4) == (Word 2)" $ zshr (fromIntegral 4) (fromInt 1) == (fromIntegral 2) :: Word

    assert' "Word64 4  +  Word64 2 == Word64 6" $ fromIntegral 4  +  fromIntegral 2 == fromIntegral 6 :: Word64
    assert' "Word64 6 .&. Word64 4 == Word64 4" $ fromIntegral 6 .&. fromIntegral 4 == fromIntegral 4 :: Word64
    assert' "Word64 4 .|. Word64 2 == Word64 6" $ fromIntegral 4 .|. fromIntegral 2 == fromIntegral 6 :: Word64
    assert' "not Word64 0x0000000000000000 = complement Word64 0xFFFFFFFFFFFFFFFF" $ not(fromIntegral 0x00000000) == top :: Word64
    assert' "shl (Word64 4)  == (Word64 8)" $ shl (fromIntegral 4)  (fromInt 1) == (fromIntegral 8) :: Word64
    assert' "shr (Word64 4)  == (Word64 2)" $ shr (fromIntegral 4)  (fromInt 1) == (fromIntegral 2) :: Word64
    assert' "zshr (Word64 4) == (Word64 2)" $ zshr (fromIntegral 4) (fromInt 1) == (fromIntegral 2) :: Word64

    assert' "Word32 4  +  Word32 2 == Word32 6" $ fromIntegral 4  +  fromIntegral 2 == fromIntegral 6 :: Word32
    assert' "Word32 6 .&. Word32 4 == Word32 4" $ fromIntegral 6 .&. fromIntegral 4 == fromIntegral 4 :: Word32
    assert' "Word32 4 .|. Word32 2 == Word32 6" $ fromIntegral 4 .|. fromIntegral 2 == fromIntegral 6 :: Word32
    assert' "not Word32 0x00000000 = complement Word32 0xFFFFFFFF" $ not(fromIntegral 0x00000000) == top :: Word32
    assert' "shl (Word32 4)  == (Word32 8)" $ shl (fromIntegral 4)  (fromInt 1) == (fromIntegral 8) :: Word32
    assert' "shr (Word32 4)  == (Word32 2)" $ shr (fromIntegral 4)  (fromInt 1) == (fromIntegral 2) :: Word32
    assert' "zshr (Word32 4) == (Word32 2)" $ zshr (fromIntegral 4) (fromInt 1) == (fromIntegral 2) :: Word32

    assert' "Word16 4  +  Word16 2 == Word16 6" $ fromIntegral 4  +  fromIntegral 2 == fromIntegral 6 :: Word16
    assert' "Word16 6 .&. Word16 4 == Word16 4" $ fromIntegral 6 .&. fromIntegral 4 == fromIntegral 4 :: Word16
    assert' "Word16 4 .|. Word16 2 == Word16 6" $ fromIntegral 4 .|. fromIntegral 2 == fromIntegral 6 :: Word16
    assert' "not Word16 0x00000000 = complement Word16 0xFFFFFFFF" $ not(fromIntegral 0x0000) == top :: Word16
    assert' "shl (Word16 4)  == (Word16 8)" $ shl (fromIntegral 4)  (fromInt 1) == (fromIntegral 8) :: Word16
    assert' "shr (Word16 4)  == (Word16 2)" $ shr (fromIntegral 4)  (fromInt 1) == (fromIntegral 2) :: Word16
    assert' "zshr (Word16 4) == (Word16 2)" $ zshr (fromIntegral 4) (fromInt 1) == (fromIntegral 2) :: Word16

    assert' "Word8 4  +  Word8 2 == Word8 6" $ fromIntegral 4  +  fromIntegral 2 == fromIntegral 6 :: Word8
    assert' "Word8 6 .&. Word8 4 == Word8 4" $ fromIntegral 6 .&. fromIntegral 4 == fromIntegral 4 :: Word8
    assert' "Word8 4 .|. Word8 2 == Word8 6" $ fromIntegral 4 .|. fromIntegral 2 == fromIntegral 6 :: Word8
    assert' "not Word8 0x00000000 = complement Word8 0xFFFFFFFF" $ not(fromIntegral 0x00) == top :: Word8
    assert' "shl (Word8 4)  == (Word8 8)" $ shl (fromIntegral 4)  (fromInt 1) == (fromIntegral 8) :: Word8
    assert' "shr (Word8 4)  == (Word8 2)" $ shr (fromIntegral 4)  (fromInt 1) == (fromIntegral 2) :: Word8
    assert' "zshr (Word8 4) == (Word8 2)" $ zshr (fromIntegral 4) (fromInt 1) == (fromIntegral 2) :: Word8

    -- Wrapping with +/-
    assert' "top + 1 == 0: 64" $ top + (fromIntegral 1) == bottom :: Word64
    assert' "top + 1 == 0: 32" $ top + (fromIntegral 1) == bottom :: Word32    
    assert' "top + 1 == 0: 16" $ top + (fromIntegral 1) == bottom :: Word16
    assert' "top + 1 == 0: 8" $ top + (fromIntegral 1) == bottom :: Word8
    assert' "bottom - 1 == 0: 64" $ bottom - (fromIntegral 1) == top :: Word64    
    assert' "bottom - 1 == 0: 32" $ bottom - (fromIntegral 1) == top :: Word32    
    assert' "bottom - 1 == 0: 16" $ bottom - (fromIntegral 1) == top :: Word16
    assert' "bottom - 1 == 0: 8" $ bottom - (fromIntegral 1) == top :: Word8
    
                                                          -- Shifts galore, because things can go wrong eaiser
    assert' "zshr bottom 64 == bottom" $ zshr bottom (fromInt 64) == bottom :: Word64
    assert' "zshr bottom 32 == bottom" $ zshr bottom (fromInt 32) == bottom :: Word32
    assert' "zshr bottom 16 == bottom" $ zshr bottom (fromInt 16) == bottom :: Word16
    assert' "zshr bottom 8 == bottom" $ zshr bottom (fromInt 8) == bottom :: Word8
    assert' "zshr bottom 65 == bottom" $ zshr bottom (fromInt 65) == bottom :: Word64
    assert' "zshr bottom 33 == bottom" $ zshr bottom (fromInt 33) == bottom :: Word32
    assert' "zshr bottom 17 == bottom" $ zshr bottom (fromInt 17) == bottom :: Word16
    assert' "zshr bottom 9 == bottom" $ zshr bottom (fromInt 9) == bottom :: Word8
    assert' "zshr bottom 63 == bottom" $ zshr bottom (fromInt 63) == bottom + bottom :: Word64
    assert' "zshr bottom 31 == bottom" $ zshr bottom (fromInt 31) == bottom + bottom :: Word32
    assert' "zshr bottom 15 == bottom" $ zshr bottom (fromInt 15) == bottom + bottom :: Word16
    assert' "zshr bottom 7 == bottom" $ zshr bottom (fromInt 7) == bottom + bottom :: Word8

    assert' "zshr top 64 == bottom" $ zshr top (fromInt 64) == bottom :: Word64
    assert' "zshr top 32 == bottom" $ zshr top (fromInt 32) == bottom :: Word32
    assert' "zshr top 16 == bottom" $ zshr top (fromInt 16) == bottom :: Word16
    assert' "zshr top 8 == bottom" $ zshr top (fromInt 8) == bottom :: Word8
    assert' "zshr top 65 == bottom" $ zshr top (fromInt 65) == bottom :: Word64
    assert' "zshr top 33 == bottom" $ zshr top (fromInt 33) == bottom :: Word32
    assert' "zshr top 17 == bottom" $ zshr top (fromInt 17) == bottom :: Word16
    assert' "zshr top 9 == bottom" $ zshr top (fromInt 9) == bottom :: Word8
    assert' "zshr top 63 == bottom" $ zshr top (fromInt 63) == bottom + (fromIntegral 1) :: Word64
    assert' "zshr top 31 == bottom" $ zshr top (fromInt 31) == bottom + (fromIntegral 1) :: Word32
    assert' "zshr top 15 == bottom" $ zshr top (fromInt 15) == bottom + (fromIntegral 1) :: Word16
    assert' "zshr top 7 == bottom" $ zshr top (fromInt 7) == bottom + (fromIntegral 1) :: Word8

    assert' "shr bottom 64 == bottom" $ shr bottom (fromInt 64) == bottom :: Word64
    assert' "shr bottom 32 == bottom" $ shr bottom (fromInt 32) == bottom :: Word32
    assert' "shr bottom 16 == bottom" $ shr bottom (fromInt 16) == bottom :: Word16
    assert' "shr bottom 8 == bottom" $ shr bottom (fromInt 8) == bottom :: Word8
    assert' "shr bottom 65 == bottom" $ shr bottom (fromInt 65) == bottom :: Word64
    assert' "shr bottom 33 == bottom" $ shr bottom (fromInt 33) == bottom :: Word32
    assert' "shr bottom 17 == bottom" $ shr bottom (fromInt 17) == bottom :: Word16
    assert' "shr bottom 9 == bottom" $ shr bottom (fromInt 9) == bottom :: Word8
    assert' "shr bottom 63 == bottom" $ shr bottom (fromInt 63) == bottom :: Word64
    assert' "shr bottom 31 == bottom" $ shr bottom (fromInt 31) == bottom :: Word32
    assert' "shr bottom 15 == bottom" $ shr bottom (fromInt 15) == bottom :: Word16
    assert' "shr bottom 7 == bottom" $ shr bottom (fromInt 7) == bottom :: Word8

    assert' "shr top 64 == top" $ shr top (fromInt 64) == top :: Word64
    assert' "shr top 32 == top" $ shr top (fromInt 32) == top :: Word32
    assert' "shr top 16 == top" $ shr top (fromInt 16) == top :: Word16
    assert' "shr top 8 == top" $ shr top (fromInt 8) == top :: Word8
    assert' "shr top 65 == top" $ shr top (fromInt 65) == top :: Word64
    assert' "shr top 33 == top" $ shr top (fromInt 33) == top :: Word32
    assert' "shr top 17 == top" $ shr top (fromInt 17) == top :: Word16
    assert' "shr top 9 == top" $ shr top (fromInt 9) == top :: Word8
    assert' "shr top 63 == top" $ shr top (fromInt 63) == top :: Word64
    assert' "shr top 31 == top" $ shr top (fromInt 31) == top :: Word32
    assert' "shr top 15 == top" $ shr top (fromInt 15) == top :: Word16
    assert' "shr top 7 == top" $ shr top (fromInt 7) == top :: Word8

    log "Done"
