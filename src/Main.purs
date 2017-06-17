module Main where

import Prelude (Unit, discard, ($), add, (==))
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
       
import Data.Word (Word, fromInt)

import Test.Assert (ASSERT, assert')
       
main :: forall e. Eff (console :: CONSOLE, assert :: ASSERT | e) Unit
main = do
    log "Run Tests"
    assert' "Word 4 + Word 2 == Word 7" $ fromInt 5 `add` fromInt 2 == fromInt 7 :: Word
    log "Done"
