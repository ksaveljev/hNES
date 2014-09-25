module NES.EmulatorHelpers where

import Data.Word (Word8, Word16)
import Data.Bits (testBit)

import NES.CPU
import NES.MonadEmulator

getCarryFlag :: MonadEmulator m => m Bool
getCarryFlag = getFlag CF

getZeroFlag :: MonadEmulator m => m Bool
getZeroFlag = getFlag ZF

getInterruptDisableFlag :: MonadEmulator m => m Bool
getInterruptDisableFlag = getFlag IDF

getDecimalModeFlag :: MonadEmulator m => m Bool
getDecimalModeFlag = getFlag DMF

getBreakCommandFlag :: MonadEmulator m => m Bool
getBreakCommandFlag = getFlag BCF

getOverflowFlag :: MonadEmulator m => m Bool
getOverflowFlag = getFlag OF

getNegativeFlag :: MonadEmulator m => m Bool
getNegativeFlag = getFlag NF

setNegativeFlag :: MonadEmulator m => Word8 -> m ()
setNegativeFlag = setFlag NF . (`testBit` 7)

setCarryFlag :: MonadEmulator m => Bool -> m ()
setCarryFlag = setFlag CF

setOverflowFlag :: MonadEmulator m => Bool -> m ()
setOverflowFlag = setFlag OF

setZeroFlag :: MonadEmulator m => Word8 -> m ()
setZeroFlag = setFlag ZF . (== 0)

setBreakCommandFlag :: MonadEmulator m => Bool -> m ()
setBreakCommandFlag = setFlag BCF

setDecimalModeFlag :: MonadEmulator m => Bool -> m ()
setDecimalModeFlag = setFlag DMF

setInterruptDisableFlag :: MonadEmulator m => Bool -> m ()
setInterruptDisableFlag = setFlag IDF

loadSP :: MonadEmulator m => m Word8
loadSP = load8 Sp

storeSP :: MonadEmulator m => Word8 -> m ()
storeSP = store8 Sp

loadPC :: MonadEmulator m => m Word16
loadPC = load16 Pc

loadA :: MonadEmulator m => m Word8
loadA = load8 A

loadX :: MonadEmulator m => m Word8
loadX = load8 X

loadY :: MonadEmulator m => m Word8
loadY = load8 Y

storePC :: MonadEmulator m => Word16 -> m ()
storePC = store16 Pc

loadRAM :: MonadEmulator m => Word16 -> m Word8
loadRAM = load8 . Ram

push :: MonadEmulator m => Word8 -> m ()
push w8 = do
    sp <- loadSP
    storeSP $ sp - 1
    store8 (Ram $ 0x100 + fromIntegral sp) w8

pop :: MonadEmulator m => m Word8
pop = do
    sp <- loadSP
    storeSP $ sp + 1
    load8 $ Ram $ 0x100 + fromIntegral sp + 1
