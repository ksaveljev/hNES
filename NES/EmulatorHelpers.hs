module NES.EmulatorHelpers where

import Data.Word (Word8, Word16)
import Data.Bits (testBit)

import NES.CPU (Flag(..), Storage(..))
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

setBFlag :: MonadEmulator m => Bool -> m ()
setBFlag = setFlag BF

setBreakCommandFlag :: MonadEmulator m => Bool -> m ()
setBreakCommandFlag = setFlag BCF

setDecimalModeFlag :: MonadEmulator m => Bool -> m ()
setDecimalModeFlag = setFlag DMF

setInterruptDisableFlag :: MonadEmulator m => Bool -> m ()
setInterruptDisableFlag = setFlag IDF

setZNFlags :: MonadEmulator m => Word8 -> m ()
setZNFlags w8 = do
    setZeroFlag w8
    setNegativeFlag w8

loadSP :: MonadEmulator m => m Word8
loadSP = load8 Sp

storeSP :: MonadEmulator m => Word8 -> m ()
storeSP = store8 Sp

loadPC :: MonadEmulator m => m Word16
loadPC = load16 Pc

storePC :: MonadEmulator m => Word16 -> m ()
storePC = store16 Pc

loadA :: MonadEmulator m => m Word8
loadA = load8 A

storeA :: MonadEmulator m => Word8 -> m ()
storeA = store8 A

alterA :: MonadEmulator m => (Word8 -> Word8) -> m Word8
alterA f = loadA >>= storeA . f >> loadA

loadX :: MonadEmulator m => m Word8
loadX = load8 X

storeX :: MonadEmulator m => Word8 -> m ()
storeX = store8 X

alterX :: MonadEmulator m => (Word8 -> Word8) -> m Word8
alterX f = loadX >>= storeX . f >> loadX

loadY :: MonadEmulator m => m Word8
loadY = load8 Y

storeY :: MonadEmulator m => Word8 -> m ()
storeY = store8 Y

alterY :: MonadEmulator m => (Word8 -> Word8) -> m Word8
alterY f = loadY >>= storeY . f >> loadY

loadRAM :: MonadEmulator m => Word16 -> m Word8
loadRAM = load8 . Ram

loadSR :: MonadEmulator m => m Word8
loadSR = load8 SR

storeSR :: MonadEmulator m => Word8 -> m ()
storeSR = store8 SR

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

alterCpuCycles :: MonadEmulator m => Word8 -> m()
alterCpuCycles w8 = getCpuCycles >>= \v -> setCpuCycles $ v + fromIntegral w8
