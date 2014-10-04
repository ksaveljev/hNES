module NES.Debug.Emulator where

import Data.Word (Word8, Word16)
import Data.Bits ((.|.))
import Debug.Trace (trace)
import Text.Printf (printf)

import NES.CPU(Flag(..))
import NES.Instruction
import NES.MonadEmulator
import NES.Emulator
import NES.EmulatorHelpers

emulateCycles :: MonadEmulator m => Int -> m ()
emulateCycles 0 = return ()
emulateCycles n = do
    pc <- loadPC
    registersState <- registersSnapshot
    instruction <- decodeInstruction
    execute instruction
    traceCurrentState pc instruction registersState
    emulateCycles $ n - 1

traceCurrentState :: MonadEmulator m => Word16 -> Instruction -> String -> m ()
traceCurrentState pc instruction registers = trace (printf "%04X %-20s" pc (show instruction) ++ " " ++ registers) $ return ()

registersSnapshot :: MonadEmulator m => m String
registersSnapshot = do
    a <- loadA
    x <- loadX
    y <- loadY
    status <- getStatus 
    sp <- loadSP
    return $ printf "A:%02X X:%02X Y:%02X P:%02X SP:%02X" a x y status sp
    where
      getStatus :: MonadEmulator m => m Word8
      getStatus = do
        cf <- getFlag CF >>= (\b -> return $ if b then 0x01 else 0)
        zf <- getFlag ZF >>= (\b -> return $ if b then 0x02 else 0)
        idf <- getFlag IDF >>= (\b -> return $ if b then 0x04 else 0)
        dmf <- getFlag DMF >>= (\b -> return $ if b then 0x08 else 0)
        ovf <- getFlag OF >>= (\b -> return $ if b then 0x40 else 0)
        nf <- getFlag NF >>= (\b -> return $ if b then 0x80 else 0)
        return $ 0x20 .|. cf .|. zf .|. idf .|. dmf .|. ovf .|. nf
