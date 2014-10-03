module NES.Emulator (emulate
                    , emulateCycles
                    , loadProgram
                    ) where

import Data.Word (Word8, Word16)
import Data.Bits ((.&.), (.|.), shiftL, shiftR, xor, testBit, complement)
import Control.Monad (unless, when)
import Control.Applicative ((<$>))
import qualified Data.ByteString as B

import NES.CPU (Flag(..), Storage(..))
import NES.ROM
import NES.Instruction
import NES.MonadEmulator
import NES.EmulatorHelpers
import NES.Util

import Debug.Trace (trace)
import Text.Printf (printf)

-- TODO: execute forever
emulate :: MonadEmulator m => m ()
emulate = do
    instruction <- decodeInstruction
    execute instruction

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

traceCurrentState :: MonadEmulator m => Word16 -> Instruction -> String -> m ()
traceCurrentState pc instruction registers = trace (printf "%04X %-20s" pc (show instruction) ++ " " ++ registers) $ return ()

emulateCycles :: MonadEmulator m => Int -> m ()
emulateCycles 0 = return ()
emulateCycles n = do
    pc <- loadPC
    registersState <- registersSnapshot
    instruction <- decodeInstruction
    execute instruction
    traceCurrentState pc instruction registersState
    emulateCycles $ n - 1

loadProgram :: MonadEmulator m => B.ByteString -> m ()
loadProgram program = do
    -- TODO: Currently support PRG-ROM with only 1 ROM page
    when (prgROMsize (header rom) > 1) $ error "PRG-ROM has more than 1 ROM page!"
    mapM_ (loadPrgROM 0x8000) [0..B.length prg - 1]
    mapM_ (loadPrgROM 0xC000) [0..B.length prg - 1]
    where
      rom = loadROM program
      prg = prgROM rom
      loadPrgROM offset i =
        let byte = B.index prg i
            address = offset + fromIntegral i
        in store8 (Ram address) byte

loadNextWord8 :: MonadEmulator m => m Word8
loadNextWord8 = do
    pc <- load16 Pc
    w8 <- load8 $ Ram pc
    store16 Pc (pc + 1)
    return w8

decodeInstruction :: MonadEmulator m => m Instruction
decodeInstruction = do
    opCode <- loadNextWord8
    let (mn, am) = decodeOpCode opCode
    Instruction mn am <$> case operandLength am of
                            1 -> sequence [loadNextWord8] 
                            2 -> sequence [loadNextWord8, loadNextWord8]
                            _ -> return []

getStorageAddr :: MonadEmulator m => Instruction -> m Storage
getStorageAddr (Instruction _ am arg) =
    case arg of
      [] -> case am of
              Accumulator -> return A
              _ -> oops
      [w8] -> case am of
                ZeroPage -> return $ Ram $ fromIntegral w8
                ZeroPageX -> do
                  x <- load8 X
                  return $ Ram $ fromIntegral $ w8 + x
                ZeroPageY -> do
                  y <- load8 Y
                  return $ Ram $ fromIntegral $ w8 + y
                IndexedIndirect -> do
                  x <- load8 X
                  let addr = fromIntegral $ x + w8 :: Word16
                  l <- load8 $ Ram addr
                  h <- load8 $ Ram $ addr + 1
                  return $ Ram $ makeW16 h l
                IndirectIndexed -> do
                  y <- load8 Y
                  l <- load8 $ Ram $ fromIntegral w8
                  h <- load8 $ Ram $ fromIntegral $ w8 + 1
                  return $ Ram $ makeW16 h l + fromIntegral y
                _ -> oops
      [l,h] -> case am of
                 Absolute -> return $ Ram $ makeW16 h l
                 AbsoluteX -> do
                   x <- load8 X
                   return $ Ram $ makeW16 h l + fromIntegral x
                 AbsoluteY -> do
                   y <- load8 Y
                   return $ Ram $ makeW16 h l + fromIntegral y
                 _ -> oops
      _ -> error "Incorrect Instruction argument in getStorageAddr"
      where
        oops = error "Incorrect Instruction in getStorageAddr"

loadStorageValue8 :: MonadEmulator m => Instruction -> m Word8
loadStorageValue8 instruction@(Instruction _ am arg) =
    case arg of
      [w8] -> case am of
                Immediate -> return w8
                Relative -> return w8
                _ -> load
      _ -> load
  where
    load = do
      addr <- getStorageAddr instruction
      load8 addr

loadStorageValue16 :: MonadEmulator m => Instruction -> m Word16
loadStorageValue16 (Instruction _ am arg) =
    case arg of
      [l,h] -> case am of
                 Absolute -> return $ makeW16 h l
                 Indirect -> do
                   low <- load8 $ Ram $ makeW16 h l
                   high <- load8 $ Ram $ makeW16 h (l + 1) -- 6502 bug
                   return $ makeW16 high low
                 _ -> error "Incorrect Instruction in loadStorageValue16"
      _ -> error "Incorrect Instruction argument in loadStorageValue16"

storeStorageValue8 :: MonadEmulator m => Instruction -> Word8 -> m ()
storeStorageValue8 instruction w8 = getStorageAddr instruction >>= (`store8` w8)

execute :: MonadEmulator m => Instruction -> m ()
execute instruction@(Instruction mv _ _) =
    case mv of
      ADC -> do
        v <- loadStorageValue8 instruction
        a <- load8 A
        carry <- getFlag CF
        let result = v + a + bToW8 carry
        store8 A result
        setCarryFlag $ if carry then result <= v else result < v
        setOverflowFlag $ isOverflow a v result
        setZNFlags result
      AND -> do
        v <- loadStorageValue8 instruction
        a <- load8 A
        let result = v .&. a
        store8 A result
        setZNFlags result
      ASL -> do
        v <- loadStorageValue8 instruction
        let result = v `shiftL` 1
        setCarryFlag $ testBit v 7
        setZNFlags result
        storeStorageValue8 instruction result
      BCC -> do
        v <- loadStorageValue8 instruction
        carry <- getFlag CF
        pc <- load16 Pc
        unless carry $ store16 Pc $ pc + fromIntegral (makeSigned v)
      BCS -> do
        v <- loadStorageValue8 instruction
        carry <- getFlag CF
        pc <- load16 Pc
        when carry $ store16 Pc $ pc + fromIntegral (makeSigned v)
      BEQ -> do
        v <- loadStorageValue8 instruction
        zero <- getFlag ZF
        pc <- load16 Pc
        when zero $ store16 Pc $ pc + fromIntegral (makeSigned v)
      BIT -> do
        v <- loadStorageValue8 instruction
        a <- load8 A
        setZeroFlag (v .&. a)
        setOverflowFlag $ testBit v 6
        setNegativeFlag v
      BMI -> do
        v <- loadStorageValue8 instruction
        negative <- getFlag NF
        pc <- load16 Pc
        when negative $ store16 Pc $ pc + fromIntegral (makeSigned v)
      BNE -> do
        v <- loadStorageValue8 instruction
        zero <- getFlag ZF
        pc <- load16 Pc
        unless zero $ store16 Pc $ pc + fromIntegral (makeSigned v)
      BPL -> do
        v <- loadStorageValue8 instruction
        negative <- getFlag NF
        pc <- load16 Pc
        unless negative $ store16 Pc $ pc + fromIntegral (makeSigned v)
      BRK -> do
        pc <- loadPC
        push $ fromIntegral $ pc `shiftR` 8
        push $ fromIntegral pc
        setBFlag True
        load8 SR >>= push
        setBreakCommandFlag True
        low <- loadRAM 0xFFFE
        high <- loadRAM 0xFFFF
        storePC $ makeW16 high low
      BVC -> do
        v <- loadStorageValue8 instruction
        overflow <- getOverflowFlag
        pc <- loadPC
        unless overflow $ storePC $ pc + fromIntegral (makeSigned v)
      BVS -> do
        v <- loadStorageValue8 instruction
        overflow <- getOverflowFlag
        pc <- loadPC
        when overflow $ storePC $ pc + fromIntegral (makeSigned v)
      CLC -> setCarryFlag False
      CLD -> setDecimalModeFlag False
      CLI -> setInterruptDisableFlag False
      CLV -> setOverflowFlag False
      CMP -> do
        v <- loadStorageValue8 instruction
        a <- loadA
        let result = a - v
        setCarryFlag $ a >= v
        setZNFlags result
      CPX -> do
        v <- loadStorageValue8 instruction
        x <- loadX
        let result = x - v
        setCarryFlag $ x >= v
        setZNFlags result
      CPY -> do
        v <- loadStorageValue8 instruction
        y <- loadY
        let result = y - v
        setCarryFlag $ y >= v
        setZNFlags result
      DEC -> do
        v <- loadStorageValue8 instruction
        let result = v - 1
        storeStorageValue8 instruction result
        setZNFlags result
      DEX -> do
        x <- loadX
        let result = x - 1
        storeX result
        setZNFlags result
      DEY -> do
        y <- loadY
        let result = y - 1
        storeY result
        setZNFlags result
      EOR -> do
        v <- loadStorageValue8 instruction
        a <- loadA
        let result = a `xor` v
        storeA result
        setZNFlags result
      INC -> do
        v <- loadStorageValue8 instruction
        let result = v + 1
        storeStorageValue8 instruction result
        setZNFlags result
      INX -> do
        x <- loadX
        let result = x + 1
        storeX result
        setZNFlags result
      INY -> do
        y <- loadY
        let result = y + 1
        storeY result
        setZNFlags result
      JMP -> do
        addr <- loadStorageValue16 instruction
        storePC addr
      JSR -> do
        addr <- loadStorageValue16 instruction
        pc <- loadPC
        let pc' = pc - 1
        push $ fromIntegral $ pc' `shiftR` 8
        push $ fromIntegral pc' 
        storePC addr
      LDA -> do
        v <- loadStorageValue8 instruction
        storeA v
        setZNFlags v
      LDX -> do
        v <- loadStorageValue8 instruction
        storeX v
        setZNFlags v
      LDY -> do
        v <- loadStorageValue8 instruction
        storeY v
        setZNFlags v
      LSR -> do
        v <- loadStorageValue8 instruction
        let result = v `shiftR` 1
        storeStorageValue8 instruction result
        setCarryFlag $ testBit v 0
        setZNFlags result
      NOP -> return ()
      ORA -> do
        v <- loadStorageValue8 instruction
        a <- loadA
        let result = a .|. v
        storeA result
        setZNFlags result
      PHA -> do
        a <- loadA
        push a
      PHP -> do
        setBFlag True
        status <- loadSR
        push status
      PLA -> do
        a <- pop
        storeA a
        setZNFlags a
      PLP -> do
        status <- pop
        storeSR status
      ROL -> do
        v <- loadStorageValue8 instruction
        carry <- bToW8 <$> getCarryFlag
        let result = (v `shiftL` 1) .|. carry
        storeStorageValue8 instruction result
        setCarryFlag $ testBit v 7
        setZNFlags result
      ROR -> do
        v <- loadStorageValue8 instruction
        carry <- getCarryFlag
        let result = (v `shiftR` 1) .|. if carry then 0x80 else 0
        storeStorageValue8 instruction result
        setCarryFlag $ testBit v 0
        setZNFlags result
      RTI -> do
        status <- pop
        low <- pop
        high <- pop
        storeSR status
        storePC $ makeW16 high low
      RTS -> do
        low <- pop
        high <- pop
        storePC $ makeW16 high low + 1
      SBC -> do
        v <- loadStorageValue8 instruction
        a <- loadA
        carry <- bToW8 <$> getCarryFlag
        let result = a - v - (1 - carry)
        storeA result
        setCarryFlag $ if carry == 1 then result <= a else result < a
        setOverflowFlag $ isOverflow a (complement v) result
        setZNFlags result
      SEC -> setCarryFlag True
      SED -> setDecimalModeFlag True
      SEI -> setInterruptDisableFlag True
      STA -> do
        a <- loadA
        storeStorageValue8 instruction a
      STX -> do
        x <- loadX
        storeStorageValue8 instruction x
      STY -> do
        y <- loadY
        storeStorageValue8 instruction y
      TAX -> do
        a <- loadA
        storeX a
        setZNFlags a
      TAY -> do
        a <- loadA
        storeY a
        setZNFlags a
      TSX -> do
        sp <- loadSP
        storeX sp
        setZNFlags sp
      TXA -> do
        x <- loadX
        storeA x
        setZNFlags x
      TXS -> do
        x <- loadX
        storeSP x
      TYA -> do
        y <- loadY
        storeA y
        setZNFlags y
