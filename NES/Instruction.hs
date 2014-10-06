module NES.Instruction ( decodeOpCode
                       , AddressingMode(..)
                       , Mnemonic(..)
                       , Instruction(..)
                       , operandLength
                       ) where

import Data.Word (Word8)
import Text.Printf (printf)

import NES.Util (makeW16)

type OpCode = Word8
type Cycles = Word8

data AddressingMode = Implicit
                    | Accumulator
                    | Immediate
                    | ZeroPage
                    | ZeroPageX
                    | ZeroPageY
                    | Relative
                    | Absolute
                    | AbsoluteX
                    | AbsoluteY
                    | Indirect
                    | IndexedIndirect
                    | IndirectIndexed

data Mnemonic = ADC | AND | ASL | BCC
              | BCS | BEQ | BIT | BMI
              | BNE | BPL | BRK | BVC
              | BVS | CLC | CLD | CLI
              | CLV | CMP | CPX | CPY
              | DEC | DEX | DEY | EOR
              | INC | INX | INY | JMP
              | JSR | LDA | LDX | LDY
              | LSR | NOP | ORA | PHA
              | PHP | PLA | PLP | ROL
              | ROR | RTI | RTS | SBC
              | SEC | SED | SEI | STA
              | STX | STY | TAX | TAY
              | TSX | TXA | TXS | TYA
              deriving (Show)

data Instruction = Instruction OpCode Cycles Mnemonic AddressingMode [Word8]

instance Show Instruction where
    show (Instruction _ _ mn am arg) = show mn ++ showArgument
      where
        mkw16 [a,b] = makeW16 b a
        mkw16 _ = error "mkw16 error"
        showArgument =
          case am of
            Implicit -> ""
            Accumulator -> " A"
            Immediate -> " #$" ++ printf "%02X" (head arg)
            ZeroPage -> " $" ++ printf "%02X" (head arg)
            ZeroPageX -> " $" ++ printf "%02X,X" (head arg)
            ZeroPageY -> " $" ++ printf "%02X,Y" (head arg)
            Relative -> " $" ++ printf "*+%02X" (head arg)
            Absolute -> " $" ++ printf "%04X" (mkw16 arg)
            AbsoluteX -> " $" ++ printf "%04X,X" (mkw16 arg)
            AbsoluteY -> " $" ++ printf "%04X,Y" (mkw16 arg)
            Indirect -> " ($" ++ printf "%04X" (mkw16 arg) ++ ")"
            IndexedIndirect -> " ($" ++ printf "%02X,X)" (head arg)
            IndirectIndexed -> " ($" ++ printf "%02X),Y" (head arg)

operandLength :: AddressingMode -> Word8
operandLength Implicit        = 0
operandLength Accumulator     = 0
operandLength Immediate       = 1
operandLength ZeroPage        = 1
operandLength ZeroPageX       = 1
operandLength ZeroPageY       = 1
operandLength Relative        = 1
operandLength Absolute        = 2
operandLength AbsoluteX       = 2
operandLength AbsoluteY       = 2
operandLength Indirect        = 2
operandLength IndexedIndirect = 1
operandLength IndirectIndexed = 1

decodeOpCode :: Word8 -> (Mnemonic, AddressingMode, Cycles)
decodeOpCode w =
    case w of
      0x69 -> (ADC, Immediate,       2)
      0x65 -> (ADC, ZeroPage,        3)
      0x75 -> (ADC, ZeroPageX,       4)
      0x6D -> (ADC, Absolute,        4)
      0x7D -> (ADC, AbsoluteX,       4) -- +1 if page crossed
      0x79 -> (ADC, AbsoluteY,       4) -- +1 if page crossed
      0x61 -> (ADC, IndexedIndirect, 6)
      0x71 -> (ADC, IndirectIndexed, 5) -- +1 if page crossed
      0x29 -> (AND, Immediate,       2)
      0x25 -> (AND, ZeroPage,        3)
      0x35 -> (AND, ZeroPageX,       4)
      0x2D -> (AND, Absolute,        4)
      0x3D -> (AND, AbsoluteX,       4) -- +1 if page crossed
      0x39 -> (AND, AbsoluteY,       4) -- +1 if page crossed
      0x21 -> (AND, IndexedIndirect, 6)
      0x31 -> (AND, IndirectIndexed, 5)
      0x0A -> (ASL, Accumulator,     2)
      0x06 -> (ASL, ZeroPage,        5)
      0x16 -> (ASL, ZeroPageX,       6)
      0x0E -> (ASL, Absolute,        6)
      0x1E -> (ASL, AbsoluteX,       7)
      0x90 -> (BCC, Relative,        2) -- +1 if branch succeeds +2 if to a new page
      0xB0 -> (BCS, Relative,        2) -- +1 if branch succeeds +2 if to a new page
      0xF0 -> (BEQ, Relative,        2) -- +1 if branch succeeds +2 if to a new page
      0x24 -> (BIT, ZeroPage,        3)
      0x2C -> (BIT, Absolute,        4)
      0x30 -> (BMI, Relative,        2) -- +1 if branch succeeds +2 if to a new page
      0xD0 -> (BNE, Relative,        2) -- +1 if branch succeeds +2 if to a new page
      0x10 -> (BPL, Relative,        2) -- +1 if branch succeeds +2 if to a new page
      0x00 -> (BRK, Implicit,        7)
      0x50 -> (BVC, Relative,        2) -- +1 if branch succeeds +2 if to a new page
      0x70 -> (BVS, Relative,        2) -- +1 if branch succeeds +2 if to a new page
      0x18 -> (CLC, Implicit,        2)
      0xD8 -> (CLD, Implicit,        2)
      0x58 -> (CLI, Implicit,        2)
      0xB8 -> (CLV, Implicit,        2)
      0xC9 -> (CMP, Immediate,       2)
      0xC5 -> (CMP, ZeroPage,        3)
      0xD5 -> (CMP, ZeroPageX,       4)
      0xCD -> (CMP, Absolute,        4)
      0xDD -> (CMP, AbsoluteX,       4) -- +1 if page crossed
      0xD9 -> (CMP, AbsoluteY,       4) -- +1 if page crossed
      0xC1 -> (CMP, IndexedIndirect, 6)
      0xD1 -> (CMP, IndirectIndexed, 5) -- +1 if page crossed
      0xE0 -> (CPX, Immediate,       2)
      0xE4 -> (CPX, ZeroPage,        3)
      0xEC -> (CPX, Absolute,        4)
      0xC0 -> (CPY, Immediate,       2)
      0xC4 -> (CPY, ZeroPage,        3)
      0xCC -> (CPY, Absolute,        4)
      0xC6 -> (DEC, ZeroPage,        5)
      0xD6 -> (DEC, ZeroPageX,       6)
      0xCE -> (DEC, Absolute,        6)
      0xDE -> (DEC, AbsoluteX,       7)
      0xCA -> (DEX, Implicit,        2)
      0x88 -> (DEY, Implicit,        2)
      0x49 -> (EOR, Immediate,       2)
      0x45 -> (EOR, ZeroPage,        3)
      0x55 -> (EOR, ZeroPageX,       4)
      0x4D -> (EOR, Absolute,        4)
      0x5D -> (EOR, AbsoluteX,       4) -- +1 if page crossed
      0x59 -> (EOR, AbsoluteY,       4) -- +1 if page crossed
      0x41 -> (EOR, IndexedIndirect, 6)
      0x51 -> (EOR, IndirectIndexed, 5) -- +1 if page crossed
      0xE6 -> (INC, ZeroPage,        5)
      0xF6 -> (INC, ZeroPageX,       6)
      0xEE -> (INC, Absolute,        6)
      0xFE -> (INC, AbsoluteX,       7)
      0xE8 -> (INX, Implicit,        2)
      0xC8 -> (INY, Implicit,        2)
      0x4C -> (JMP, Absolute,        3)
      0x6C -> (JMP, Indirect,        5)
      0x20 -> (JSR, Absolute,        6)
      0xA9 -> (LDA, Immediate,       2)
      0xA5 -> (LDA, ZeroPage,        3)
      0xB5 -> (LDA, ZeroPageX,       4)
      0xAD -> (LDA, Absolute,        4)
      0xBD -> (LDA, AbsoluteX,       4) -- +1 if page crossed
      0xB9 -> (LDA, AbsoluteY,       4) -- +1 if page crossed
      0xA1 -> (LDA, IndexedIndirect, 6)
      0xB1 -> (LDA, IndirectIndexed, 5) -- +1 if page crossed
      0xA2 -> (LDX, Immediate,       2)
      0xA6 -> (LDX, ZeroPage,        3)
      0xB6 -> (LDX, ZeroPageY,       4)
      0xAE -> (LDX, Absolute,        4)
      0xBE -> (LDX, AbsoluteY,       4) -- +1 if page crossed
      0xA0 -> (LDY, Immediate,       2)
      0xA4 -> (LDY, ZeroPage,        3)
      0xB4 -> (LDY, ZeroPageX,       4)
      0xAC -> (LDY, Absolute,        4)
      0xBC -> (LDY, AbsoluteX,       4) -- +1 if page crossed
      0x4A -> (LSR, Accumulator,     2)
      0x46 -> (LSR, ZeroPage,        5)
      0x56 -> (LSR, ZeroPageX,       6)
      0x4E -> (LSR, Absolute,        6)
      0x5E -> (LSR, AbsoluteX,       7)
      0xEA -> (NOP, Implicit,        2)
      0x09 -> (ORA, Immediate,       2)
      0x05 -> (ORA, ZeroPage,        3)
      0x15 -> (ORA, ZeroPageX,       4)
      0x0D -> (ORA, Absolute,        4)
      0x1D -> (ORA, AbsoluteX,       4) -- +1 if page crossed
      0x19 -> (ORA, AbsoluteY,       4) -- +1 if page crossed
      0x01 -> (ORA, IndexedIndirect, 6)
      0x11 -> (ORA, IndirectIndexed, 5) -- +1 if page crossed
      0x48 -> (PHA, Implicit,        3)
      0x08 -> (PHP, Implicit,        3)
      0x68 -> (PLA, Implicit,        4)
      0x28 -> (PLP, Implicit,        4)
      0x2A -> (ROL, Accumulator,     2)
      0x26 -> (ROL, ZeroPage,        5)
      0x36 -> (ROL, ZeroPageX,       6)
      0x2E -> (ROL, Absolute,        6)
      0x3E -> (ROL, AbsoluteX,       7)
      0x6A -> (ROR, Accumulator,     2)
      0x66 -> (ROR, ZeroPage,        5)
      0x76 -> (ROR, ZeroPageX,       6)
      0x6E -> (ROR, Absolute,        6)
      0x7E -> (ROR, AbsoluteX,       7)
      0x40 -> (RTI, Implicit,        6)
      0x60 -> (RTS, Implicit,        6)
      0xE9 -> (SBC, Immediate,       2)
      0xE5 -> (SBC, ZeroPage,        3)
      0xF5 -> (SBC, ZeroPageX,       4)
      0xED -> (SBC, Absolute,        4)
      0xFD -> (SBC, AbsoluteX,       4) -- +1 if page crossed
      0xF9 -> (SBC, AbsoluteY,       4) -- +1 if page crossed
      0xE1 -> (SBC, IndexedIndirect, 6)
      0xF1 -> (SBC, IndirectIndexed, 5) -- +1 if page crossed
      0x38 -> (SEC, Implicit,        2)
      0xF8 -> (SED, Implicit,        2)
      0x78 -> (SEI, Implicit,        2)
      0x85 -> (STA, ZeroPage,        3)
      0x95 -> (STA, ZeroPageX,       4)
      0x8D -> (STA, Absolute,        4)
      0x9D -> (STA, AbsoluteX,       5)
      0x99 -> (STA, AbsoluteY,       5)
      0x81 -> (STA, IndexedIndirect, 6)
      0x91 -> (STA, IndirectIndexed, 6)
      0x86 -> (STX, ZeroPage,        3)
      0x96 -> (STX, ZeroPageY,       4)
      0x8E -> (STX, Absolute,        4)
      0x84 -> (STY, ZeroPage,        4)
      0x94 -> (STY, ZeroPageX,       4)
      0x8C -> (STY, Absolute,        4)
      0xAA -> (TAX, Implicit,        2)
      0xA8 -> (TAY, Implicit,        2)
      0xBA -> (TSX, Implicit,        2)
      0x8A -> (TXA, Implicit,        2)
      0x9A -> (TXS, Implicit,        2)
      0x98 -> (TYA, Implicit,        2)
      _ -> error "invalid opCode"
