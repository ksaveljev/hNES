module NES.Instruction ( decodeOpCode
                       , AddressingMode(..)
                       , Cycles
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
              -- unofficial
              | ASO | RLA | LSE | RRA
              | AXS | LAX | DCM | INS
              | ALR | ARR | XAA | OAL
              | SAX | SKB | SKW | HLT
              | TAS | SAY | XAS | AXA
              | ANC | LAS
              deriving (Show)

data Instruction = Instruction OpCode Cycles Mnemonic AddressingMode [Word8]

instance Show Instruction where
    show (Instruction opcode _ mn am arg) = printf "%02X" opcode ++ " " ++ show mn ++ showArgument
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
      0x0B -> (ANC, Immediate,       2)
      0x2B -> (ANC, Immediate,       2)
      0x87 -> (AXS, ZeroPage,        3)
      0x97 -> (AXS, ZeroPageY,       4)
      0x83 -> (AXS, IndexedIndirect, 6)
      0x8F -> (AXS, Absolute,        4)
      0x6B -> (ARR, Immediate,       2)
      0x4B -> (ALR, Immediate,       2)
      0xAB -> (OAL, Immediate,       2)
      0x9F -> (AXA, AbsoluteY,       5)
      0x93 -> (AXA, IndirectIndexed, 6)
      0xCB -> (SAX, Immediate,       2)
      0xC7 -> (DCM, ZeroPage,        5)
      0xD7 -> (DCM, ZeroPageX,       6)
      0xCF -> (DCM, Absolute,        6)
      0xDF -> (DCM, AbsoluteX,       7)
      0xDB -> (DCM, AbsoluteY,       7)
      0xC3 -> (DCM, IndexedIndirect, 8)
      0xD3 -> (DCM, IndirectIndexed, 8)
      0x04 -> (SKB, ZeroPage,        3)
      0x14 -> (SKB, ZeroPageX,       4)
      0x34 -> (SKB, ZeroPageX,       4)
      0x44 -> (SKB, ZeroPage,        3)
      0x54 -> (SKB, ZeroPageX,       4)
      0x64 -> (SKB, ZeroPage,        3)
      0x74 -> (SKB, ZeroPageX,       4)
      0x80 -> (SKB, Immediate,       2)
      0x82 -> (SKB, Immediate,       2)
      0x89 -> (SKB, Immediate,       2)
      0xC2 -> (SKB, Immediate,       2)
      0xD4 -> (SKB, ZeroPageX,       4)
      0xE2 -> (SKB, Immediate,       2)
      0xF4 -> (SKB, ZeroPageX,       4)
      0xE7 -> (INS, ZeroPage,        5)
      0xF7 -> (INS, ZeroPageX,       6)
      0xEF -> (INS, Absolute,        6)
      0xFF -> (INS, AbsoluteX,       7)
      0xFB -> (INS, AbsoluteY,       7)
      0xE3 -> (INS, IndexedIndirect, 8)
      0xF3 -> (INS, IndirectIndexed, 8)
      0x02 -> (HLT, Implicit,        0)
      0x12 -> (HLT, Implicit,        0)
      0x22 -> (HLT, Implicit,        0)
      0x32 -> (HLT, Implicit,        0)
      0x42 -> (HLT, Implicit,        0)
      0x52 -> (HLT, Implicit,        0)
      0x62 -> (HLT, Implicit,        0)
      0x72 -> (HLT, Implicit,        0)
      0x92 -> (HLT, Implicit,        0)
      0xB2 -> (HLT, Implicit,        0)
      0xD2 -> (HLT, Implicit,        0)
      0xF2 -> (HLT, Implicit,        0)
      0xBB -> (LAS, AbsoluteY,       4) -- +1 if page crossed
      0xA7 -> (LAX, ZeroPage,        3)
      0xB7 -> (LAX, ZeroPageY,       4)
      0xAF -> (LAX, Absolute,        4)
      0xBF -> (LAX, AbsoluteY,       4) -- +1 if page crossed
      0xA3 -> (LAX, IndexedIndirect, 6)
      0xB3 -> (LAX, IndirectIndexed, 5) -- +1 if page crossed
      0x1A -> (NOP, Implicit,        2)
      0x3A -> (NOP, Implicit,        2)
      0x5A -> (NOP, Implicit,        2)
      0x7A -> (NOP, Implicit,        2)
      0xDA -> (NOP, Implicit,        2)
      0xFA -> (NOP, Implicit,        2)
      0x27 -> (RLA, ZeroPage,        5)
      0x37 -> (RLA, ZeroPageX,       6)
      0x2F -> (RLA, Absolute,        6)
      0x3F -> (RLA, AbsoluteX,       7)
      0x3B -> (RLA, AbsoluteY,       7)
      0x23 -> (RLA, IndexedIndirect, 8)
      0x33 -> (RLA, IndirectIndexed, 8)
      0x67 -> (RRA, ZeroPage,        5)
      0x77 -> (RRA, ZeroPageX,       6)
      0x6F -> (RRA, Absolute,        6)
      0x7F -> (RRA, AbsoluteX,       7)
      0x7B -> (RRA, AbsoluteY,       7)
      0x63 -> (RRA, IndexedIndirect, 8)
      0x73 -> (RRA, IndirectIndexed, 8)
      0xEB -> (SBC, Immediate,       2)
      0x07 -> (ASO, ZeroPage,        5)
      0x17 -> (ASO, ZeroPageX,       6)
      0x0F -> (ASO, Absolute,        6)
      0x1F -> (ASO, AbsoluteX,       7)
      0x1B -> (ASO, AbsoluteY,       7)
      0x03 -> (ASO, IndexedIndirect, 8)
      0x13 -> (ASO, IndirectIndexed, 8)
      0x47 -> (LSE, ZeroPage,        5)
      0x57 -> (LSE, ZeroPageX,       6)
      0x4F -> (LSE, Absolute,        6)
      0x5F -> (LSE, AbsoluteX,       7)
      0x5B -> (LSE, AbsoluteY,       7)
      0x43 -> (LSE, IndexedIndirect, 8)
      0x53 -> (LSE, IndirectIndexed, 8)
      0x9E -> (XAS, AbsoluteY,       5)
      0x9C -> (SAY, AbsoluteX,       5)
      0x0C -> (SKW, Absolute,        4)
      0x1C -> (SKW, AbsoluteX,       4) -- +1 if page crossed
      0x3C -> (SKW, AbsoluteX,       4) -- +1 if page crossed
      0x5C -> (SKW, AbsoluteX,       4) -- +1 if page crossed
      0x7C -> (SKW, AbsoluteX,       4) -- +1 if page crossed
      0xDC -> (SKW, AbsoluteX,       4) -- +1 if page crossed
      0xFC -> (SKW, AbsoluteX,       4) -- +1 if page crossed
      0x8B -> (XAA, Immediate,       2)
      0x9B -> (TAS, AbsoluteY,       5)
      _ -> error "invalid opCode"
