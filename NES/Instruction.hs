module NES.Instruction ( Operand
                       , OpCodeView(..)
                       , OpCode
                       , viewOpCode
                       , Instruction(..)
                       ) where

import Data.Word (Word8)

type Operand = Word8

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

data OpCodeView = OpCode Operand Mnemonic AddressingMode
newtype OpCode = OpCodeC { viewOpCode :: OpCodeView }

data Instruction = Instruction OpCode [Operand]

decodeOpCode :: Word8 -> OpCode
decodeOpCode w =
    let {-# INLINE oc #-}
        oc = OpCode w
        in OpCodeC $ case w of
                       0x69 -> oc ADC Immediate
                       0x65 -> oc ADC ZeroPage
                       0x75 -> oc ADC ZeroPageX
                       0x6D -> oc ADC Absolute
                       0x7D -> oc ADC AbsoluteX
                       0x79 -> oc ADC AbsoluteY
                       0x61 -> oc ADC IndexedIndirect
                       0x71 -> oc ADC IndirectIndexed
                       0x29 -> oc AND Immediate
                       0x25 -> oc AND ZeroPage
                       0x35 -> oc AND ZeroPageX
                       0x2D -> oc AND Absolute
                       0x3D -> oc AND AbsoluteX
                       0x39 -> oc AND AbsoluteY
                       0x21 -> oc AND IndexedIndirect
                       0x31 -> oc AND IndirectIndexed
                       0x0A -> oc ASL Accumulator
                       0x06 -> oc ASL ZeroPage
                       0x16 -> oc ASL ZeroPageX
                       0x0E -> oc ASL Absolute
                       0x1E -> oc ASL AbsoluteX
                       0x90 -> oc BCC Relative
                       0xB0 -> oc BCS Relative
                       0xF0 -> oc BEQ Relative
                       0x24 -> oc BIT ZeroPage
                       0x2C -> oc BIT Absolute
                       0x30 -> oc BMI Relative
                       0xD0 -> oc BNE Relative
                       0x10 -> oc BPL Relative
                       0x00 -> oc BRK Implicit
                       0x50 -> oc BVC Relative
                       0x70 -> oc BVS Relative
                       0x18 -> oc CLC Implicit
                       0xD8 -> oc CLD Implicit
                       0x58 -> oc CLI Implicit
                       0xB8 -> oc CLV Implicit
                       0xC9 -> oc CMP Immediate
                       0xC5 -> oc CMP ZeroPage
                       0xD5 -> oc CMP ZeroPageX
                       0xCD -> oc CMP Absolute
                       0xDD -> oc CMP AbsoluteX
                       0xD9 -> oc CMP AbsoluteY
                       0xC1 -> oc CMP IndexedIndirect
                       0xD1 -> oc CMP IndirectIndexed
                       0xE0 -> oc CPX Immediate
                       0xE4 -> oc CPX ZeroPage
                       0xEC -> oc CPX Absolute
                       0xC0 -> oc CPY Immediate
                       0xC4 -> oc CPY ZeroPage
                       0xCC -> oc CPY Absolute
                       0xC6 -> oc DEC ZeroPage
                       0xD6 -> oc DEC ZeroPageX
                       0xCE -> oc DEC Absolute
                       0xDE -> oc DEC AbsoluteX
                       0xCA -> oc DEX Implicit
                       0x88 -> oc DEY Implicit
                       0x49 -> oc EOR Immediate
                       0x45 -> oc EOR ZeroPage
                       0x55 -> oc EOR ZeroPageX
                       0x4D -> oc EOR Absolute
                       0x5D -> oc EOR AbsoluteX
                       0x59 -> oc EOR AbsoluteY
                       0x41 -> oc EOR IndexedIndirect
                       0x51 -> oc EOR IndirectIndexed
                       0xE6 -> oc INC ZeroPage
                       0xF6 -> oc INC ZeroPageX
                       0xEE -> oc INC Absolute
                       0xFE -> oc INC AbsoluteX
                       0xE8 -> oc INX Implicit
                       0xC8 -> oc INY Implicit
                       0x4C -> oc JMP Absolute
                       0x6C -> oc JMP Indirect
                       0x20 -> oc JSR Absolute
                       0xA9 -> oc LDA Immediate
                       0xA5 -> oc LDA ZeroPage
                       0xB5 -> oc LDA ZeroPageX
                       0xAD -> oc LDA Absolute
                       0xBD -> oc LDA AbsoluteX
                       0xB9 -> oc LDA AbsoluteY
                       0xA1 -> oc LDA IndexedIndirect
                       0xB1 -> oc LDA IndirectIndexed
                       0xA2 -> oc LDX Immediate
                       0xA6 -> oc LDX ZeroPage
                       0xB6 -> oc LDX ZeroPageY
                       0xAE -> oc LDX Absolute
                       0xBE -> oc LDX AbsoluteY
                       0xA0 -> oc LDY Immediate
                       0xA4 -> oc LDY ZeroPage
                       0xB4 -> oc LDY ZeroPageX
                       0xAC -> oc LDY Absolute
                       0xBC -> oc LDY AbsoluteX
                       0x4A -> oc LSR Accumulator
                       0x46 -> oc LSR ZeroPage
                       0x56 -> oc LSR ZeroPageX
                       0x4E -> oc LSR Absolute
                       0x5E -> oc LSR AbsoluteX
                       0xEA -> oc NOP Implicit
                       0x09 -> oc ORA Immediate
                       0x05 -> oc ORA ZeroPage
                       0x15 -> oc ORA ZeroPageX
                       0x0D -> oc ORA Absolute
                       0x1D -> oc ORA AbsoluteX
                       0x19 -> oc ORA AbsoluteY
                       0x01 -> oc ORA IndexedIndirect
                       0x11 -> oc ORA IndirectIndexed
                       0x48 -> oc PHA Implicit
                       0x08 -> oc PHP Implicit
                       0x68 -> oc PLA Implicit
                       0x28 -> oc PLP Implicit
                       0x2A -> oc ROL Accumulator
                       0x26 -> oc ROL ZeroPage
                       0x36 -> oc ROL ZeroPageX
                       0x2E -> oc ROL Absolute
                       0x3E -> oc ROL AbsoluteX
                       0x6A -> oc ROR Accumulator
                       0x66 -> oc ROR ZeroPage
                       0x76 -> oc ROR ZeroPageX
                       0x6E -> oc ROR Absolute
                       0x7E -> oc ROR AbsoluteX
                       0x40 -> oc RTI Implicit
                       0x60 -> oc RTS Implicit
                       0xE9 -> oc SBC Immediate
                       0xE5 -> oc SBC ZeroPage
                       0xF5 -> oc SBC ZeroPageX
                       0xED -> oc SBC Absolute
                       0xFD -> oc SBC AbsoluteX
                       0xF9 -> oc SBC AbsoluteY
                       0xE1 -> oc SBC IndexedIndirect
                       0xF1 -> oc SBC IndirectIndexed
                       0x38 -> oc SEC Implicit
                       0xF8 -> oc SED Implicit
                       0x78 -> oc SEI Implicit
                       0x85 -> oc STA ZeroPage
                       0x95 -> oc STA ZeroPageX
                       0x8D -> oc STA Absolute
                       0x9D -> oc STA AbsoluteX
                       0x99 -> oc STA AbsoluteY
                       0x81 -> oc STA IndexedIndirect
                       0x91 -> oc STA IndirectIndexed
                       0x86 -> oc STX ZeroPage
                       0x96 -> oc STX ZeroPageY
                       0x8E -> oc STX Absolute
                       0x84 -> oc STY ZeroPage
                       0x94 -> oc STY ZeroPageX
                       0x8C -> oc STY Absolute
                       0xAA -> oc TAX Implicit
                       0xA8 -> oc TAY Implicit
                       0xBA -> oc TSX Implicit
                       0x8A -> oc TXA Implicit
                       0x9A -> oc TXS Implicit
                       0x98 -> oc TYA Implicit
                       _ -> error "Incorrect opCode"
