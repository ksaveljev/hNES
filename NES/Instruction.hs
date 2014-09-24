module NES.Instruction where

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
    let o = OpCode w
        in OpCodeC $ case w of
                       0x69 -> o ADC Immediate
                       0x65 -> o ADC ZeroPage
                       0x75 -> o ADC ZeroPageX
                       0x6D -> o ADC Absolute
                       0x7D -> o ADC AbsoluteX
                       0x79 -> o ADC AbsoluteY
                       0x61 -> o ADC IndexedIndirect
                       0x71 -> o ADC IndirectIndexed
                       0x29 -> o AND Immediate
                       0x25 -> o AND ZeroPage
                       0x35 -> o AND ZeroPageX
                       0x2D -> o AND Absolute
                       0x3D -> o AND AbsoluteX
                       0x39 -> o AND AbsoluteY
                       0x21 -> o AND IndexedIndirect
                       0x31 -> o AND IndirectIndexed
                       0x0A -> o ASL Accumulator
                       0x06 -> o ASL ZeroPage
                       0x16 -> o ASL ZeroPageX
                       0x0E -> o ASL Absolute
                       0x1E -> o ASL AbsoluteX
                       0x90 -> o BCC Relative
                       0xB0 -> o BCS Relative
                       0xF0 -> o BEQ Relative
                       0x24 -> o BIT ZeroPage
                       0x2C -> o BIT Absolute
                       0x30 -> o BMI Relative
                       0xD0 -> o BNE Relative
                       0x10 -> o BPL Relative
                       0x00 -> o BRK Implicit
                       0x50 -> o BVC Relative
                       0x70 -> o BVS Relative
                       0x18 -> o CLC Implicit
                       0xD8 -> o CLD Implicit
                       0x58 -> o CLI Implicit
                       0xB8 -> o CLV Implicit
                       0xC9 -> o CMP Immediate
                       0xC5 -> o CMP ZeroPage
                       0xD5 -> o CMP ZeroPageX
                       0xCD -> o CMP Absolute
                       0xDD -> o CMP AbsoluteX
                       0xD9 -> o CMP AbsoluteY
                       0xC1 -> o CMP IndexedIndirect
                       0xD1 -> o CMP IndirectIndexed
                       0xE0 -> o CPX Immediate
                       0xE4 -> o CPX ZeroPage
                       0xEC -> o CPX Absolute
                       0xC0 -> o CPY Immediate
                       0xC4 -> o CPY ZeroPage
                       0xCC -> o CPY Absolute
                       0xC6 -> o DEC ZeroPage
                       0xD6 -> o DEC ZeroPageX
                       0xCE -> o DEC Absolute
                       0xDE -> o DEC AbsoluteX
                       0xCA -> o DEX Implicit
                       0x88 -> o DEY Implicit
                       0x49 -> o EOR Immediate
                       0x45 -> o EOR ZeroPage
                       0x55 -> o EOR ZeroPageX
                       0x4D -> o EOR Absolute
                       0x5D -> o EOR AbsoluteX
                       0x59 -> o EOR AbsoluteY
                       0x41 -> o EOR IndexedIndirect
                       0x51 -> o EOR IndirectIndexed
                       0xE6 -> o INC ZeroPage
                       0xF6 -> o INC ZeroPageX
                       0xEE -> o INC Absolute
                       0xFE -> o INC AbsoluteX
                       0xE8 -> o INX Implicit
                       0xC8 -> o INY Implicit
                       0x4C -> o JMP Absolute
                       0x6C -> o JMP Indirect
                       0x20 -> o JSR Absolute
                       0xA9 -> o LDA Immediate
                       0xA5 -> o LDA ZeroPage
                       0xB5 -> o LDA ZeroPageX
                       0xAD -> o LDA Absolute
                       0xBD -> o LDA AbsoluteX
                       0xB9 -> o LDA AbsoluteY
                       0xA1 -> o LDA IndexedIndirect
                       0xB1 -> o LDA IndirectIndexed
                       0xA2 -> o LDX Immediate
                       0xA6 -> o LDX ZeroPage
                       0xB6 -> o LDX ZeroPageY
                       0xAE -> o LDX Absolute
                       0xBE -> o LDX AbsoluteY
                       0xA0 -> o LDY Immediate
                       0xA4 -> o LDY ZeroPage
                       0xB4 -> o LDY ZeroPageX
                       0xAC -> o LDY Absolute
                       0xBC -> o LDY AbsoluteX
                       0x4A -> o LSR Accumulator
                       0x46 -> o LSR ZeroPage
                       0x56 -> o LSR ZeroPageX
                       0x4E -> o LSR Absolute
                       0x5E -> o LSR AbsoluteX
                       0xEA -> o NOP Implicit
                       0x09 -> o ORA Immediate
                       0x05 -> o ORA ZeroPage
                       0x15 -> o ORA ZeroPageX
                       0x0D -> o ORA Absolute
                       0x1D -> o ORA AbsoluteX
                       0x19 -> o ORA AbsoluteY
                       0x01 -> o ORA IndexedIndirect
                       0x11 -> o ORA IndirectIndexed
                       0x48 -> o PHA Implicit
                       0x08 -> o PHP Implicit
                       0x68 -> o PLA Implicit
                       0x28 -> o PLP Implicit
                       0x2A -> o ROL Accumulator
                       0x26 -> o ROL ZeroPage
                       0x36 -> o ROL ZeroPageX
                       0x2E -> o ROL Absolute
                       0x3E -> o ROL AbsoluteX
                       0x6A -> o ROR Accumulator
                       0x66 -> o ROR ZeroPage
                       0x76 -> o ROR ZeroPageX
                       0x6E -> o ROR Absolute
                       0x7E -> o ROR AbsoluteX
                       0x40 -> o RTI Implicit
                       0x60 -> o RTS Implicit
                       0xE9 -> o SBC Immediate
                       0xE5 -> o SBC ZeroPage
                       0xF5 -> o SBC ZeroPageX
                       0xED -> o SBC Absolute
                       0xFD -> o SBC AbsoluteX
                       0xF9 -> o SBC AbsoluteY
                       0xE1 -> o SBC IndexedIndirect
                       0xF1 -> o SBC IndirectIndexed
                       0x38 -> o SEC Implicit
                       0xF8 -> o SED Implicit
                       0x78 -> o SEI Implicit
                       0x85 -> o STA ZeroPage
                       0x95 -> o STA ZeroPageX
                       0x8D -> o STA Absolute
                       0x9D -> o STA AbsoluteX
                       0x99 -> o STA AbsoluteY
                       0x81 -> o STA IndexedIndirect
                       0x91 -> o STA IndirectIndexed
                       0x86 -> o STX ZeroPage
                       0x96 -> o STX ZeroPageY
                       0x8E -> o STX Absolute
                       0x84 -> o STY ZeroPage
                       0x94 -> o STY ZeroPageX
                       0x8C -> o STY Absolute
                       0xAA -> o TAX Implicit
                       0xA8 -> o TAY Implicit
                       0xBA -> o TSX Implicit
                       0x8A -> o TXA Implicit
                       0x9A -> o TXS Implicit
                       0x98 -> o TYA Implicit
                       _ -> error "Incorrect opCode"
