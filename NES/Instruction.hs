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
