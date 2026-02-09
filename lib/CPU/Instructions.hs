module CPU.Instructions where

import Data.Int (Int8)
import Data.Word (Word16, Word8)
import Types (Registers (..))

data Instruction
  = ADC_A_R8 Registers
  | ADC_A_HL
  | ADC_A_N8 Word8
  | ADD_A_R8 Registers
  | ADD_A_HL
  | ADD_A_N8 Word8
  | ADD_HL_R16 Word16
  | ADD_HL_SP
  | ADD_SP_E8 Int8
  | SUB_A_R8 Registers
  | SUB_A_HL
  | SBC_A_R8 Registers
  | SBC_A_HL
  | AND_A_R8 Registers
  | AND_A_HL
  | XOR_A_R8 Registers
  | XOR_A_HL
  | OR_A_R8 Registers
  | OR_A_HL
  | CP_A_R8 Registers
  | CP_A_HL