module CPU.Instructions where

import Data.Int (Int8)
import Data.Word (Word16, Word8)
import Types (Registers (..), Registers16)

data Instruction
  = ADC_A_R8 Registers
  | ADC_A_HL_REF
  | ADC_A_N8 Word8
  | ADD_A_R8 Registers
  | ADD_A_HL_REF
  | ADD_A_N8 Word8
  | ADD_HL_R16 Word16
  | ADD_HL_SP
  | ADD_SP_E8 Int8
  | SUB_A_R8 Registers
  | SUB_A_HL_REF
  | SBC_A_R8 Registers
  | SBC_A_HL_REF
  | AND_A_R8 Registers
  | AND_A_HL_REF
  | XOR_A_R8 Registers
  | XOR_A_HL_REF
  | OR_A_R8 Registers
  | OR_A_HL_REF
  | CP_A_R8 Registers
  | CP_A_HL_REF
  | LD_R8_R8 Registers Registers
  | LD_R8_HL Registers
  | LD_HL_R8 Registers
  | INC_HL_REF
  | DEC_HL_REF
  | HALT
  | JR_NC_E8 Int8
  | LD_R16_N16 Registers16 Word16
  | LD_HLD_A
  | INC_R16 Registers16
  | DEC_R16 Registers16
  | LD_HL_N8 Word8
  | SCF
  | JR_C_E8 Int8
  | LD_A_HLD
  | DEC_SP
  | INC_R8 Registers
  | DEC_R8 Registers
  | LD_R8_N8 Registers Word8
  | CCF
  | JR_NZ_E8 Int8
  | LD_HLI_A