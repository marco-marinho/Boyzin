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
  | ADD_HL_N16 Word16
  | ADD_HL_R16 Registers16
  | ADD_SP_E8 Int8
  | SUB_A_R8 Registers
  | SUB_A_HL_REF
  | SUB_A_N8 Word8
  | SBC_A_R8 Registers
  | SBC_A_N8 Word8
  | SBC_A_HL_REF
  | AND_A_R8 Registers
  | AND_A_N8 Word8
  | AND_A_HL_REF
  | XOR_A_R8 Registers
  | XOR_A_N8 Word8
  | XOR_A_HL_REF
  | OR_A_R8 Registers
  | OR_A_N8 Word8
  | OR_A_HL_REF
  | CP_A_R8 Registers
  | CP_A_N8 Word8
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
  | DAA
  | JR_Z_E8 Int8
  | LD_A_HLI
  | CPL
  | STOP
  | LD_R16_REF_A Registers16
  | RLA
  | JR_E8 Int8
  | LD_A_R16_REF Registers16
  | RRA
  | NOP
  | RLCA
  | LD_N16_REF_SP Word16
  | RRCA
  | RET_NZ
  | POP_R16 Registers16
  | JP_NZ_N16 Word16
  | JP_N16 Word16
  | CALL_NZ_N16 Word16
  | PUSH_R16 Registers16
  | RST Word8
  | RET_Z
  | RET
  | JP_Z_N16 Word16
  | RLC_R8 Registers
  | RLC_HL_REF
  | RRC_R8 Registers
  | RRC_HL_REF
  | RL_R8 Registers
  | RL_HL_REF
  | RR_R8 Registers
  | RR_HL_REF
  | SLA_R8 Registers
  | SLA_HL_REF
  | SRA_R8 Registers
  | SRA_HL_REF
  | SWAP_R8 Registers
  | SWAP_HL_REF
  | SRL_R8 Registers
  | SRL_HL_REF
  | BIT_U3_R8 Int Registers
  | BIT_U3_HL_REF Int
  | RES_U3_R8 Int Registers
  | RES_U3_HL_REF Int
  | SET_U3_R8 Int Registers
  | SET_U3_HL_REF Int
  | CALL_Z_N16 Word16
  | CALL_N16 Word16
  | RET_NC
  | JP_NC_N16 Word16
  | CALL_NC_N16 Word16
  | RET_C
  | RETI
  | JP_C_N16 Word16
  | CALL_C_N16 Word16
  | LDH_N8_REF_A Word8
  | LDH_C_A
  | JP_HL
  | LD_N16_REF_A Word16
  | LDH_A_N8_REF Word8
  | LDH_A_C
  | DI
  | LD_HL_SP_E8 Int8
  | LD_SP_HL
  | LD_A_N16_REF Word16
  | EI