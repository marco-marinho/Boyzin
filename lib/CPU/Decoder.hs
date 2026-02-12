module CPU.Decoder where

import CPU.Instructions (Instruction (..))
import CPU.Interface (readMemory, readPC)
import Data.Bits (Bits (shiftL, (.|.)))
import Data.Word (Word16)
import Types
  ( Cpu,
    Registers (RegA, RegB, RegC, RegD, RegE, RegH, RegL),
    Registers16 (RegBC, RegDE, RegHL, RegSP),
  )

read16Bits :: Cpu -> Word16 -> IO Word16
read16Bits cpu addr = do
  low <- readMemory cpu addr
  high <- readMemory cpu (addr + 1)
  return $ (fromIntegral high `shiftL` 8) .|. fromIntegral low

decodeInstruction :: Cpu -> IO Instruction
decodeInstruction cpu = do
  pcValue <- readPC cpu
  opcode <- readMemory cpu pcValue
  case opcode of
    0x00 -> return NOP
    0x01 -> do
      value <- read16Bits cpu (pcValue + 1)
      return $ LD_R16_N16 RegBC value
    0x02 -> return $ LD_R16_REF_A RegBC
    0x03 -> return $ INC_R16 RegBC
    0x04 -> return $ INC_R8 RegB
    0x05 -> return $ DEC_R8 RegB
    0x06 -> do
      value <- readMemory cpu (pcValue + 1)
      return $ LD_R8_N8 RegB value
    0x07 -> return RLCA
    0x08 -> do
      offset <- read16Bits cpu (pcValue + 1)
      return $ LD_N16_REF_SP offset
    0x09 -> return $ ADD_HL_R16 RegBC
    0x0A -> return $ LD_A_R16_REF RegBC
    0x0B -> return $ DEC_R16 RegBC
    0x0C -> return $ INC_R8 RegC
    0x0D -> return $ DEC_R8 RegC
    0x0E -> do
      value <- readMemory cpu (pcValue + 1)
      return $ LD_R8_N8 RegC value
    0x0F -> return RRCA
    0x10 -> return STOP
    0x11 -> do
      value <- read16Bits cpu (pcValue + 1)
      return $ LD_R16_N16 RegDE value
    0x12 -> return $ LD_R16_REF_A RegDE
    0x13 -> return $ INC_R16 RegDE
    0x14 -> return $ INC_R8 RegD
    0x15 -> return $ DEC_R8 RegD
    0x16 -> do
      value <- readMemory cpu (pcValue + 1)
      return $ LD_R8_N8 RegD value
    0x17 -> return RLA
    0x18 -> do
      offset <- readMemory cpu (pcValue + 1)
      return $ JR_E8 (fromIntegral offset)
    0x19 -> return $ ADD_HL_R16 RegDE
    0x1A -> return $ LD_A_R16_REF RegDE
    0x1B -> return $ DEC_R16 RegDE
    0x1C -> return $ INC_R8 RegE
    0x1D -> return $ DEC_R8 RegE
    0x1E -> do
      value <- readMemory cpu (pcValue + 1)
      return $ LD_R8_N8 RegE value
    0x1F -> return RRA
    0x20 -> do
      offset <- readMemory cpu (pcValue + 1)
      return $ JR_NZ_E8 (fromIntegral offset)
    0x21 -> do
      value <- read16Bits cpu (pcValue + 1)
      return $ LD_R16_N16 RegHL value
    0x22 -> return LD_HLI_A
    0x23 -> return $ INC_R16 RegHL
    0x24 -> return $ INC_R8 RegH
    0x25 -> return $ DEC_R8 RegH
    0x26 -> do
      value <- readMemory cpu (pcValue + 1)
      return $ LD_R8_N8 RegH value
    0x27 -> return DAA
    0x28 -> do
      offset <- readMemory cpu (pcValue + 1)
      return $ JR_Z_E8 (fromIntegral offset)
    0x29 -> return $ ADD_HL_R16 RegHL
    0x2A -> return LD_A_HLI
    0x2B -> return $ DEC_R16 RegHL
    0x2C -> return $ INC_R8 RegL
    0x2D -> return $ DEC_R8 RegL
    0x2E -> do
      value <- readMemory cpu (pcValue + 1)
      return $ LD_R8_N8 RegL value
    0x2F -> return CPL
    0x30 -> do
      offset <- readMemory cpu (pcValue + 1)
      return $ JR_NC_E8 (fromIntegral offset)
    0x31 -> do
      value <- read16Bits cpu (pcValue + 1)
      return $ LD_R16_N16 RegSP value
    0x32 -> return LD_HLD_A
    0x33 -> return $ INC_R16 RegSP
    0x34 -> return INC_HL_REF
    0x35 -> return DEC_HL_REF
    0x36 -> do
      value <- readMemory cpu (pcValue + 1)
      return $ LD_HL_N8 value
    0x37 -> return SCF
    0x38 -> do
      offset <- readMemory cpu (pcValue + 1)
      return $ JR_C_E8 (fromIntegral offset)
    0x39 -> return $ ADD_HL_R16 RegSP
    0x3A -> return LD_A_HLD
    0x3B -> return $ DEC_R16 RegSP
    0x3C -> return $ INC_R8 RegA
    0x3D -> return $ DEC_R8 RegA
    0x3E -> do
      value <- readMemory cpu (pcValue + 1)
      return $ LD_R8_N8 RegA value
    0x3F -> return CCF
    0x40 -> return $ LD_R8_R8 RegB RegB
    0x41 -> return $ LD_R8_R8 RegB RegC
    0x42 -> return $ LD_R8_R8 RegB RegD
    0x43 -> return $ LD_R8_R8 RegB RegE
    0x44 -> return $ LD_R8_R8 RegB RegH
    0x45 -> return $ LD_R8_R8 RegB RegL
    0x46 -> return $ LD_R8_HL RegB
    0x47 -> return $ LD_R8_R8 RegB RegA
    0x48 -> return $ LD_R8_R8 RegC RegB
    0x49 -> return $ LD_R8_R8 RegC RegC
    0x4A -> return $ LD_R8_R8 RegC RegD
    0x4B -> return $ LD_R8_R8 RegC RegE
    0x4C -> return $ LD_R8_R8 RegC RegH
    0x4D -> return $ LD_R8_R8 RegC RegL
    0x4E -> return $ LD_R8_HL RegC
    0x4F -> return $ LD_R8_R8 RegC RegA
    0x50 -> return $ LD_R8_R8 RegD RegB
    0x51 -> return $ LD_R8_R8 RegD RegC
    0x52 -> return $ LD_R8_R8 RegD RegD
    0x53 -> return $ LD_R8_R8 RegD RegE
    0x54 -> return $ LD_R8_R8 RegD RegH
    0x55 -> return $ LD_R8_R8 RegD RegL
    0x56 -> return $ LD_R8_HL RegD
    0x57 -> return $ LD_R8_R8 RegD RegA
    0x58 -> return $ LD_R8_R8 RegE RegB
    0x59 -> return $ LD_R8_R8 RegE RegC
    0x5A -> return $ LD_R8_R8 RegE RegD
    0x5B -> return $ LD_R8_R8 RegE RegE
    0x5C -> return $ LD_R8_R8 RegE RegH
    0x5D -> return $ LD_R8_R8 RegE RegL
    0x5E -> return $ LD_R8_HL RegE
    0x5F -> return $ LD_R8_R8 RegE RegA
    0x60 -> return $ LD_R8_R8 RegH RegB
    0x61 -> return $ LD_R8_R8 RegH RegC
    0x62 -> return $ LD_R8_R8 RegH RegD
    0x63 -> return $ LD_R8_R8 RegH RegE
    0x64 -> return $ LD_R8_R8 RegH RegH
    0x65 -> return $ LD_R8_R8 RegH RegL
    0x66 -> return $ LD_R8_HL RegH
    0x67 -> return $ LD_R8_R8 RegH RegA
    0x68 -> return $ LD_R8_R8 RegL RegB
    0x69 -> return $ LD_R8_R8 RegL RegC
    0x6A -> return $ LD_R8_R8 RegL RegD
    0x6B -> return $ LD_R8_R8 RegL RegE
    0x6C -> return $ LD_R8_R8 RegL RegH
    0x6D -> return $ LD_R8_R8 RegL RegL
    0x6E -> return $ LD_R8_HL RegL
    0x6F -> return $ LD_R8_R8 RegL RegA
    0x70 -> return $ LD_HL_R8 RegB
    0x71 -> return $ LD_HL_R8 RegC
    0x72 -> return $ LD_HL_R8 RegD
    0x73 -> return $ LD_HL_R8 RegE
    0x74 -> return $ LD_HL_R8 RegH
    0x75 -> return $ LD_HL_R8 RegL
    0x76 -> return HALT
    0x77 -> return $ LD_HL_R8 RegA
    0x78 -> return $ LD_R8_R8 RegA RegB
    0x79 -> return $ LD_R8_R8 RegA RegC
    0x7A -> return $ LD_R8_R8 RegA RegD
    0x7B -> return $ LD_R8_R8 RegA RegE
    0x7C -> return $ LD_R8_R8 RegA RegH
    0x7D -> return $ LD_R8_R8 RegA RegL
    0x7E -> return $ LD_R8_HL RegA
    0x7F -> return $ LD_R8_R8 RegA RegA
    0x80 -> return $ ADD_A_R8 RegB
    0x81 -> return $ ADD_A_R8 RegC
    0x82 -> return $ ADD_A_R8 RegD
    0x83 -> return $ ADD_A_R8 RegE
    0x84 -> return $ ADD_A_R8 RegH
    0x85 -> return $ ADD_A_R8 RegL
    0x86 -> return ADD_A_HL_REF
    0x87 -> return $ ADD_A_R8 RegA
    0x88 -> return $ ADC_A_R8 RegB
    0x89 -> return $ ADC_A_R8 RegC
    0x8A -> return $ ADC_A_R8 RegD
    0x8B -> return $ ADC_A_R8 RegE
    0x8C -> return $ ADC_A_R8 RegH
    0x8D -> return $ ADC_A_R8 RegL
    0x8E -> return ADC_A_HL_REF
    0x8F -> return $ ADC_A_R8 RegA
    0x90 -> return $ SUB_A_R8 RegB
    0x91 -> return $ SUB_A_R8 RegC
    0x92 -> return $ SUB_A_R8 RegD
    0x93 -> return $ SUB_A_R8 RegE
    0x94 -> return $ SUB_A_R8 RegH
    0x95 -> return $ SUB_A_R8 RegL
    0x96 -> return SUB_A_HL_REF
    0x97 -> return $ SUB_A_R8 RegA
    0x98 -> return $ SBC_A_R8 RegB
    0x99 -> return $ SBC_A_R8 RegC
    0x9A -> return $ SBC_A_R8 RegD
    0x9B -> return $ SBC_A_R8 RegE
    0x9C -> return $ SBC_A_R8 RegH
    0x9D -> return $ SBC_A_R8 RegL
    0x9E -> return SBC_A_HL_REF
    0x9F -> return $ SBC_A_R8 RegA
    0xA0 -> return $ AND_A_R8 RegB
    0xA1 -> return $ AND_A_R8 RegC
    0xA2 -> return $ AND_A_R8 RegD
    0xA3 -> return $ AND_A_R8 RegE
    0xA4 -> return $ AND_A_R8 RegH
    0xA5 -> return $ AND_A_R8 RegL
    0xA6 -> return AND_A_HL_REF
    0xA7 -> return $ AND_A_R8 RegA
    0xA8 -> return $ XOR_A_R8 RegB
    0xA9 -> return $ XOR_A_R8 RegC
    0xAA -> return $ XOR_A_R8 RegD
    0xAB -> return $ XOR_A_R8 RegE
    0xAC -> return $ XOR_A_R8 RegH
    0xAD -> return $ XOR_A_R8 RegL
    0xAE -> return XOR_A_HL_REF
    0xAF -> return $ XOR_A_R8 RegA
    0xB0 -> return $ OR_A_R8 RegB
    0xB1 -> return $ OR_A_R8 RegC
    0xB2 -> return $ OR_A_R8 RegD
    0xB3 -> return $ OR_A_R8 RegE
    0xB4 -> return $ OR_A_R8 RegH
    0xB5 -> return $ OR_A_R8 RegL
    0xB6 -> return OR_A_HL_REF
    0xB7 -> return $ OR_A_R8 RegA
    0xB8 -> return $ CP_A_R8 RegB
    0xB9 -> return $ CP_A_R8 RegC
    0xBA -> return $ CP_A_R8 RegD
    0xBB -> return $ CP_A_R8 RegE
    0xBC -> return $ CP_A_R8 RegH
    0xBD -> return $ CP_A_R8 RegL
    0xBE -> return CP_A_HL_REF
    0xBF -> return $ CP_A_R8 RegA
    0xC0 -> return RET_NZ
    0xC1 -> return $ POP_R16 RegBC
    0xC2 -> do
      address <- read16Bits cpu (pcValue + 1)
      return $ JP_NZ_N16 address
    0xC3 -> do
      address <- read16Bits cpu (pcValue + 1)
      return $ JP_N16 address
    0xC4 -> do
      address <- read16Bits cpu (pcValue + 1)
      return $ CALL_NZ_N16 address
    0xC5 -> return $ PUSH_R16 RegBC
    0xC6 -> do
      value <- readMemory cpu (pcValue + 1)
      return $ ADD_A_N8 value
    0xC7 -> return $ RST 0x00
    0xC8 -> return RET_Z
    0xC9 -> return RET
    0xCA -> do
      address <- read16Bits cpu (pcValue + 1)
      return $ JP_Z_N16 address
    0xCB -> decodePrefixed cpu
    0xCC -> do
      address <- read16Bits cpu (pcValue + 1)
      return $ CALL_Z_N16 address
    0xCD -> do
      address <- read16Bits cpu (pcValue + 1)
      return $ CALL_N16 address
    0xCE -> do
      value <- readMemory cpu (pcValue + 1)
      return $ ADC_A_N8 value
    0xCF -> return $ RST 0x08
    0xD0 -> return RET_NC
    0xD1 -> return $ POP_R16 RegDE
    0xD2 -> do
      address <- read16Bits cpu (pcValue + 1)
      return $ JP_NC_N16 address
    0xD3 -> error "Invalid opcode: 0xD3"
    0xD4 -> do
      address <- read16Bits cpu (pcValue + 1)
      return $ CALL_NC_N16 address
    0xD5 -> return $ PUSH_R16 RegDE
    0xD6 -> do
      value <- readMemory cpu (pcValue + 1)
      return $ SUB_A_N8 value
    0xD7 -> return $ RST 0x10
    0xD8 -> return RET_C
    0xD9 -> return RETI
    0xDA -> do
      address <- read16Bits cpu (pcValue + 1)
      return $ JP_C_N16 address
    0xDB -> error "Invalid opcode: 0xDB"
    0xDC -> do
      address <- read16Bits cpu (pcValue + 1)
      return $ CALL_C_N16 address
    0xDD -> error "Invalid opcode: 0xDD"
    0xDE -> do
      value <- readMemory cpu (pcValue + 1)
      return $ SBC_A_N8 value
    0xDF -> return $ RST 0x18
    0xE0 -> do
      offset <- readMemory cpu (pcValue + 1)
      return $ LDH_N8_REF_A offset
    0xE1 -> return $ POP_R16 RegHL
    0xE2 -> do
      c <- readMemory cpu (pcValue + 1)
      return $ LDH_C_A c
    _ -> error $ "Unknown opcode: " ++ show opcode

decodePrefixed :: Cpu -> IO Instruction
decodePrefixed cpu = do
  pcValue <- readPC cpu
  opcode <- readMemory cpu (pcValue + 1)
  case opcode of
    0x00 -> return $ RLC_R8 RegB
    0x01 -> return $ RLC_R8 RegC
    0x02 -> return $ RLC_R8 RegD
    0x03 -> return $ RLC_R8 RegE
    0x04 -> return $ RLC_R8 RegH
    0x05 -> return $ RLC_R8 RegL
    0x06 -> return RLC_HL_REF
    0x07 -> return $ RLC_R8 RegA
    0x08 -> return $ RRC_R8 RegB
    0x09 -> return $ RRC_R8 RegC
    0x0A -> return $ RRC_R8 RegD
    0x0B -> return $ RRC_R8 RegE
    0x0C -> return $ RRC_R8 RegH
    0x0D -> return $ RRC_R8 RegL
    0x0E -> return RRC_HL_REF
    0x0F -> return $ RRC_R8 RegA
    0x10 -> return $ RL_R8 RegB
    0x11 -> return $ RL_R8 RegC
    0x12 -> return $ RL_R8 RegD
    0x13 -> return $ RL_R8 RegE
    0x14 -> return $ RL_R8 RegH
    0x15 -> return $ RL_R8 RegL
    0x16 -> return RL_HL_REF
    0x17 -> return $ RL_R8 RegA
    0x18 -> return $ RR_R8 RegB
    0x19 -> return $ RR_R8 RegC
    0x1A -> return $ RR_R8 RegD
    0x1B -> return $ RR_R8 RegE
    0x1C -> return $ RR_R8 RegH
    0x1D -> return $ RR_R8 RegL
    0x1E -> return RR_HL_REF
    0x1F -> return $ RR_R8 RegA
    0x20 -> return $ SLA_R8 RegB
    0x21 -> return $ SLA_R8 RegC
    0x22 -> return $ SLA_R8 RegD
    0x23 -> return $ SLA_R8 RegE
    0x24 -> return $ SLA_R8 RegH
    0x25 -> return $ SLA_R8 RegL
    0x26 -> return SLA_HL_REF
    0x27 -> return $ SLA_R8 RegA
    0x28 -> return $ SRA_R8 RegB
    0x29 -> return $ SRA_R8 RegC
    0x2A -> return $ SRA_R8 RegD
    0x2B -> return $ SRA_R8 RegE
    0x2C -> return $ SRA_R8 RegH
    0x2D -> return $ SRA_R8 RegL
    0x2E -> return SRA_HL_REF
    0x2F -> return $ SRA_R8 RegA
    0x30 -> return $ SWAP_R8 RegB
    0x31 -> return $ SWAP_R8 RegC
    0x32 -> return $ SWAP_R8 RegD
    0x33 -> return $ SWAP_R8 RegE
    0x34 -> return $ SWAP_R8 RegH
    0x35 -> return $ SWAP_R8 RegL
    0x36 -> return SWAP_HL_REF
    0x37 -> return $ SWAP_R8 RegA
    0x38 -> return $ SRL_R8 RegB
    0x39 -> return $ SRL_R8 RegC
    0x3A -> return $ SRL_R8 RegD
    0x3B -> return $ SRL_R8 RegE
    0x3C -> return $ SRL_R8 RegH
    0x3D -> return $ SRL_R8 RegL
    0x3E -> return SRL_HL_REF
    0x3F -> return $ SRL_R8 RegA
    0x40 -> return $ BIT_U3_R8 0 RegB
    0x41 -> return $ BIT_U3_R8 0 RegC
    0x42 -> return $ BIT_U3_R8 0 RegD
    0x43 -> return $ BIT_U3_R8 0 RegE
    0x44 -> return $ BIT_U3_R8 0 RegH
    0x45 -> return $ BIT_U3_R8 0 RegL
    0x46 -> return $ BIT_U3_HL_REF 0
    0x47 -> return $ BIT_U3_R8 0 RegA
    0x48 -> return $ BIT_U3_R8 1 RegB
    0x49 -> return $ BIT_U3_R8 1 RegC
    0x4A -> return $ BIT_U3_R8 1 RegD
    0x4B -> return $ BIT_U3_R8 1 RegE
    0x4C -> return $ BIT_U3_R8 1 RegH
    0x4D -> return $ BIT_U3_R8 1 RegL
    0x4E -> return $ BIT_U3_HL_REF 1
    0x4F -> return $ BIT_U3_R8 1 RegA
    0x50 -> return $ BIT_U3_R8 2 RegB
    0x51 -> return $ BIT_U3_R8 2 RegC
    0x52 -> return $ BIT_U3_R8 2 RegD
    0x53 -> return $ BIT_U3_R8 2 RegE
    0x54 -> return $ BIT_U3_R8 2 RegH
    0x55 -> return $ BIT_U3_R8 2 RegL
    0x56 -> return $ BIT_U3_HL_REF 2
    0x57 -> return $ BIT_U3_R8 2 RegA
    0x58 -> return $ BIT_U3_R8 3 RegB
    0x59 -> return $ BIT_U3_R8 3 RegC
    0x5A -> return $ BIT_U3_R8 3 RegD
    0x5B -> return $ BIT_U3_R8 3 RegE
    0x5C -> return $ BIT_U3_R8 3 RegH
    0x5D -> return $ BIT_U3_R8 3 RegL
    0x5E -> return $ BIT_U3_HL_REF 3
    0x5F -> return $ BIT_U3_R8 3 RegA
    0x60 -> return $ BIT_U3_R8 4 RegB
    0x61 -> return $ BIT_U3_R8 4 RegC
    0x62 -> return $ BIT_U3_R8 4 RegD
    0x63 -> return $ BIT_U3_R8 4 RegE
    0x64 -> return $ BIT_U3_R8 4 RegH
    0x65 -> return $ BIT_U3_R8 4 RegL
    0x66 -> return $ BIT_U3_HL_REF 4
    0x67 -> return $ BIT_U3_R8 4 RegA
    0x68 -> return $ BIT_U3_R8 5 RegB
    0x69 -> return $ BIT_U3_R8 5 RegC
    0x6A -> return $ BIT_U3_R8 5 RegD
    0x6B -> return $ BIT_U3_R8 5 RegE
    0x6C -> return $ BIT_U3_R8 5 RegH
    0x6D -> return $ BIT_U3_R8 5 RegL
    0x6E -> return $ BIT_U3_HL_REF 5
    0x6F -> return $ BIT_U3_R8 5 RegA
    0x70 -> return $ BIT_U3_R8 6 RegB
    0x71 -> return $ BIT_U3_R8 6 RegC
    0x72 -> return $ BIT_U3_R8 6 RegD
    0x73 -> return $ BIT_U3_R8 6 RegE
    0x74 -> return $ BIT_U3_R8 6 RegH
    0x75 -> return $ BIT_U3_R8 6 RegL
    0x76 -> return $ BIT_U3_HL_REF 6
    0x77 -> return $ BIT_U3_R8 6 RegA
    0x78 -> return $ BIT_U3_R8 7 RegB
    0x79 -> return $ BIT_U3_R8 7 RegC
    0x7A -> return $ BIT_U3_R8 7 RegD
    0x7B -> return $ BIT_U3_R8 7 RegE
    0x7C -> return $ BIT_U3_R8 7 RegH
    0x7D -> return $ BIT_U3_R8 7 RegL
    0x7E -> return $ BIT_U3_HL_REF 7
    0x7F -> return $ BIT_U3_R8 7 RegA
    0x80 -> return $ RES_U3_R8 0 RegB
    0x81 -> return $ RES_U3_R8 0 RegC
    0x82 -> return $ RES_U3_R8 0 RegD
    0x83 -> return $ RES_U3_R8 0 RegE
    0x84 -> return $ RES_U3_R8 0 RegH
    0x85 -> return $ RES_U3_R8 0 RegL
    0x86 -> return $ RES_U3_HL_REF 0
    0x87 -> return $ RES_U3_R8 0 RegA
    0x88 -> return $ RES_U3_R8 1 RegB
    0x89 -> return $ RES_U3_R8 1 RegC
    0x8A -> return $ RES_U3_R8 1 RegD
    0x8B -> return $ RES_U3_R8 1 RegE
    0x8C -> return $ RES_U3_R8 1 RegH
    0x8D -> return $ RES_U3_R8 1 RegL
    0x8E -> return $ RES_U3_HL_REF 1
    0x8F -> return $ RES_U3_R8 1 RegA
    0x90 -> return $ RES_U3_R8 2 RegB
    0x91 -> return $ RES_U3_R8 2 RegC
    0x92 -> return $ RES_U3_R8 2 RegD
    0x93 -> return $ RES_U3_R8 2 RegE
    0x94 -> return $ RES_U3_R8 2 RegH
    0x95 -> return $ RES_U3_R8 2 RegL
    0x96 -> return $ RES_U3_HL_REF 2
    0x97 -> return $ RES_U3_R8 2 RegA
    0x98 -> return $ RES_U3_R8 3 RegB
    0x99 -> return $ RES_U3_R8 3 RegC
    0x9A -> return $ RES_U3_R8 3 RegD
    0x9B -> return $ RES_U3_R8 3 RegE
    0x9C -> return $ RES_U3_R8 3 RegH
    0x9D -> return $ RES_U3_R8 3 RegL
    0x9E -> return $ RES_U3_HL_REF 3
    0x9F -> return $ RES_U3_R8 3 RegA
    0xA0 -> return $ RES_U3_R8 4 RegB
    0xA1 -> return $ RES_U3_R8 4 RegC
    0xA2 -> return $ RES_U3_R8 4 RegD
    0xA3 -> return $ RES_U3_R8 4 RegE
    0xA4 -> return $ RES_U3_R8 4 RegH
    0xA5 -> return $ RES_U3_R8 4 RegL
    0xA6 -> return $ RES_U3_HL_REF 4
    0xA7 -> return $ RES_U3_R8 4 RegA
    0xA8 -> return $ RES_U3_R8 5 RegB
    0xA9 -> return $ RES_U3_R8 5 RegC
    0xAA -> return $ RES_U3_R8 5 RegD
    0xAB -> return $ RES_U3_R8 5 RegE
    0xAC -> return $ RES_U3_R8 5 RegH
    0xAD -> return $ RES_U3_R8 5 RegL
    0xAE -> return $ RES_U3_HL_REF 5
    0xAF -> return $ RES_U3_R8 5 RegA
    0xB0 -> return $ RES_U3_R8 6 RegB
    0xB1 -> return $ RES_U3_R8 6 RegC
    0xB2 -> return $ RES_U3_R8 6 RegD
    0xB3 -> return $ RES_U3_R8 6 RegE
    0xB4 -> return $ RES_U3_R8 6 RegH
    0xB5 -> return $ RES_U3_R8 6 RegL
    0xB6 -> return $ RES_U3_HL_REF 6
    0xB7 -> return $ RES_U3_R8 6 RegA
    0xB8 -> return $ RES_U3_R8 7 RegB
    0xB9 -> return $ RES_U3_R8 7 RegC
    0xBA -> return $ RES_U3_R8 7 RegD
    0xBB -> return $ RES_U3_R8 7 RegE
    0xBC -> return $ RES_U3_R8 7 RegH
    0xBD -> return $ RES_U3_R8 7 RegL
    0xBE -> return $ RES_U3_HL_REF 7
    0xBF -> return $ RES_U3_R8 7 RegA
    0xC0 -> return $ SET_U3_R8 0 RegB
    0xC1 -> return $ SET_U3_R8 0 RegC
    0xC2 -> return $ SET_U3_R8 0 RegD
    0xC3 -> return $ SET_U3_R8 0 RegE
    0xC4 -> return $ SET_U3_R8 0 RegH
    0xC5 -> return $ SET_U3_R8 0 RegL
    0xC6 -> return $ SET_U3_HL_REF 0
    0xC7 -> return $ SET_U3_R8 0 RegA
    0xC8 -> return $ SET_U3_R8 1 RegB
    0xC9 -> return $ SET_U3_R8 1 RegC
    0xCA -> return $ SET_U3_R8 1 RegD
    0xCB -> return $ SET_U3_R8 1 RegE
    0xCC -> return $ SET_U3_R8 1 RegH
    0xCD -> return $ SET_U3_R8 1 RegL
    0xCE -> return $ SET_U3_HL_REF 1
    0xCF -> return $ SET_U3_R8 1 RegA
    0xD0 -> return $ SET_U3_R8 2 RegB
    0xD1 -> return $ SET_U3_R8 2 RegC
    0xD2 -> return $ SET_U3_R8 2 RegD
    0xD3 -> return $ SET_U3_R8 2 RegE
    0xD4 -> return $ SET_U3_R8 2 RegH
    0xD5 -> return $ SET_U3_R8 2 RegL
    0xD6 -> return $ SET_U3_HL_REF 2
    0xD7 -> return $ SET_U3_R8 2 RegA
    0xD8 -> return $ SET_U3_R8 3 RegB
    0xD9 -> return $ SET_U3_R8 3 RegC
    0xDA -> return $ SET_U3_R8 3 RegD
    0xDB -> return $ SET_U3_R8 3 RegE
    0xDC -> return $ SET_U3_R8 3 RegH
    0xDD -> return $ SET_U3_R8 3 RegL
    0xDE -> return $ SET_U3_HL_REF 3
    0xDF -> return $ SET_U3_R8 3 RegA
    0xE0 -> return $ SET_U3_R8 4 RegB
    0xE1 -> return $ SET_U3_R8 4 RegC
    0xE2 -> return $ SET_U3_R8 4 RegD
    0xE3 -> return $ SET_U3_R8 4 RegE
    0xE4 -> return $ SET_U3_R8 4 RegH
    0xE5 -> return $ SET_U3_R8 4 RegL
    0xE6 -> return $ SET_U3_HL_REF 4
    0xE7 -> return $ SET_U3_R8 4 RegA
    0xE8 -> return $ SET_U3_R8 5 RegB
    0xE9 -> return $ SET_U3_R8 5 RegC
    0xEA -> return $ SET_U3_R8 5 RegD
    0xEB -> return $ SET_U3_R8 5 RegE
    0xEC -> return $ SET_U3_R8 5 RegH
    0xED -> return $ SET_U3_R8 5 RegL
    0xEE -> return $ SET_U3_HL_REF 5
    0xEF -> return $ SET_U3_R8 5 RegA
    0xF0 -> return $ SET_U3_R8 6 RegB
    0xF1 -> return $ SET_U3_R8 6 RegC
    0xF2 -> return $ SET_U3_R8 6 RegD
    0xF3 -> return $ SET_U3_R8 6 RegE
    0xF4 -> return $ SET_U3_R8 6 RegH
    0xF5 -> return $ SET_U3_R8 6 RegL
    0xF6 -> return $ SET_U3_HL_REF 6
    0xF7 -> return $ SET_U3_R8 6 RegA
    0xF8 -> return $ SET_U3_R8 7 RegB
    0xF9 -> return $ SET_U3_R8 7 RegC
    0xFA -> return $ SET_U3_R8 7 RegD
    0xFB -> return $ SET_U3_R8 7 RegE
    0xFC -> return $ SET_U3_R8 7 RegH
    0xFD -> return $ SET_U3_R8 7 RegL
    0xFE -> return $ SET_U3_HL_REF 7
    0xFF -> return $ SET_U3_R8 7 RegA
    _ -> error $ "Unknown prefixed opcode: " ++ show opcode