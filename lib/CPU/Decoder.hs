module CPU.Decoder where

import CPU.Instructions (Instruction (..))
import CPU.Interface (readMemory, readPC)
import Data.Bits (Bits (shiftL, (.|.)))
import Types
  ( Cpu,
    Registers (RegA, RegB, RegC, RegD, RegE, RegH, RegL),
    Registers16 (RegHL, RegSP),
  )

decodeInstruction :: Cpu -> IO Instruction
decodeInstruction cpu = do
  pcValue <- readPC cpu
  opcode <- readMemory cpu (fromIntegral pcValue)
  case opcode of
    0x20 -> do
      offset <- readMemory cpu (fromIntegral (pcValue + 1))
      return $ JR_NZ_E8 (fromIntegral offset)
    0x21 -> do
      low <- readMemory cpu (fromIntegral (pcValue + 1))
      high <- readMemory cpu (fromIntegral (pcValue + 2))
      let value = (fromIntegral high `shiftL` 8) .|. fromIntegral low
      return $ LD_R16_N16 RegHL value
    0x22 -> return LD_HLI_A
    0x23 -> return $ INC_R16 RegHL
    0x24 -> return $ INC_R8 RegH
    0x25 -> return $ DEC_R8 RegH
    0x26 -> do
      value <- readMemory cpu (fromIntegral (pcValue + 1))
      return $ LD_R8_N8 RegH value
    0x27 -> return DAA
    0x30 -> do
      offset <- readMemory cpu (fromIntegral (pcValue + 1))
      return $ JR_NC_E8 (fromIntegral offset)
    0x31 -> do
      low <- readMemory cpu (fromIntegral (pcValue + 1))
      high <- readMemory cpu (fromIntegral (pcValue + 2))
      let value = (fromIntegral high `shiftL` 8) .|. fromIntegral low
      return $ LD_R16_N16 RegSP value
    0x32 -> return LD_HLD_A
    0x33 -> return $ INC_R16 RegSP
    0x34 -> return INC_HL_REF
    0x35 -> return DEC_HL_REF
    0x36 -> do
      value <- readMemory cpu (fromIntegral (pcValue + 1))
      return $ LD_HL_N8 value
    0x37 -> return SCF
    0x38 -> do
      offset <- readMemory cpu (fromIntegral (pcValue + 1))
      return $ JR_C_E8 (fromIntegral offset)
    0x39 -> return ADD_HL_SP
    0x3A -> return LD_A_HLD
    0x3B -> return $ DEC_R16 RegSP
    0x3C -> return $ INC_R8 RegA
    0x3D -> return $ DEC_R8 RegA
    0x3E -> do
      value <- readMemory cpu (fromIntegral (pcValue + 1))
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
    _ -> error $ "Unknown opcode: " ++ show opcode