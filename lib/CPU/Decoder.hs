module CPU.Decoder where

import CPU.Instructions (Instruction (..))
import CPU.Interface
import Control.Monad.ST
import Types

decodeInstruction :: Cpu s -> ST s Instruction
decodeInstruction cpu = do
  pcValue <- readPC cpu
  opcode <- readMemory cpu (fromIntegral pcValue)
  case opcode of
    0x80 -> return $ ADD_A_R8 RegB
    0x81 -> return $ ADD_A_R8 RegC
    0x82 -> return $ ADD_A_R8 RegD
    0x83 -> return $ ADD_A_R8 RegE
    0x84 -> return $ ADD_A_R8 RegH
    0x85 -> return $ ADD_A_R8 RegL
    0x86 -> return ADD_A_HL
    0x87 -> return $ ADD_A_R8 RegA
    0x88 -> return $ ADC_A_R8 RegB
    0x89 -> return $ ADC_A_R8 RegC
    0x8A -> return $ ADC_A_R8 RegD
    0x8B -> return $ ADC_A_R8 RegE
    0x8C -> return $ ADC_A_R8 RegH
    0x8D -> return $ ADC_A_R8 RegL
    0x8E -> return ADC_A_HL
    0x8F -> return $ ADC_A_R8 RegA
    0x90 -> return $ SUB_A_R8 RegB
    0x91 -> return $ SUB_A_R8 RegC
    0x92 -> return $ SUB_A_R8 RegD
    0x93 -> return $ SUB_A_R8 RegE
    0x94 -> return $ SUB_A_R8 RegH
    0x95 -> return $ SUB_A_R8 RegL
    0x96 -> return SUB_A_HL
    0x97 -> return $ SUB_A_R8 RegA
    0x98 -> return $ SBC_A_R8 RegB
    0x99 -> return $ SBC_A_R8 RegC
    0x9A -> return $ SBC_A_R8 RegD
    0x9B -> return $ SBC_A_R8 RegE
    0x9C -> return $ SBC_A_R8 RegH
    0x9D -> return $ SBC_A_R8 RegL
    0x9E -> return SBC_A_HL
    0x9F -> return $ SBC_A_R8 RegA
    0xA0 -> return $ AND_A_R8 RegB
    0xA1 -> return $ AND_A_R8 RegC
    0xA2 -> return $ AND_A_R8 RegD
    0xA3 -> return $ AND_A_R8 RegE
    0xA4 -> return $ AND_A_R8 RegH
    0xA5 -> return $ AND_A_R8 RegL
    0xA6 -> return AND_A_HL
    0xA7 -> return $ AND_A_R8 RegA
    0xA8 -> return $ XOR_A_R8 RegB
    0xA9 -> return $ XOR_A_R8 RegC
    0xAA -> return $ XOR_A_R8 RegD
    0xAB -> return $ XOR_A_R8 RegE
    0xAC -> return $ XOR_A_R8 RegH
    0xAD -> return $ XOR_A_R8 RegL
    0xAE -> return XOR_A_HL
    0xAF -> return $ XOR_A_R8 RegA
    0xB0 -> return $ OR_A_R8 RegB
    0xB1 -> return $ OR_A_R8 RegC
    0xB2 -> return $ OR_A_R8 RegD
    0xB3 -> return $ OR_A_R8 RegE
    0xB4 -> return $ OR_A_R8 RegH
    0xB5 -> return $ OR_A_R8 RegL
    0xB6 -> return OR_A_HL
    0xB7 -> return $ OR_A_R8 RegA
    0xB8 -> return $ CP_A_R8 RegB
    0xB9 -> return $ CP_A_R8 RegC
    0xBA -> return $ CP_A_R8 RegD
    0xBB -> return $ CP_A_R8 RegE
    0xBC -> return $ CP_A_R8 RegH
    0xBD -> return $ CP_A_R8 RegL
    0xBE -> return CP_A_HL
    0xBF -> return $ CP_A_R8 RegA
    _ -> error $ "Unknown opcode: " ++ show opcode