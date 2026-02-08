module Decoder where

import Control.Monad.ST
import CpuUtils
import Data.STRef
import Lens.Micro ((^.))
import Types

decodeInstruction :: Cpu s -> ST s Instruction
decodeInstruction cpu = do
  pcValue <- readSTRef (cpu ^. pc)
  opcode <- fetchMemory cpu (fromIntegral pcValue)
  case opcode of
    0x80 -> return $ ADD_A_R8 RegB
    0x81 -> return $ ADD_A_R8 RegC
    0x82 -> return $ ADD_A_R8 RegD
    0x83 -> return $ ADD_A_R8 RegE
    0x84 -> return $ ADD_A_R8 RegH
    0x85 -> return $ ADD_A_R8 RegL
    0x86 -> return ADD_A_HL
    0x87 -> return $ ADD_A_R8 RegA
    _ -> error $ "Unknown opcode: " ++ show opcode