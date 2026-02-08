module CPU.Executor where

import CPU.Interface
import Control.Monad.ST
import Instructions
import Types

executeInstruction :: Cpu s -> Instruction -> ST s ()
executeInstruction cpu instruction = case instruction of
  ADDC_A_R8 target -> do
    (result, flags) <- addc <$> getRegister cpu RegA <*> getRegister cpu target <*> getCarryFlag cpu
    setRegister cpu RegA result
    setRegister cpu RegF flags
  ADDC_A_HL -> do
    hl <- readHL cpu
    memVal <- fetchMemory cpu (fromIntegral hl)
    (result, flags) <- addc <$> getRegister cpu RegA <*> pure memVal <*> getCarryFlag cpu
    setRegister cpu RegA result
    setRegister cpu RegF flags
  ADDC_A_N8 val -> do
    (result, flags) <- addc <$> getRegister cpu RegA <*> pure val <*> getCarryFlag cpu
    setRegister cpu RegA result
    setRegister cpu RegF flags
  ADD_A_R8 target -> do
    (result, flags) <- add <$> getRegister cpu RegA <*> getRegister cpu target
    setRegister cpu RegA result
    setRegister cpu RegF flags
  ADD_A_HL -> do
    hl <- readHL cpu
    memVal <- fetchMemory cpu (fromIntegral hl)
    (result, flags) <- add <$> getRegister cpu RegA <*> pure memVal
    setRegister cpu RegA result
    setRegister cpu RegF flags
  ADD_A_N8 val -> do
    (result, flags) <- add <$> getRegister cpu RegA <*> pure val
    setRegister cpu RegA result
    setRegister cpu RegF flags
  ADD_HL_R16 val -> do
    (result, flags) <- add16 <$> readHL cpu <*> pure val <*> getZeroFlag cpu
    setHL cpu result
    setRegister cpu RegF flags
  ADD_HL_SP -> do
    (result, flags) <- add16 <$> readHL cpu <*> readSP cpu <*> getZeroFlag cpu
    setHL cpu result
    setRegister cpu RegF flags
  ADD_SP_E8 val -> do
    (result, flags) <- addSigned <$> readSP cpu <*> pure val
    setSP cpu result
    setRegister cpu RegF flags
