module CPU.Executor where

import CPU.Instructions
import CPU.Interface
import Control.Monad.ST (ST)
import Types (Cpu, Instruction (..), Registers (..))

executeInstruction :: Cpu s -> Instruction -> ST s ()
executeInstruction cpu instruction = case instruction of
  ADC_A_R8 target -> do
    (result, flags) <- addc <$> readRegister cpu RegA <*> readRegister cpu target <*> readCarryFlag cpu
    setRegister cpu RegA result
    setRegister cpu RegF flags
  ADC_A_HL -> do
    hl <- readHL cpu
    memVal <- readMemory cpu (fromIntegral hl)
    (result, flags) <- addc <$> readRegister cpu RegA <*> pure memVal <*> readCarryFlag cpu
    setRegister cpu RegA result
    setRegister cpu RegF flags
  ADC_A_N8 val -> do
    (result, flags) <- addc <$> readRegister cpu RegA <*> pure val <*> readCarryFlag cpu
    setRegister cpu RegA result
    setRegister cpu RegF flags
  ADD_A_R8 target -> do
    (result, flags) <- add <$> readRegister cpu RegA <*> readRegister cpu target
    setRegister cpu RegA result
    setRegister cpu RegF flags
  ADD_A_HL -> do
    hl <- readHL cpu
    memVal <- readMemory cpu (fromIntegral hl)
    (result, flags) <- add <$> readRegister cpu RegA <*> pure memVal
    setRegister cpu RegA result
    setRegister cpu RegF flags
  ADD_A_N8 val -> do
    (result, flags) <- add <$> readRegister cpu RegA <*> pure val
    setRegister cpu RegA result
    setRegister cpu RegF flags
  ADD_HL_R16 val -> do
    (result, flags) <- add16 <$> readHL cpu <*> pure val <*> readZeroFlag cpu
    setHL cpu result
    setRegister cpu RegF flags
  ADD_HL_SP -> do
    (result, flags) <- add16 <$> readHL cpu <*> readSP cpu <*> readZeroFlag cpu
    setHL cpu result
    setRegister cpu RegF flags
  ADD_SP_E8 val -> do
    (result, flags) <- addSigned <$> readSP cpu <*> pure val
    setSP cpu result
    setRegister cpu RegF flags
