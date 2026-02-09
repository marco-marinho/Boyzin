module CPU.Executor where

import CPU.Instructions (Instruction (..))
import CPU.Interface
import CPU.Pure qualified as Pure
import Control.Monad.ST (ST)
import Types (Cpu, Registers (..))

executeInstruction :: Cpu s -> Instruction -> ST s ()
executeInstruction cpu instruction = case instruction of
  ADC_A_R8 target -> do
    (result, flags) <- Pure.addc <$> readRegister cpu RegA <*> readRegister cpu target <*> readCarryFlag cpu
    setRegister cpu RegA result
    setRegister cpu RegF flags
  ADC_A_HL -> do
    hl <- readHL cpu
    memVal <- readMemory cpu (fromIntegral hl)
    (result, flags) <- Pure.addc <$> readRegister cpu RegA <*> pure memVal <*> readCarryFlag cpu
    setRegister cpu RegA result
    setRegister cpu RegF flags
  ADC_A_N8 val -> do
    (result, flags) <- Pure.addc <$> readRegister cpu RegA <*> pure val <*> readCarryFlag cpu
    setRegister cpu RegA result
    setRegister cpu RegF flags
  ADD_A_R8 target -> do
    (result, flags) <- Pure.add <$> readRegister cpu RegA <*> readRegister cpu target
    setRegister cpu RegA result
    setRegister cpu RegF flags
  ADD_A_HL -> do
    hl <- readHL cpu
    memVal <- readMemory cpu (fromIntegral hl)
    (result, flags) <- Pure.add <$> readRegister cpu RegA <*> pure memVal
    setRegister cpu RegA result
    setRegister cpu RegF flags
  ADD_A_N8 val -> do
    (result, flags) <- Pure.add <$> readRegister cpu RegA <*> pure val
    setRegister cpu RegA result
    setRegister cpu RegF flags
  ADD_HL_R16 val -> do
    (result, flags) <- Pure.add16 <$> readHL cpu <*> pure val <*> readZeroFlag cpu
    setHL cpu result
    setRegister cpu RegF flags
  ADD_HL_SP -> do
    (result, flags) <- Pure.add16 <$> readHL cpu <*> readSP cpu <*> readZeroFlag cpu
    setHL cpu result
    setRegister cpu RegF flags
  ADD_SP_E8 val -> do
    (result, flags) <- Pure.addSigned <$> readSP cpu <*> pure val
    setSP cpu result
    setRegister cpu RegF flags
  SUB_A_R8 target -> do
    (result, flags) <- Pure.sub <$> readRegister cpu RegA <*> readRegister cpu target
    setRegister cpu RegA result
    setRegister cpu RegF flags
  SUB_A_HL -> do
    hl <- readHL cpu
    memVal <- readMemory cpu (fromIntegral hl)
    (result, flags) <- Pure.sub <$> readRegister cpu RegA <*> pure memVal
    setRegister cpu RegA result
    setRegister cpu RegF flags
  SBC_A_R8 target -> do
    (result, flags) <- Pure.subc <$> readRegister cpu RegA <*> readRegister cpu target <*> readCarryFlag cpu
    setRegister cpu RegA result
    setRegister cpu RegF flags
  SBC_A_HL -> do
    hl <- readHL cpu
    memVal <- readMemory cpu (fromIntegral hl)
    (result, flags) <- Pure.subc <$> readRegister cpu RegA <*> pure memVal <*> readCarryFlag cpu
    setRegister cpu RegA result
    setRegister cpu RegF flags
  AND_A_R8 target -> do
    (result, flags) <- Pure.and <$> readRegister cpu RegA <*> readRegister cpu target
    setRegister cpu RegA result
    setRegister cpu RegF flags
  AND_A_HL -> do
    hl <- readHL cpu
    memVal <- readMemory cpu (fromIntegral hl)
    (result, flags) <- Pure.and <$> readRegister cpu RegA <*> pure memVal
    setRegister cpu RegA result
    setRegister cpu RegF flags
  XOR_A_R8 target -> do
    (result, flags) <- Pure.iXor <$> readRegister cpu RegA <*> readRegister cpu target
    setRegister cpu RegA result
    setRegister cpu RegF flags
  XOR_A_HL -> do
    hl <- readHL cpu
    memVal <- readMemory cpu (fromIntegral hl)
    (result, flags) <- Pure.iXor <$> readRegister cpu RegA <*> pure memVal
    setRegister cpu RegA result
    setRegister cpu RegF flags
  OR_A_R8 target -> do
    (result, flags) <- Pure.or <$> readRegister cpu RegA <*> readRegister cpu target
    setRegister cpu RegA result
    setRegister cpu RegF flags
  OR_A_HL -> do
    hl <- readHL cpu
    memVal <- readMemory cpu (fromIntegral hl)
    (result, flags) <- Pure.or <$> readRegister cpu RegA <*> pure memVal
    setRegister cpu RegA result
    setRegister cpu RegF flags
  CP_A_R8 target -> do
    (_, flags) <- Pure.sub <$> readRegister cpu RegA <*> readRegister cpu target
    setRegister cpu RegF flags
  CP_A_HL -> do
    hl <- readHL cpu
    memVal <- readMemory cpu (fromIntegral hl)
    (_, flags) <- Pure.sub <$> readRegister cpu RegA <*> pure memVal
    setRegister cpu RegF flags