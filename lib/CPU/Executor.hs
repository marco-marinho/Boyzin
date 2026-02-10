module CPU.Executor where

import CPU.Instructions (Instruction (..))
import CPU.Interface
import CPU.Pure qualified as Pure
import Control.Monad (when)
import Types (Cpu, Registers (..), Registers16 (..))

executeInstruction :: Cpu -> Instruction -> IO ()
executeInstruction cpu instruction = case instruction of
  ADC_A_R8 target -> do
    (result, flags) <- Pure.addc <$> readRegister cpu RegA <*> readRegister cpu target <*> readCarryFlag cpu
    setRegister cpu RegA result
    setRegister cpu RegF flags
  ADC_A_HL_REF -> do
    hl <- readPair cpu RegHL
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
  ADD_A_HL_REF -> do
    hl <- readPair cpu RegHL
    memVal <- readMemory cpu (fromIntegral hl)
    (result, flags) <- Pure.add <$> readRegister cpu RegA <*> pure memVal
    setRegister cpu RegA result
    setRegister cpu RegF flags
  ADD_A_N8 val -> do
    (result, flags) <- Pure.add <$> readRegister cpu RegA <*> pure val
    setRegister cpu RegA result
    setRegister cpu RegF flags
  ADD_HL_R16 val -> do
    (result, flags) <- Pure.add16 <$> readPair cpu RegHL <*> pure val <*> readZeroFlag cpu
    setPair cpu RegHL result
    setRegister cpu RegF flags
  ADD_HL_SP -> do
    (result, flags) <- Pure.add16 <$> readPair cpu RegHL <*> readSP cpu <*> readZeroFlag cpu
    setPair cpu RegHL result
    setRegister cpu RegF flags
  ADD_SP_E8 val -> do
    (result, flags) <- Pure.addSigned <$> readSP cpu <*> pure val
    setSP cpu result
    setRegister cpu RegF flags
  SUB_A_R8 target -> do
    (result, flags) <- Pure.sub <$> readRegister cpu RegA <*> readRegister cpu target
    setRegister cpu RegA result
    setRegister cpu RegF flags
  SUB_A_HL_REF -> do
    hl <- readPair cpu RegHL
    memVal <- readMemory cpu (fromIntegral hl)
    (result, flags) <- Pure.sub <$> readRegister cpu RegA <*> pure memVal
    setRegister cpu RegA result
    setRegister cpu RegF flags
  SBC_A_R8 target -> do
    (result, flags) <- Pure.subc <$> readRegister cpu RegA <*> readRegister cpu target <*> readCarryFlag cpu
    setRegister cpu RegA result
    setRegister cpu RegF flags
  SBC_A_HL_REF -> do
    hl <- readPair cpu RegHL
    memVal <- readMemory cpu (fromIntegral hl)
    (result, flags) <- Pure.subc <$> readRegister cpu RegA <*> pure memVal <*> readCarryFlag cpu
    setRegister cpu RegA result
    setRegister cpu RegF flags
  AND_A_R8 target -> do
    (result, flags) <- Pure.and <$> readRegister cpu RegA <*> readRegister cpu target
    setRegister cpu RegA result
    setRegister cpu RegF flags
  AND_A_HL_REF -> do
    hl <- readPair cpu RegHL
    memVal <- readMemory cpu (fromIntegral hl)
    (result, flags) <- Pure.and <$> readRegister cpu RegA <*> pure memVal
    setRegister cpu RegA result
    setRegister cpu RegF flags
  XOR_A_R8 target -> do
    (result, flags) <- Pure.iXor <$> readRegister cpu RegA <*> readRegister cpu target
    setRegister cpu RegA result
    setRegister cpu RegF flags
  XOR_A_HL_REF -> do
    hl <- readPair cpu RegHL
    memVal <- readMemory cpu (fromIntegral hl)
    (result, flags) <- Pure.iXor <$> readRegister cpu RegA <*> pure memVal
    setRegister cpu RegA result
    setRegister cpu RegF flags
  OR_A_R8 target -> do
    (result, flags) <- Pure.or <$> readRegister cpu RegA <*> readRegister cpu target
    setRegister cpu RegA result
    setRegister cpu RegF flags
  OR_A_HL_REF -> do
    hl <- readPair cpu RegHL
    memVal <- readMemory cpu (fromIntegral hl)
    (result, flags) <- Pure.or <$> readRegister cpu RegA <*> pure memVal
    setRegister cpu RegA result
    setRegister cpu RegF flags
  CP_A_R8 target -> do
    (_, flags) <- Pure.sub <$> readRegister cpu RegA <*> readRegister cpu target
    setRegister cpu RegF flags
  CP_A_HL_REF -> do
    hl <- readPair cpu RegHL
    memVal <- readMemory cpu (fromIntegral hl)
    (_, flags) <- Pure.sub <$> readRegister cpu RegA <*> pure memVal
    setRegister cpu RegF flags
  LD_R8_R8 dest src -> do
    val <- readRegister cpu src
    setRegister cpu dest val
  LD_R8_HL dest -> do
    hl <- readPair cpu RegHL
    val <- readMemory cpu (fromIntegral hl)
    setRegister cpu dest val
  LD_HL_R8 src -> do
    hl <- readPair cpu RegHL
    val <- readRegister cpu src
    setMemory cpu (fromIntegral hl) val
  HALT -> do
    setHalted cpu True
  JR_NC_E8 offset -> do
    carry <- readCarryFlag cpu
    when (carry == 0) $ do
      addToPC cpu (fromIntegral offset)
    incPC cpu
  LD_R16_N16 reg16 val -> do
    setPair cpu reg16 val
    doubleIncPC cpu
  LD_HLD_A -> do
    hl <- readPair cpu RegHL
    aVal <- readRegister cpu RegA
    setMemory cpu (fromIntegral hl) aVal
    setPair cpu RegHL (hl - 1)
  INC_R16 reg16 -> do
    incPair cpu reg16
  DEC_R16 reg16 -> do
    decPair cpu reg16
  INC_HL_REF -> do
    hl <- readPair cpu RegHL
    memVal <- readMemory cpu (fromIntegral hl)
    (result, flags) <- Pure.inc memVal <$> readCarryFlag cpu
    setMemory cpu (fromIntegral hl) result
    setRegister cpu RegF flags
  DEC_HL_REF -> do
    hl <- readPair cpu RegHL
    memVal <- readMemory cpu (fromIntegral hl)
    (result, flags) <- Pure.dec memVal <$> readCarryFlag cpu
    setMemory cpu (fromIntegral hl) result
    setRegister cpu RegF flags
  LD_HL_N8 val -> do
    hl <- readPair cpu RegHL
    setMemory cpu (fromIntegral hl) val
    incPC cpu
  SCF -> do
    currZero <- readZeroFlag cpu
    let newFlags = Pure.setCarryFlag currZero
    setRegister cpu RegF newFlags
  JR_C_E8 offset -> do
    carry <- readCarryFlag cpu
    when (carry /= 0) $ do
      addToPC cpu (fromIntegral offset)
    incPC cpu
  LD_A_HLD -> do
    hl <- readPair cpu RegHL
    val <- readMemory cpu (fromIntegral hl)
    setRegister cpu RegA val
    setPair cpu RegHL (hl - 1)
  DEC_SP -> decSP cpu
  INC_R8 reg -> do
    incRegister cpu reg
  DEC_R8 reg -> do
    decRegister cpu reg
  LD_R8_N8 reg val -> do
    setRegister cpu reg val
    incPC cpu
  CCF -> do
    currFlags <- readRegister cpu RegF
    let newFlags = Pure.complementCarryFlag currFlags
    setRegister cpu RegF newFlags
  JR_NZ_E8 offset -> do
    zeroFlag <- readZeroFlag cpu
    when (zeroFlag == 0) $ do
      addToPC cpu (fromIntegral offset)
    incPC cpu
  LD_HLI_A -> do
    hl <- readPair cpu RegHL
    aVal <- readRegister cpu RegA
    setMemory cpu (fromIntegral hl) aVal
    setPair cpu RegHL (hl + 1)
  DAA -> do
    aVal <- readRegister cpu RegA
    currFlags <- readRegister cpu RegF
    let (result, newFlags) = Pure.daa aVal currFlags
    setRegister cpu RegA result
    setRegister cpu RegF newFlags