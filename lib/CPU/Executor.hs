module CPU.Executor where

import CPU.Instructions (Instruction (..))
import CPU.Interface
  ( addToPC,
    call,
    decPair,
    decRegister,
    decSP,
    doubleIncPC,
    incPC,
    incPair,
    incRegister,
    pop,
    push,
    readCarryFlag,
    readMemory,
    readPair,
    readRegister,
    readSP,
    readZeroFlag,
    resetIME,
    ret,
    setAboutToEI,
    setHalted,
    setIME,
    setMemory,
    setPC,
    setPair,
    setRegister,
    setSP,
    tripleIncPC,
  )
import CPU.Pure qualified as Pure
import Control.Monad (when)
import Data.Bits (shiftR, (.&.))
import Data.Word (Word16)
import Types (Cpu, Registers (..), Registers16 (..))

executeInstruction :: Cpu -> Instruction -> IO ()
executeInstruction cpu instruction = case instruction of
  ADC_A_R8 target -> do
    incPC cpu
    (result, flags) <- Pure.addc <$> readRegister cpu RegA <*> readRegister cpu target <*> readCarryFlag cpu
    setRegister cpu RegA result
    setRegister cpu RegF flags
  ADC_A_HL_REF -> do
    incPC cpu
    hl <- readPair cpu RegHL
    memVal <- readMemory cpu (fromIntegral hl)
    (result, flags) <- Pure.addc <$> readRegister cpu RegA <*> pure memVal <*> readCarryFlag cpu
    setRegister cpu RegA result
    setRegister cpu RegF flags
  ADC_A_N8 val -> do
    doubleIncPC cpu
    (result, flags) <- Pure.addc <$> readRegister cpu RegA <*> pure val <*> readCarryFlag cpu
    setRegister cpu RegA result
    setRegister cpu RegF flags
  ADD_A_R8 target -> do
    incPC cpu
    (result, flags) <- Pure.add <$> readRegister cpu RegA <*> readRegister cpu target
    setRegister cpu RegA result
    setRegister cpu RegF flags
  ADD_A_HL_REF -> do
    incPC cpu
    hl <- readPair cpu RegHL
    memVal <- readMemory cpu (fromIntegral hl)
    (result, flags) <- Pure.add <$> readRegister cpu RegA <*> pure memVal
    setRegister cpu RegA result
    setRegister cpu RegF flags
  ADD_A_N8 val -> do
    doubleIncPC cpu
    (result, flags) <- Pure.add <$> readRegister cpu RegA <*> pure val
    setRegister cpu RegA result
    setRegister cpu RegF flags
  ADD_HL_N16 val -> do
    incPC cpu
    (result, flags) <- Pure.add16 <$> readPair cpu RegHL <*> pure val <*> readZeroFlag cpu
    setPair cpu RegHL result
    setRegister cpu RegF flags
  ADD_HL_R16 reg -> do
    incPC cpu
    (result, flags) <- Pure.add16 <$> readPair cpu RegHL <*> readPair cpu reg <*> readZeroFlag cpu
    setPair cpu RegHL result
    setRegister cpu RegF flags
  ADD_SP_E8 val -> do
    doubleIncPC cpu
    (result, flags) <- Pure.addSigned <$> readSP cpu <*> pure val
    setSP cpu result
    setRegister cpu RegF flags
  SUB_A_R8 target -> do
    incPC cpu
    (result, flags) <- Pure.sub <$> readRegister cpu RegA <*> readRegister cpu target
    setRegister cpu RegA result
    setRegister cpu RegF flags
  SUB_A_N8 value -> do
    doubleIncPC cpu
    (result, flags) <- Pure.sub <$> readRegister cpu RegA <*> pure value
    setRegister cpu RegA result
    setRegister cpu RegF flags
  SUB_A_HL_REF -> do
    incPC cpu
    hl <- readPair cpu RegHL
    memVal <- readMemory cpu (fromIntegral hl)
    (result, flags) <- Pure.sub <$> readRegister cpu RegA <*> pure memVal
    setRegister cpu RegA result
    setRegister cpu RegF flags
  SBC_A_R8 target -> do
    incPC cpu
    (result, flags) <- Pure.subc <$> readRegister cpu RegA <*> readRegister cpu target <*> readCarryFlag cpu
    setRegister cpu RegA result
    setRegister cpu RegF flags
  SBC_A_N8 value -> do
    doubleIncPC cpu
    (result, flags) <- Pure.subc <$> readRegister cpu RegA <*> pure value <*> readCarryFlag cpu
    setRegister cpu RegA result
    setRegister cpu RegF flags
  SBC_A_HL_REF -> do
    incPC cpu
    hl <- readPair cpu RegHL
    memVal <- readMemory cpu (fromIntegral hl)
    (result, flags) <- Pure.subc <$> readRegister cpu RegA <*> pure memVal <*> readCarryFlag cpu
    setRegister cpu RegA result
    setRegister cpu RegF flags
  AND_A_R8 target -> do
    incPC cpu
    (result, flags) <- Pure.and <$> readRegister cpu RegA <*> readRegister cpu target
    setRegister cpu RegA result
    setRegister cpu RegF flags
  AND_A_N8 value -> do
    doubleIncPC cpu
    (result, flags) <- Pure.and <$> readRegister cpu RegA <*> pure value
    setRegister cpu RegA result
    setRegister cpu RegF flags
  AND_A_HL_REF -> do
    incPC cpu
    hl <- readPair cpu RegHL
    memVal <- readMemory cpu (fromIntegral hl)
    (result, flags) <- Pure.and <$> readRegister cpu RegA <*> pure memVal
    setRegister cpu RegA result
    setRegister cpu RegF flags
  XOR_A_R8 target -> do
    incPC cpu
    (result, flags) <- Pure.iXor <$> readRegister cpu RegA <*> readRegister cpu target
    setRegister cpu RegA result
    setRegister cpu RegF flags
  XOR_A_N8 value -> do
    doubleIncPC cpu
    (result, flags) <- Pure.iXor <$> readRegister cpu RegA <*> pure value
    setRegister cpu RegA result
    setRegister cpu RegF flags
  XOR_A_HL_REF -> do
    incPC cpu
    hl <- readPair cpu RegHL
    memVal <- readMemory cpu (fromIntegral hl)
    (result, flags) <- Pure.iXor <$> readRegister cpu RegA <*> pure memVal
    setRegister cpu RegA result
    setRegister cpu RegF flags
  OR_A_R8 target -> do
    incPC cpu
    (result, flags) <- Pure.or <$> readRegister cpu RegA <*> readRegister cpu target
    setRegister cpu RegA result
    setRegister cpu RegF flags
  OR_A_N8 value -> do
    doubleIncPC cpu
    (result, flags) <- Pure.or <$> readRegister cpu RegA <*> pure value
    setRegister cpu RegA result
    setRegister cpu RegF flags
  OR_A_HL_REF -> do
    incPC cpu
    hl <- readPair cpu RegHL
    memVal <- readMemory cpu (fromIntegral hl)
    (result, flags) <- Pure.or <$> readRegister cpu RegA <*> pure memVal
    setRegister cpu RegA result
    setRegister cpu RegF flags
  CP_A_R8 target -> do
    incPC cpu
    (_, flags) <- Pure.sub <$> readRegister cpu RegA <*> readRegister cpu target
    setRegister cpu RegF flags
  CP_A_N8 value -> do
    doubleIncPC cpu
    (_, flags) <- Pure.sub <$> readRegister cpu RegA <*> pure value
    setRegister cpu RegF flags
  CP_A_HL_REF -> do
    incPC cpu
    hl <- readPair cpu RegHL
    memVal <- readMemory cpu (fromIntegral hl)
    (_, flags) <- Pure.sub <$> readRegister cpu RegA <*> pure memVal
    setRegister cpu RegF flags
  LD_R8_R8 dest src -> do
    incPC cpu
    val <- readRegister cpu src
    setRegister cpu dest val
  LD_R8_HL dest -> do
    incPC cpu
    hl <- readPair cpu RegHL
    val <- readMemory cpu (fromIntegral hl)
    setRegister cpu dest val
  LD_HL_R8 src -> do
    incPC cpu
    hl <- readPair cpu RegHL
    val <- readRegister cpu src
    setMemory cpu (fromIntegral hl) val
  HALT -> do
    incPC cpu
    setHalted cpu True
  JR_NC_E8 offset -> do
    doubleIncPC cpu
    carry <- readCarryFlag cpu
    when (carry == 0) $ do
      addToPC cpu (fromIntegral offset)
  LD_R16_N16 reg16 val -> do
    tripleIncPC cpu
    setPair cpu reg16 val
  LD_HLD_A -> do
    incPC cpu
    hl <- readPair cpu RegHL
    aVal <- readRegister cpu RegA
    setMemory cpu (fromIntegral hl) aVal
    setPair cpu RegHL (hl - 1)
  INC_R16 reg16 -> do
    incPC cpu
    incPair cpu reg16
  DEC_R16 reg16 -> do
    incPC cpu
    decPair cpu reg16
  INC_HL_REF -> do
    incPC cpu
    hl <- readPair cpu RegHL
    memVal <- readMemory cpu (fromIntegral hl)
    (result, flags) <- Pure.inc memVal <$> readCarryFlag cpu
    setMemory cpu (fromIntegral hl) result
    setRegister cpu RegF flags
  DEC_HL_REF -> do
    incPC cpu
    hl <- readPair cpu RegHL
    memVal <- readMemory cpu (fromIntegral hl)
    (result, flags) <- Pure.dec memVal <$> readCarryFlag cpu
    setMemory cpu (fromIntegral hl) result
    setRegister cpu RegF flags
  LD_HL_N8 val -> do
    doubleIncPC cpu
    hl <- readPair cpu RegHL
    setMemory cpu (fromIntegral hl) val
  SCF -> do
    incPC cpu
    currZero <- readZeroFlag cpu
    let newFlags = Pure.setCarryFlag currZero
    setRegister cpu RegF newFlags
  JR_C_E8 offset -> do
    doubleIncPC cpu
    carry <- readCarryFlag cpu
    when (carry /= 0) $ do
      addToPC cpu (fromIntegral offset)
  LD_A_HLD -> do
    incPC cpu
    hl <- readPair cpu RegHL
    val <- readMemory cpu (fromIntegral hl)
    setRegister cpu RegA val
    setPair cpu RegHL (hl - 1)
  DEC_SP -> do
    incPC cpu
    decSP cpu
  INC_R8 reg -> do
    incPC cpu
    incRegister cpu reg
  DEC_R8 reg -> do
    incPC cpu
    decRegister cpu reg
  LD_R8_N8 reg val -> do
    doubleIncPC cpu
    setRegister cpu reg val
  CCF -> do
    incPC cpu
    currFlags <- readRegister cpu RegF
    let newFlags = Pure.complementCarryFlag currFlags
    setRegister cpu RegF newFlags
  JR_NZ_E8 offset -> do
    doubleIncPC cpu
    zeroFlag <- readZeroFlag cpu
    when (zeroFlag == 0) $ do
      addToPC cpu (fromIntegral offset)
  LD_HLI_A -> do
    incPC cpu
    hl <- readPair cpu RegHL
    aVal <- readRegister cpu RegA
    setMemory cpu (fromIntegral hl) aVal
    setPair cpu RegHL (hl + 1)
  DAA -> do
    incPC cpu
    aVal <- readRegister cpu RegA
    currFlags <- readRegister cpu RegF
    let (result, newFlags) = Pure.daa aVal currFlags
    setRegister cpu RegA result
    setRegister cpu RegF newFlags
  JR_Z_E8 offset -> do
    doubleIncPC cpu
    zeroFlag <- readZeroFlag cpu
    when (zeroFlag /= 0) $ do
      addToPC cpu (fromIntegral offset)
  LD_A_HLI -> do
    incPC cpu
    hl <- readPair cpu RegHL
    val <- readMemory cpu (fromIntegral hl)
    setRegister cpu RegA val
    setPair cpu RegHL (hl + 1)
  CPL -> do
    incPC cpu
    aVal <- readRegister cpu RegA
    currFlags <- readRegister cpu RegF
    let (result, newFlags) = Pure.comp aVal currFlags
    setRegister cpu RegA result
    setRegister cpu RegF newFlags
  STOP -> do
    incPC cpu
  LD_R16_REF_A reg16 -> do
    incPC cpu
    aVal <- readRegister cpu RegA
    memAddr <- readPair cpu reg16
    setMemory cpu (fromIntegral memAddr) aVal
  RLA -> do
    incPC cpu
    aVal <- readRegister cpu RegA
    currCarry <- readCarryFlag cpu
    let (result, newFlags) = Pure.rla aVal currCarry
    setRegister cpu RegA result
    setRegister cpu RegF newFlags
  JR_E8 offset -> do
    doubleIncPC cpu
    addToPC cpu (fromIntegral offset)
  LD_A_R16_REF reg16 -> do
    incPC cpu
    memAddr <- readPair cpu reg16
    val <- readMemory cpu (fromIntegral memAddr)
    setRegister cpu RegA val
  RRA -> do
    incPC cpu
    aVal <- readRegister cpu RegA
    currCarry <- readCarryFlag cpu
    let (result, newFlags) = Pure.rra aVal currCarry
    setRegister cpu RegA result
    setRegister cpu RegF newFlags
  NOP -> do
    incPC cpu
  RLCA -> do
    incPC cpu
    aVal <- readRegister cpu RegA
    let (result, newFlags) = Pure.rlca aVal
    setRegister cpu RegA result
    setRegister cpu RegF newFlags
  LD_N16_REF_SP addr -> do
    tripleIncPC cpu
    spVal <- readSP cpu
    setMemory cpu (fromIntegral addr) (fromIntegral (spVal .&. 0xFF))
    setMemory cpu (fromIntegral (addr + 1)) (fromIntegral (shiftR spVal 8))
  RRCA -> do
    incPC cpu
    aVal <- readRegister cpu RegA
    let (result, newFlags) = Pure.rrca aVal
    setRegister cpu RegA result
    setRegister cpu RegF newFlags
  RET_NZ -> do
    incPC cpu
    zeroFlag <- readZeroFlag cpu
    when (zeroFlag == 0) $ do
      ret cpu
  POP_R16 reg16 -> do
    incPC cpu
    pop cpu reg16
  JP_NZ_N16 addr -> do
    tripleIncPC cpu
    zeroFlag <- readZeroFlag cpu
    when (zeroFlag == 0) $ do
      setPC cpu addr
  JP_N16 addr -> do
    setPC cpu addr
  CALL_NZ_N16 addr -> do
    tripleIncPC cpu
    zeroFlag <- readZeroFlag cpu
    when (zeroFlag == 0) $ do
      call cpu addr
  PUSH_R16 reg16 -> do
    incPC cpu
    push cpu reg16
  RST rstAddr -> do
    incPC cpu
    call cpu (fromIntegral rstAddr)
  RET_Z -> do
    incPC cpu
    zeroFlag <- readZeroFlag cpu
    when (zeroFlag /= 0) $ do
      ret cpu
  RET -> do
    incPC cpu
    ret cpu
  JP_Z_N16 addr -> do
    tripleIncPC cpu
    zeroFlag <- readZeroFlag cpu
    when (zeroFlag /= 0) $ do
      setPC cpu addr
  RLC_R8 reg -> do
    doubleIncPC cpu
    regVal <- readRegister cpu reg
    let (result, newFlags) = Pure.rlc regVal
    setRegister cpu reg result
    setRegister cpu RegF newFlags
  RLC_HL_REF -> do
    doubleIncPC cpu
    hl <- readPair cpu RegHL
    memVal <- readMemory cpu (fromIntegral hl)
    let (result, newFlags) = Pure.rlc memVal
    setMemory cpu (fromIntegral hl) result
    setRegister cpu RegF newFlags
  RRC_R8 reg -> do
    doubleIncPC cpu
    regVal <- readRegister cpu reg
    let (result, newFlags) = Pure.rrc regVal
    setRegister cpu reg result
    setRegister cpu RegF newFlags
  RRC_HL_REF -> do
    doubleIncPC cpu
    hl <- readPair cpu RegHL
    memVal <- readMemory cpu (fromIntegral hl)
    let (result, newFlags) = Pure.rrc memVal
    setMemory cpu (fromIntegral hl) result
    setRegister cpu RegF newFlags
  RL_R8 reg -> do
    doubleIncPC cpu
    regVal <- readRegister cpu reg
    currCarry <- readCarryFlag cpu
    let (result, newFlags) = Pure.rl regVal currCarry
    setRegister cpu reg result
    setRegister cpu RegF newFlags
  RL_HL_REF -> do
    doubleIncPC cpu
    hl <- readPair cpu RegHL
    memVal <- readMemory cpu (fromIntegral hl)
    currCarry <- readCarryFlag cpu
    let (result, newFlags) = Pure.rl memVal currCarry
    setMemory cpu (fromIntegral hl) result
    setRegister cpu RegF newFlags
  RR_R8 reg -> do
    doubleIncPC cpu
    regVal <- readRegister cpu reg
    currCarry <- readCarryFlag cpu
    let (result, newFlags) = Pure.rr regVal currCarry
    setRegister cpu reg result
    setRegister cpu RegF newFlags
  RR_HL_REF -> do
    doubleIncPC cpu
    hl <- readPair cpu RegHL
    memVal <- readMemory cpu (fromIntegral hl)
    currCarry <- readCarryFlag cpu
    let (result, newFlags) = Pure.rr memVal currCarry
    setMemory cpu (fromIntegral hl) result
    setRegister cpu RegF newFlags
  SLA_R8 reg -> do
    doubleIncPC cpu
    regVal <- readRegister cpu reg
    let (result, newFlags) = Pure.sla regVal
    setRegister cpu reg result
    setRegister cpu RegF newFlags
  SLA_HL_REF -> do
    doubleIncPC cpu
    hl <- readPair cpu RegHL
    memVal <- readMemory cpu (fromIntegral hl)
    let (result, newFlags) = Pure.sla memVal
    setMemory cpu (fromIntegral hl) result
    setRegister cpu RegF newFlags
  SRA_R8 reg -> do
    doubleIncPC cpu
    regVal <- readRegister cpu reg
    let (result, newFlags) = Pure.sra regVal
    setRegister cpu reg result
    setRegister cpu RegF newFlags
  SRA_HL_REF -> do
    doubleIncPC cpu
    hl <- readPair cpu RegHL
    memVal <- readMemory cpu (fromIntegral hl)
    let (result, newFlags) = Pure.sra memVal
    setMemory cpu (fromIntegral hl) result
    setRegister cpu RegF newFlags
  SWAP_R8 reg -> do
    doubleIncPC cpu
    regVal <- readRegister cpu reg
    let (result, newFlags) = Pure.swap regVal
    setRegister cpu reg result
    setRegister cpu RegF newFlags
  SWAP_HL_REF -> do
    doubleIncPC cpu
    hl <- readPair cpu RegHL
    memVal <- readMemory cpu (fromIntegral hl)
    let (result, newFlags) = Pure.swap memVal
    setMemory cpu (fromIntegral hl) result
    setRegister cpu RegF newFlags
  SRL_R8 reg -> do
    doubleIncPC cpu
    regVal <- readRegister cpu reg
    let (result, newFlags) = Pure.srl regVal
    setRegister cpu reg result
    setRegister cpu RegF newFlags
  SRL_HL_REF -> do
    doubleIncPC cpu
    hl <- readPair cpu RegHL
    memVal <- readMemory cpu (fromIntegral hl)
    let (result, newFlags) = Pure.srl memVal
    setMemory cpu (fromIntegral hl) result
    setRegister cpu RegF newFlags
  BIT_U3_R8 bit reg -> do
    doubleIncPC cpu
    regVal <- readRegister cpu reg
    currCarry <- readCarryFlag cpu
    let newFlags = Pure.bit regVal currCarry bit
    setRegister cpu RegF newFlags
  BIT_U3_HL_REF bit -> do
    doubleIncPC cpu
    hl <- readPair cpu RegHL
    memVal <- readMemory cpu (fromIntegral hl)
    currCarry <- readCarryFlag cpu
    let newFlags = Pure.bit memVal currCarry bit
    setRegister cpu RegF newFlags
  RES_U3_R8 bit reg -> do
    doubleIncPC cpu
    regVal <- readRegister cpu reg
    let result = Pure.res regVal bit
    setRegister cpu reg result
  RES_U3_HL_REF bit -> do
    doubleIncPC cpu
    hl <- readPair cpu RegHL
    memVal <- readMemory cpu (fromIntegral hl)
    let result = Pure.res memVal bit
    setMemory cpu (fromIntegral hl) result
  SET_U3_R8 bit reg -> do
    doubleIncPC cpu
    regVal <- readRegister cpu reg
    let result = Pure.set regVal bit
    setRegister cpu reg result
  SET_U3_HL_REF bit -> do
    doubleIncPC cpu
    hl <- readPair cpu RegHL
    memVal <- readMemory cpu (fromIntegral hl)
    let result = Pure.set memVal bit
    setMemory cpu (fromIntegral hl) result
  CALL_Z_N16 addr -> do
    tripleIncPC cpu
    zeroFlag <- readZeroFlag cpu
    when (zeroFlag /= 0) $ do
      call cpu addr
  CALL_N16 addr -> do
    tripleIncPC cpu
    call cpu addr
  RET_NC -> do
    incPC cpu
    carryFlag <- readCarryFlag cpu
    when (carryFlag == 0) $ do
      ret cpu
  JP_NC_N16 addr -> do
    tripleIncPC cpu
    carryFlag <- readCarryFlag cpu
    when (carryFlag == 0) $ do
      setPC cpu addr
  CALL_NC_N16 addr -> do
    tripleIncPC cpu
    carryFlag <- readCarryFlag cpu
    when (carryFlag == 0) $ do
      call cpu addr
  RET_C -> do
    incPC cpu
    carryFlag <- readCarryFlag cpu
    when (carryFlag /= 0) $ do
      ret cpu
  RETI -> do
    incPC cpu
    setIME cpu
    ret cpu
  JP_C_N16 addr -> do
    tripleIncPC cpu
    carryFlag <- readCarryFlag cpu
    when (carryFlag /= 0) $ do
      setPC cpu addr
  CALL_C_N16 addr -> do
    tripleIncPC cpu
    carryFlag <- readCarryFlag cpu
    when (carryFlag /= 0) $ do
      call cpu addr
  LDH_N8_REF_A offset -> do
    doubleIncPC cpu
    aVal <- readRegister cpu RegA
    let addr = (0xFF00 :: Word16) + fromIntegral offset
    setMemory cpu addr aVal
  LDH_C_A -> do
    incPC cpu
    aVal <- readRegister cpu RegA
    cVal <- readRegister cpu RegC
    let addr = (0xFF00 :: Word16) + fromIntegral cVal
    setMemory cpu addr aVal
  JP_HL -> do
    incPC cpu
    hl <- readPair cpu RegHL
    setPC cpu hl
  LD_N16_REF_A addr -> do
    tripleIncPC cpu
    aVal <- readRegister cpu RegA
    setMemory cpu (fromIntegral addr) aVal
  LDH_A_N8_REF offset -> do
    doubleIncPC cpu
    let addr = (0xFF00 :: Word16) + fromIntegral offset
    val <- readMemory cpu addr
    setRegister cpu RegA val
  LDH_A_C -> do
    incPC cpu
    cVal <- readRegister cpu RegC
    let addr = (0xFF00 :: Word16) + fromIntegral cVal
    val <- readMemory cpu addr
    setRegister cpu RegA val
  DI -> do
    incPC cpu
    resetIME cpu
  LD_HL_SP_E8 val -> do
    doubleIncPC cpu
    (result, flags) <- Pure.addSigned <$> readSP cpu <*> pure val
    setPair cpu RegHL result
    setRegister cpu RegF flags
  LD_SP_HL -> do
    incPC cpu
    hl <- readPair cpu RegHL
    setSP cpu hl
  LD_A_N16_REF addr -> do
    tripleIncPC cpu
    val <- readMemory cpu (fromIntegral addr)
    setRegister cpu RegA val
  EI -> do
    incPC cpu
    setAboutToEI cpu True
