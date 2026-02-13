module CPU.Interface where

import CPU.Pure qualified as Pure
import Control.Monad (when)
import Data.Bits
import Data.IORef
import Data.Int (Int16, Int32)
import Data.Vector.Unboxed.Mutable qualified as MV
import Data.Word (Word16, Word8)
import Lens.Micro ((^.))
import Types

addToRegister :: Cpu -> Registers -> Word8 -> IO ()
addToRegister cpu reg val = do
  currentVal <- readRegister cpu reg
  let newVal = currentVal + val
  MV.unsafeWrite (cpu ^. registers) (regIdx reg) newVal

incPC :: Cpu -> IO ()
incPC cpu = do
  pcValue <- readIORef (cpu ^. pc)
  writeIORef (cpu ^. pc) (pcValue + 1)

incSP :: Cpu -> IO ()
incSP cpu = do
  spValue <- readIORef (cpu ^. sp)
  writeIORef (cpu ^. sp) (spValue + 1)

doubleIncPC :: Cpu -> IO ()
doubleIncPC cpu = do
  pcValue <- readIORef (cpu ^. pc)
  writeIORef (cpu ^. pc) (pcValue + 2)

tripleIncPC :: Cpu -> IO ()
tripleIncPC cpu = do
  pcValue <- readIORef (cpu ^. pc)
  writeIORef (cpu ^. pc) (pcValue + 3)

addToPC :: Cpu -> Int16 -> IO ()
addToPC cpu offset = do
  currentPC <- readIORef (cpu ^. pc)
  let newPC = (fromIntegral currentPC :: Int32) + fromIntegral offset
  writeIORef (cpu ^. pc) (fromIntegral newPC)

readRegister :: Cpu -> Registers -> IO Word8
readRegister cpu reg = MV.unsafeRead (cpu ^. registers) (regIdx reg)

readZeroFlag :: Cpu -> IO Word8
readZeroFlag cpu = (`shiftR` 7) . (.&. 0x80) <$> readRegister cpu RegF

readCarryFlag :: Cpu -> IO Word8
readCarryFlag cpu = (`shiftR` 4) . (.&. 0x10) <$> readRegister cpu RegF

readMemory :: Cpu -> Word16 -> IO Word8
readMemory cpu addr = MV.unsafeRead (cpu ^. memory) (fromIntegral addr)

readPair :: Cpu -> Registers16 -> IO Word16
readPair cpu reg16 = case reg16 of
  RegAF -> readPair' cpu RegA RegF
  RegBC -> readPair' cpu RegB RegC
  RegDE -> readPair' cpu RegD RegE
  RegHL -> readPair' cpu RegH RegL
  RegSP -> readSP cpu

readPair' :: Cpu -> Registers -> Registers -> IO Word16
readPair' cpu highIndex lowIndex = do
  low <- readRegister cpu lowIndex
  high <- readRegister cpu highIndex
  return $ (fromIntegral high `shiftL` 8) .|. fromIntegral low

readSP :: Cpu -> IO Word16
readSP cpu = do
  readIORef (cpu ^. sp)

readPC :: Cpu -> IO Word16
readPC cpu = do
  readIORef (cpu ^. pc)

regIdx :: Registers -> Int
regIdx = fromEnum

setRegister :: Cpu -> Registers -> Word8 -> IO ()
setRegister cpu reg = MV.unsafeWrite (cpu ^. registers) (regIdx reg)

setPair :: Cpu -> Registers16 -> Word16 -> IO ()
setPair cpu reg16 val = case reg16 of
  RegAF -> setPair' cpu RegA RegF val
  RegBC -> setPair' cpu RegB RegC val
  RegDE -> setPair' cpu RegD RegE val
  RegHL -> setPair' cpu RegH RegL val
  RegSP -> setSP cpu val

setPair' :: Cpu -> Registers -> Registers -> Word16 -> IO ()
setPair' cpu highIndex lowIndex val = do
  let high = fromIntegral (val `shiftR` 8)
      low = fromIntegral (val .&. 0xFF)
  setRegister cpu highIndex high
  setRegister cpu lowIndex low

setSP :: Cpu -> Word16 -> IO ()
setSP cpu = writeIORef (cpu ^. sp)

setPC :: Cpu -> Word16 -> IO ()
setPC cpu = writeIORef (cpu ^. pc)

setIME :: Cpu -> IO ()
setIME cpu = do
  setRegister cpu RegIME 1

resetIME :: Cpu -> IO ()
resetIME cpu = do
  setRegister cpu RegIME 0

setMemory :: Cpu -> Word16 -> Word8 -> IO ()
setMemory cpu addr = MV.unsafeWrite (cpu ^. memory) (fromIntegral addr)

setHalted :: Cpu -> Bool -> IO ()
setHalted cpu = writeIORef (cpu ^. halted)

incPair :: Cpu -> Registers16 -> IO ()
incPair cpu reg16 = do
  currentVal <- readPair cpu reg16
  let newVal = currentVal + 1
  setPair cpu reg16 newVal

decPair :: Cpu -> Registers16 -> IO ()
decPair cpu reg16 = do
  currentVal <- readPair cpu reg16
  let newVal = currentVal - 1
  setPair cpu reg16 newVal

decSP :: Cpu -> IO ()
decSP cpu = do
  spValue <- readSP cpu
  writeIORef (cpu ^. sp) (spValue - 1)

decPC :: Cpu -> IO ()
decPC cpu = do
  pcValue <- readPC cpu
  writeIORef (cpu ^. pc) (pcValue - 1)

incRegister :: Cpu -> Registers -> IO ()
incRegister cpu reg = do
  currentVal <- readRegister cpu reg
  currentCarry <- readCarryFlag cpu
  let (newVal, newFlags) = Pure.inc currentVal currentCarry
  setRegister cpu reg newVal
  setRegister cpu RegF newFlags

decRegister :: Cpu -> Registers -> IO ()
decRegister cpu reg = do
  currentVal <- readRegister cpu reg
  currentCarry <- readCarryFlag cpu
  let (newVal, newFlags) = Pure.dec currentVal currentCarry
  setRegister cpu reg newVal
  setRegister cpu RegF newFlags

ret :: Cpu -> IO ()
ret cpu = do
  lowAddr <- readSP cpu
  low <- readMemory cpu (fromIntegral lowAddr)
  incSP cpu
  highAddr <- readSP cpu
  high <- readMemory cpu (fromIntegral highAddr)
  incSP cpu
  setPC cpu ((fromIntegral high `shiftL` 8) .|. fromIntegral low)

pop :: Cpu -> Registers16 -> IO ()
pop cpu reg16 = do
  lowAddr <- readSP cpu
  low <- readMemory cpu (fromIntegral lowAddr)
  incSP cpu
  highAddr <- readSP cpu
  high <- readMemory cpu (fromIntegral highAddr)
  incSP cpu
  let value = (fromIntegral high `shiftL` 8) .|. fromIntegral low
  setPair cpu reg16 value
  when (reg16 == RegAF) $ do
    let newZero = shiftR low 7 .&. 0x01
        newN = shiftR low 6 .&. 0x01
        newH = shiftR low 5 .&. 0x01
        newC = shiftR low 4 .&. 0x01
        newFlags = Pure.flagsToWord8 (newZero /= 0) (newN /= 0) (newH /= 0) (newC /= 0)
    setRegister cpu RegF newFlags

call :: Cpu -> Word16 -> IO ()
call cpu addr = do
  currPC <- readPC cpu
  decSP cpu
  addrH <- readSP cpu
  setMemory cpu (fromIntegral addrH) (fromIntegral (shiftR currPC 8))
  decSP cpu
  addrL <- readSP cpu
  setMemory cpu (fromIntegral addrL) (fromIntegral (currPC .&. 0xFF))
  setPC cpu addr

push :: Cpu -> Registers16 -> IO ()
push cpu reg16 = do
  value <- readPair cpu reg16
  decSP cpu
  addrH <- readSP cpu
  setMemory cpu (fromIntegral addrH) (fromIntegral (shiftR value 8))
  decSP cpu
  addrL <- readSP cpu
  setMemory cpu (fromIntegral addrL) (fromIntegral (value .&. 0xFF))

setPendingEI :: Cpu -> Bool -> IO ()
setPendingEI cpu = writeIORef (cpu ^. pendingEI)

resetPendingEI :: Cpu -> IO ()
resetPendingEI cpu = writeIORef (cpu ^. pendingEI) False

isPendingEI :: Cpu -> IO Bool
isPendingEI cpu = readIORef (cpu ^. pendingEI)