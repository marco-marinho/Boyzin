module CPU.Interface where

import Control.Monad.ST
import Data.Bits
import Data.STRef
import Data.Vector.Unboxed.Mutable qualified as MV
import Data.Word (Word16, Word8)
import Lens.Micro ((^.))
import Types

addToRegister :: Cpu s -> Registers -> Word8 -> ST s ()
addToRegister cpu reg val = do
  currentVal <- readRegister cpu reg
  let newVal = currentVal + val
  MV.write (cpu ^. registers) (regIdx reg) newVal

incPC :: Cpu s -> ST s ()
incPC cpu = do
  pcValue <- readSTRef (cpu ^. pc)
  writeSTRef (cpu ^. pc) (pcValue + 1)

readRegister :: Cpu s -> Registers -> ST s Word8
readRegister cpu reg = MV.read (cpu ^. registers) (regIdx reg)

readZeroFlag :: Cpu s -> ST s Bool
readZeroFlag cpu = do
  flags <- readRegister cpu RegF
  return $ (flags .&. 0x80) /= 0

readCarryFlag :: Cpu s -> ST s Word8
readCarryFlag cpu = (.&. 0x10) <$> readRegister cpu RegF

readMemory :: Cpu s -> Int -> ST s Word8
readMemory cpu = MV.read (cpu ^. memory)

readPair :: Cpu s -> Registers -> Registers -> ST s Word16
readPair cpu highIndex lowIndex = do
  low <- readRegister cpu lowIndex
  high <- readRegister cpu highIndex
  return $ (fromIntegral high `shiftL` 8) .|. fromIntegral low

readHL :: Cpu s -> ST s Word16
readHL cpu = readPair cpu RegH RegL

readSP :: Cpu s -> ST s Word16
readSP cpu = do
  readSTRef (cpu ^. sp)

readPC :: Cpu s -> ST s Word16
readPC cpu = do
  readSTRef (cpu ^. pc)

regIdx :: Registers -> Int
regIdx = fromEnum

setRegister :: Cpu s -> Registers -> Word8 -> ST s ()
setRegister cpu reg = MV.write (cpu ^. registers) (regIdx reg)

setPair :: Cpu s -> Registers -> Registers -> Word16 -> ST s ()
setPair cpu highIndex lowIndex val = do
  let high = fromIntegral (val `shiftR` 8)
      low = fromIntegral (val .&. 0xFF)
  setRegister cpu highIndex high
  setRegister cpu lowIndex low

setHL :: Cpu s -> Word16 -> ST s ()
setHL cpu = setPair cpu RegH RegL

setSP :: Cpu s -> Word16 -> ST s ()
setSP cpu = writeSTRef (cpu ^. sp)

setPC :: Cpu s -> Word16 -> ST s ()
setPC cpu = writeSTRef (cpu ^. pc)

setMemory :: Cpu s -> Int -> Word8 -> ST s ()
setMemory cpu = MV.write (cpu ^. memory)
