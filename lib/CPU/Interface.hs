module CPU.Interface where

import Control.Monad.ST
import Data.Bits
import Data.STRef
import Data.Vector.Unboxed.Mutable qualified as MV
import Data.Word (Word16, Word8)
import Lens.Micro ((^.))
import Types

getRegister :: Cpu s -> Registers -> ST s Word8
getRegister cpu reg = MV.read (cpu ^. registers) (regIdx reg)

addToRegister :: Cpu s -> Registers -> Word8 -> ST s ()
addToRegister cpu reg val = do
  currentVal <- getRegister cpu reg
  let newVal = currentVal + val
  MV.write (cpu ^. registers) (regIdx reg) newVal

setRegister :: Cpu s -> Registers -> Word8 -> ST s ()
setRegister cpu reg = MV.write (cpu ^. registers) (regIdx reg)

getCarryFlag :: Cpu s -> ST s Word8
getCarryFlag cpu = (.&. 0x10) <$> getRegister cpu RegF

fetchMemory :: Cpu s -> Int -> ST s Word8
fetchMemory cpu = MV.read (cpu ^. memory)

readPair :: Cpu s -> Registers -> Registers -> ST s Word16
readPair cpu highIndex lowIndex = do
  low <- getRegister cpu lowIndex
  high <- getRegister cpu highIndex
  return $ (fromIntegral high `shiftL` 8) .|. fromIntegral low

readHL :: Cpu s -> ST s Word16
readHL cpu = readPair cpu RegH RegL

readSP :: Cpu s -> ST s Word16
readSP cpu = do
  readSTRef (cpu ^. sp)

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

getSP :: Cpu s -> ST s Word16
getSP cpu = readSTRef (cpu ^. sp)

getZeroFlag :: Cpu s -> ST s Bool
getZeroFlag cpu = do
  flags <- getRegister cpu RegF
  return $ (flags .&. 0x80) /= 0

incPC :: Cpu s -> ST s ()
incPC cpu = do
  pcValue <- readSTRef (cpu ^. pc)
  writeSTRef (cpu ^. pc) (pcValue + 1)

regIdx :: Registers -> Int
regIdx = fromEnum