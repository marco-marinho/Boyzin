module Cpu where

import Control.Monad.ST
import Data.Bits
import Data.Vector.Unboxed.Mutable qualified as MV
import Data.Word (Word16, Word8)
import Instructions
import Lens.Micro ((^.))
import Types

getRegister :: Cpu s -> Int -> ST s Word8
getRegister cpu = MV.read (cpu ^. registers)

addToRegister :: Cpu s -> Int -> Word8 -> ST s ()
addToRegister cpu regIndex val = do
  currentVal <- getRegister cpu regIndex
  let newVal = currentVal + val
  MV.write (cpu ^. registers) regIndex newVal

setRegister :: Cpu s -> Int -> Word8 -> ST s ()
setRegister cpu = MV.write (cpu ^. registers)

getCarryFlag :: Cpu s -> ST s Word8
getCarryFlag cpu = (.&. 0x10) <$> getRegister cpu 5

fetchMemory :: Cpu s -> Int -> ST s Word8
fetchMemory cpu = MV.read (cpu ^. memory)

readPair :: Cpu s -> Int -> Int -> ST s Word16
readPair cpu highIndex lowIndex = do
  low <- getRegister cpu lowIndex
  high <- getRegister cpu highIndex
  return $ (fromIntegral high `shiftL` 8) .|. fromIntegral low

readHL :: Cpu s -> ST s Word16
readHL cpu = readPair cpu 7 8

setPair :: Cpu s -> Int -> Int -> Word16 -> ST s ()
setPair cpu highIndex lowIndex val = do
  let high = fromIntegral (val `shiftR` 8)
      low = fromIntegral (val .&. 0xFF)
  setRegister cpu highIndex high
  setRegister cpu lowIndex low

setHL :: Cpu s -> Word16 -> ST s ()
setHL cpu = setPair cpu 7 8

execute :: Cpu s -> Instruction -> ST s ()
execute cpu instruction = case instruction of
  ADDC_R8 target -> do
    (result, flags) <- addc <$> getRegister cpu 0 <*> getRegister cpu target <*> getCarryFlag cpu
    setRegister cpu 0 result
    setRegister cpu 5 flags
  ADDC_HL -> do
    hl <- readHL cpu
    memVal <- fetchMemory cpu (fromIntegral hl)
    (result, flags) <- addc <$> getRegister cpu 0 <*> pure memVal <*> getCarryFlag cpu
    setRegister cpu 0 result
    setRegister cpu 5 flags
  ADDC_N8 val -> do
    (result, flags) <- addc <$> getRegister cpu 0 <*> pure val <*> getCarryFlag cpu
    setRegister cpu 0 result
    setRegister cpu 5 flags
  ADD_R8 target -> do
    (result, flags) <- add <$> getRegister cpu 0 <*> getRegister cpu target
    setRegister cpu 0 result
    setRegister cpu 5 flags
  ADD_HL -> do
    hl <- readHL cpu
    memVal <- fetchMemory cpu (fromIntegral hl)
    (result, flags) <- add <$> getRegister cpu 0 <*> pure memVal
    setRegister cpu 0 result
    setRegister cpu 5 flags
  ADD_N8 val -> do
    (result, flags) <- add <$> getRegister cpu 0 <*> pure val
    setRegister cpu 0 result
    setRegister cpu 5 flags
  ADD_HL_R16 val -> do
    setHL cpu val
  _ -> return () -- Placeholder for unimplemented instructions