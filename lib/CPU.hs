module CPU (fetchDecodeExecute, makeCPU) where

import CPU.Decoder (decodeInstruction)
import CPU.Executor (executeInstruction)
import Data.IORef (newIORef)
import Data.Vector.Unboxed.Mutable qualified as MV
import Types (Cpu (Cpu))

fetchDecodeExecute :: Cpu -> IO ()
fetchDecodeExecute !cpu = do
  instruction <- decodeInstruction cpu
  executeInstruction cpu instruction

makeCPU :: IO Cpu
makeCPU = do
  initRegisters <- MV.replicate 10 0
  initMem <- MV.replicate 65536 0
  initPC <- newIORef 0
  initSP <- newIORef 0
  initHalted <- newIORef False
  return $ Cpu initRegisters initMem initPC initSP initHalted