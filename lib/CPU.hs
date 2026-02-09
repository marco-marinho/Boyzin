module CPU (fetchDecodeExecute, makeCPU) where

import CPU.Decoder (decodeInstruction)
import CPU.Executor (executeInstruction)
import CPU.Interface (incPC)
import Control.Monad.ST (ST)
import Data.STRef (newSTRef)
import Data.Vector.Unboxed.Mutable qualified as MV
import Types (Cpu (Cpu))

fetchDecodeExecute :: Cpu s -> ST s ()
fetchDecodeExecute cpu = do
  instruction <- decodeInstruction cpu
  executeInstruction cpu instruction
  incPC cpu

makeCPU :: ST s (Cpu s)
makeCPU = do
  initRegisters <- MV.replicate 8 0
  initMem <- MV.replicate 65536 0
  initPC <- newSTRef 0
  initSP <- newSTRef 0
  return $ Cpu initRegisters initMem initPC initSP