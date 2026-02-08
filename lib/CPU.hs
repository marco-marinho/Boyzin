module CPU (fetchDecodeExecute, makeCPU) where

import CPU.Decoder
import CPU.Executor
import CPU.Interface
import Control.Monad.ST
import Data.STRef
import Data.Vector.Unboxed.Mutable qualified as MV
import Types

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