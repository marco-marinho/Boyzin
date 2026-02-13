module CPU (fetchDecodeExecute, makeCPU) where

import CPU.Decoder (decodeInstruction)
import CPU.Executor (executeInstruction)
import CPU.Interface (isPendingEI, setIME, setPendingEI)
import Control.Monad (when)
import Data.IORef (newIORef)
import Data.Vector.Unboxed.Mutable qualified as MV
import Types (Cpu (Cpu))

fetchDecodeExecute :: Cpu -> IO ()
fetchDecodeExecute !cpu = do
  instruction <- decodeInstruction cpu
  shouldEI <- isPendingEI cpu
  executeInstruction cpu instruction
  when shouldEI $ do
    setPendingEI cpu False
    setIME cpu

makeCPU :: IO Cpu
makeCPU = do
  initRegisters <- MV.replicate 10 0
  initMem <- MV.replicate 65536 0
  initPC <- newIORef 0
  initSP <- newIORef 0
  initHalted <- newIORef False
  initPendingEI <- newIORef False
  return $ Cpu initRegisters initMem initPC initSP initHalted initPendingEI