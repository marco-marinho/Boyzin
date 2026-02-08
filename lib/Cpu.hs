module Cpu where

import Control.Monad.ST
import CpuUtils
import Decoder
import Executor
import Types

fetchDecodeExecute :: Cpu s -> ST s ()
fetchDecodeExecute cpu = do
  instruction <- decodeInstruction cpu
  execute cpu instruction
  incPC cpu