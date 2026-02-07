{-# LANGUAGE TemplateHaskell #-}

module Types where

import Data.Vector.Unboxed.Mutable qualified as MV
import Data.STRef
import Data.Word (Word16, Word8)
import Lens.Micro.TH (makeLenses)
import Data.Int (Int8)

data Registers = RegA | RegB | RegC | RegD | RegE | RegF | RegH | RegL

data Instruction
  = ADDC_A_R8 Registers
  | ADDC_A_HL
  | ADDC_A_N8 Word8
  | ADD_A_R8 Registers
  | ADD_A_HL
  | ADD_A_N8 Word8
  | ADD_HL_R16 Word16
  | ADD_HL_SP
  | ADD_SP_E8 Int8

data Cpu s = Cpu
  { _registers :: MV.MVector s Word8,
    _memory :: MV.MVector s Word8,
    _pc :: STRef s Word16,
    _sp :: STRef s Word16
  }

data FlagsRegister = FlagsRegister
  { _zeroFlag :: Bool,
    _subtractFlag :: Bool,
    _halfCarryFlag :: Bool,
    _carryFlag :: Bool
  }
  deriving (Show)

makeLenses ''FlagsRegister
makeLenses ''Cpu
