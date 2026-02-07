{-# LANGUAGE TemplateHaskell #-}

module Types where

import Data.Vector.Unboxed.Mutable qualified as MV
import Data.Word (Word16, Word8)
import Lens.Micro.TH (makeLenses)

data Instruction
  = ADDC_R8 Int
  | ADDC_HL
  | ADDC_N8 Word8
  | ADD_R8 Int
  | ADD_HL
  | ADD_N8 Word8
  | ADD_HL_R16 Word16

type Registers s = MV.MVector s Word8

data Cpu s = Cpu
  { _registers :: Registers s,
    _memory :: MV.MVector s Word8
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
