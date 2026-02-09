{-# LANGUAGE TemplateHaskell #-}

module Types where

import Data.STRef
import Data.Vector.Unboxed.Mutable qualified as MV
import Data.Word (Word16, Word8)
import Lens.Micro.TH (makeLenses)

data Registers = RegA | RegB | RegC | RegD | RegE | RegF | RegH | RegL
  deriving (Show, Eq, Enum, Bounded)

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
