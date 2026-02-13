{-# LANGUAGE TemplateHaskell #-}

module Types where

import Data.IORef
import Data.Vector.Unboxed.Mutable qualified as MV
import Data.Word (Word16, Word8)
import Lens.Micro.TH (makeLenses)

data Registers = RegA | RegB | RegC | RegD | RegE | RegF | RegH | RegL | RegIME | RegIE
  deriving (Show, Eq, Enum, Bounded)

data Registers16 = RegAF | RegBC | RegDE | RegHL | RegSP
  deriving (Show, Eq)

data Cpu = Cpu
  { _registers :: !(MV.IOVector Word8),
    _memory :: !(MV.IOVector Word8),
    _pc :: !(IORef Word16),
    _sp :: !(IORef Word16),
    _halted :: !(IORef Bool),
    _aboutToEI :: !(IORef Bool)
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
