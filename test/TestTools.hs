{-# LANGUAGE DeriveGeneric #-}

module TestTools where

import CPU (fetchDecodeExecute, makeCPU)
import CPU.Interface
import Control.Monad.ST
import Data.Aeson
import Data.ByteString.Lazy qualified as B
import Data.Text qualified as T
import Data.Word (Word16)
import GHC.Generics
import Text.Printf (printf)
import Types qualified as Ty

data ProcessorStateBefore = ProcessorStateBefore
  { bpc :: Int,
    bsp :: Int,
    ba :: Int,
    bb :: Int,
    bc :: Int,
    bd :: Int,
    be :: Int,
    bf :: Int,
    bh :: Int,
    bl :: Int,
    bime :: Int,
    bie :: Int,
    bram :: [(Word16, Int)]
  }
  deriving (Show, Generic, Eq)

instance FromJSON ProcessorStateBefore where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = drop 1}

data ProcessorStateAfter = ProcessorStateAfter
  { apc :: Int,
    asp :: Int,
    aa :: Int,
    ab :: Int,
    ac :: Int,
    ad :: Int,
    ae :: Int,
    af :: Int,
    ah :: Int,
    al :: Int,
    aime :: Int,
    aram :: [(Word16, Int)]
  }
  deriving (Show, Generic, Eq)

instance FromJSON ProcessorStateAfter where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = drop 1}

data TestEntry = TestEntry
  { name :: T.Text,
    initial :: ProcessorStateBefore,
    final :: ProcessorStateAfter,
    cycles :: [(Int, Int, String)]
  }
  deriving (Show, Generic)

instance FromJSON TestEntry

cpuToProcessorState :: Ty.Cpu -> [Word16] -> IO ProcessorStateAfter
cpuToProcessorState cpu ramAddresses = do
  (ProcessorStateAfter . fromIntegral <$> readPC cpu)
    <*> (fromIntegral <$> readSP cpu)
    <*> (fromIntegral <$> readRegister cpu Ty.RegA)
    <*> (fromIntegral <$> readRegister cpu Ty.RegB)
    <*> (fromIntegral <$> readRegister cpu Ty.RegC)
    <*> (fromIntegral <$> readRegister cpu Ty.RegD)
    <*> (fromIntegral <$> readRegister cpu Ty.RegE)
    <*> (fromIntegral <$> readRegister cpu Ty.RegF)
    <*> (fromIntegral <$> readRegister cpu Ty.RegH)
    <*> (fromIntegral <$> readRegister cpu Ty.RegL)
    <*> (fromIntegral <$> readRegister cpu Ty.RegIME)
    <*> mapM (\addr -> (fromIntegral addr,) . fromIntegral <$> readMemory cpu addr) ramAddresses

cpuFromProcessorState :: ProcessorStateBefore -> Ty.Cpu -> IO ()
cpuFromProcessorState ProcessorStateBefore {bpc, bsp, ba, bb, bc, bd, be, bf, bh, bl, bime, bie, bram} cpu = do
  setPC cpu (fromIntegral bpc)
  setSP cpu (fromIntegral bsp)
  setRegister cpu Ty.RegA (fromIntegral ba)
  setRegister cpu Ty.RegB (fromIntegral bb)
  setRegister cpu Ty.RegC (fromIntegral bc)
  setRegister cpu Ty.RegD (fromIntegral bd)
  setRegister cpu Ty.RegE (fromIntegral be)
  setRegister cpu Ty.RegF (fromIntegral bf)
  setRegister cpu Ty.RegH (fromIntegral bh)
  setRegister cpu Ty.RegL (fromIntegral bl)
  setRegister cpu Ty.RegIME (fromIntegral bime)
  setRegister cpu Ty.RegIE (fromIntegral bie)
  mapM_ (\(addr, val) -> setMemory cpu (fromIntegral addr) (fromIntegral val)) bram

runTestEntry :: TestEntry -> IO ProcessorStateAfter
runTestEntry TestEntry {initial, final, cycles} = do
  cpu <- makeCPU
  cpuFromProcessorState initial cpu
  fetchDecodeExecute cpu
  let addressesToCheck = map fst (aram final)
  cpuToProcessorState cpu addressesToCheck

formatDiff :: ProcessorStateAfter -> ProcessorStateAfter -> Int -> String
formatDiff expected actual index =
  unlines $
    ("State Mismatch at entry " ++ show index ++ ":")
      : concat
        [ ["  PC: " ++ diff (apc expected) (apc actual) | apc expected /= apc actual],
          ["  SP: " ++ diff (asp expected) (asp actual) | asp expected /= asp actual],
          ["  A:  " ++ diff (aa expected) (aa actual) | aa expected /= aa actual],
          ["  B:  " ++ diff (ab expected) (ab actual) | ab expected /= ab actual],
          ["  C:  " ++ diff (ac expected) (ac actual) | ac expected /= ac actual],
          ["  D:  " ++ diff (ad expected) (ad actual) | ad expected /= ad actual],
          ["  E:  " ++ diff (ae expected) (ae actual) | ae expected /= ae actual],
          ["  F:  " ++ diff (af expected) (af actual) | af expected /= af actual],
          ["  H:  " ++ diff (ah expected) (ah actual) | ah expected /= ah actual],
          ["  L:  " ++ diff (al expected) (al actual) | al expected /= al actual],
          ["  IME: " ++ diff (aime expected) (aime actual) | aime expected /= aime actual],
          ["  RAM: " ++ show (aram expected) ++ " vs " ++ show (aram actual) | aram expected /= aram actual]
        ]
  where
    diff = printf "Expected 0x%02x, Got 0x%02x"