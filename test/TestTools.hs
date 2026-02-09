{-# LANGUAGE DeriveGeneric #-}

module TestTools where

import CPU (fetchDecodeExecute, makeCPU)
import CPU.Interface
import Control.Monad.ST
import Data.Aeson
import Data.ByteString.Lazy qualified as B
import Data.Text qualified as T
import GHC.Generics
import Text.Printf (printf)
import Types qualified as Ty

data ProcessorState = ProcessorState
  { pc :: Int,
    sp :: Int,
    a :: Int,
    b :: Int,
    c :: Int,
    d :: Int,
    e :: Int,
    f :: Int,
    h :: Int,
    l :: Int,
    ime :: Maybe Int,
    ie :: Maybe Int,
    ram :: [(Int, Int)]
  }
  deriving (Show, Generic)

instance ToJSON ProcessorState

instance FromJSON ProcessorState

data TestEntry = TestEntry
  { name :: T.Text,
    initial :: ProcessorState,
    final :: ProcessorState,
    cycles :: [(Int, Int, String)]
  }
  deriving (Show, Generic)

instance ToJSON TestEntry

instance FromJSON TestEntry

cpuToProcessorState :: Ty.Cpu s -> [Int] -> ST s ProcessorState
cpuToProcessorState cpu ramAddresses = do
  (ProcessorState . fromIntegral <$> readPC cpu)
    <*> (fromIntegral <$> readSP cpu)
    <*> (fromIntegral <$> readRegister cpu Ty.RegA)
    <*> (fromIntegral <$> readRegister cpu Ty.RegB)
    <*> (fromIntegral <$> readRegister cpu Ty.RegC)
    <*> (fromIntegral <$> readRegister cpu Ty.RegD)
    <*> (fromIntegral <$> readRegister cpu Ty.RegE)
    <*> (fromIntegral <$> readRegister cpu Ty.RegF)
    <*> (fromIntegral <$> readRegister cpu Ty.RegH)
    <*> (fromIntegral <$> readRegister cpu Ty.RegL)
    <*> pure Nothing
    <*> pure Nothing
    <*> mapM (\addr -> (fromIntegral addr,) . fromIntegral <$> readMemory cpu addr) ramAddresses

cpuFromProcessorState :: ProcessorState -> Ty.Cpu s -> ST s ()
cpuFromProcessorState ProcessorState {pc, sp, a, b, c, d, e, f, h, l, ime, ie, ram} cpu = do
  setPC cpu (fromIntegral pc)
  setSP cpu (fromIntegral sp)
  setRegister cpu Ty.RegA (fromIntegral a)
  setRegister cpu Ty.RegB (fromIntegral b)
  setRegister cpu Ty.RegC (fromIntegral c)
  setRegister cpu Ty.RegD (fromIntegral d)
  setRegister cpu Ty.RegE (fromIntegral e)
  setRegister cpu Ty.RegF (fromIntegral f)
  setRegister cpu Ty.RegH (fromIntegral h)
  setRegister cpu Ty.RegL (fromIntegral l)
  mapM_ (\(addr, val) -> setMemory cpu (fromIntegral addr) (fromIntegral val)) ram

runTestEntry :: TestEntry -> ProcessorState
runTestEntry TestEntry {initial, final, cycles} = runST $ do
  cpu <- makeCPU
  cpuFromProcessorState initial cpu
  fetchDecodeExecute cpu
  let addressesToCheck = map fst (ram final)
  cpuToProcessorState cpu addressesToCheck

instance Eq ProcessorState where
  (==) expected actual =
    pc actual == pc expected
      && sp actual == sp expected
      && a actual == a expected
      && b actual == b expected
      && c actual == c expected
      && d actual == d expected
      && e actual == e expected
      && f actual == f expected
      && h actual == h expected
      && l actual == l expected
      && ram actual == ram expected

formatDiff :: ProcessorState -> ProcessorState -> Int -> String
formatDiff expected actual index =
  unlines $
    ("State Mismatch at entry " ++ show index ++ ":")
      : concat
        [ ["  PC: " ++ diff (pc expected) (pc actual) | pc expected /= pc actual],
          ["  SP: " ++ diff (sp expected) (sp actual) | sp expected /= sp actual],
          ["  A:  " ++ diff (a expected) (a actual) | a expected /= a actual],
          ["  B:  " ++ diff (b expected) (b actual) | b expected /= b actual],
          ["  C:  " ++ diff (c expected) (c actual) | c expected /= c actual],
          ["  D:  " ++ diff (d expected) (d actual) | d expected /= d actual],
          ["  E:  " ++ diff (e expected) (e actual) | e expected /= e actual],
          ["  F:  " ++ diff (f expected) (f actual) | f expected /= f actual],
          ["  H:  " ++ diff (h expected) (h actual) | h expected /= h actual],
          ["  L:  " ++ diff (l expected) (l actual) | l expected /= l actual],
          ["  RAM: " ++ show (ram expected) ++ " vs " ++ show (ram actual) | ram expected /= ram actual]
        ]
  where
    diff = printf "Expected 0x%02x, Got 0x%02x"