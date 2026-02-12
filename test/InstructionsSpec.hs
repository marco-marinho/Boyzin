module InstructionsSpec where

import Control.Monad
import Data.Aeson
import Data.Bits qualified as Bits
import Data.ByteString.Lazy qualified as B
import Data.Word (Word16, Word8)
import Test.Hspec
import TestTools
import Text.Printf (printf)

main :: IO ()
main = hspec spec

compareProcessorStates :: IO ProcessorState -> ProcessorState -> Int -> Expectation
compareProcessorStates expected actual index = do
  expectedState <- expected
  if expectedState == actual
    then return ()
    else expectationFailure $ formatDiff expectedState actual index

spec :: Spec
spec = do
  describe "CPU Tests" $ do
    forM_ [0x00 .. 0xCA] $ \opcode -> do
      testEntries <- runIO $ loadTestData opcode

      it (printf "0x%02x" opcode) $ do
        forM_ (zip [1 ..] testEntries) $ \(index, testEntry) -> do
          compareProcessorStates (runTestEntry testEntry) (final testEntry) index

  describe "CPU Tests - CB Prefix" $ do
    forM_ [0x00 .. 0xFF] $ \opcode -> do
      testEntries <- runIO $ loadTestDataPrefixed opcode

      it (printf "0xcb%02x" opcode) $ do
        forM_ (zip [1 ..] testEntries) $ \(index, testEntry) -> do
          compareProcessorStates (runTestEntry testEntry) (final testEntry) index

loadTestData :: Word8 -> IO [TestEntry]
loadTestData opcode = do
  let path = printf "external/sm83/v1/%02x.json" opcode
  fileContents <- B.readFile path
  case decode fileContents of
    Just entries -> return entries
    Nothing -> return []

loadTestDataPrefixed :: Word8 -> IO [TestEntry]
loadTestDataPrefixed opcode = do
  let path = printf "external/sm83/v1/cb %02x.json" opcode
  fileContents <- B.readFile path
  case decode fileContents of
    Just entries -> return entries
    Nothing -> return []