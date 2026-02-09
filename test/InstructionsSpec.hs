module InstructionsSpec where

import Control.Monad
import Data.Aeson
import Data.Bits qualified as Bits
import Data.ByteString.Lazy qualified as B
import Data.Word (Word16, Word8)
import Instructions
import Test.Hspec
import TestTools
import Text.Printf (printf)

main :: IO ()
main = hspec spec

compareProcessorStates :: ProcessorState -> ProcessorState -> Int -> Expectation
compareProcessorStates expected actual index = do
  if expected == actual
    then return ()
    else expectationFailure $ formatDiff expected actual index

spec :: Spec
spec = do
  describe "CPU Tests" $ do
    mapM_ makeTest [0x80 .. 0x8F]
  where
    makeTest opcode =
      it (printf "0x%02x" opcode) $ do
        fileContents <- B.readFile ("external/sm83/v1/" ++ printf "%02x" opcode ++ ".json")
        case decode fileContents :: Maybe [TestEntry] of
          Just testEntries ->
            forM_ (zip [1 ..] testEntries) $ \(index, testEntry) -> do
              compareProcessorStates (runTestEntry testEntry) (final testEntry) index
          Nothing -> expectationFailure "Failed to parse JSON file"