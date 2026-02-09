module Main where

import InstructionsSpec qualified
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "Instructions" InstructionsSpec.spec