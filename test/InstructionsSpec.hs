module InstructionsSpec where

import Test.Hspec
import Instructions
import Data.Word (Word8, Word16)
import Data.Bits qualified as Bits

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "Arithmetic Logic Unit" $ do
        it "Add without carry" $ do
            let (result, flags) = add 10 20
            result `shouldBe` 30
            flags `shouldBe` 0x00
        it "Add with carry" $ do
            let (result, flags) = add 200 100
            result `shouldBe` 44
            (flags Bits..&. 0x10) `shouldBe` 0x10
        it "Add with half-carry" $ do
            let (result, flags) = add 0x0F 0x01
            result `shouldBe` 0x10
            (flags Bits..&. 0x20) `shouldBe` 0x20
        it "Add with carry and half-carry" $ do
            let (result, flags) = add 0xFF 0x01
            result `shouldBe` 0x00
            (flags Bits..&. 0x10) `shouldBe` 0x10
            (flags Bits..&. 0x20) `shouldBe` 0x20
        it "Add resulting in zero" $ do
            let (result, flags) = add 0x00 0x00
            result `shouldBe` 0x00
            (flags Bits..&. 0x80) `shouldBe` 0x80
        it "Add rollover to zero" $ do
            let (result, flags) = add 0xFF 0x01
            result `shouldBe` 0x00
            (flags Bits..&. 0x80) `shouldBe` 0x80
        it "Addc without carry" $ do
            let (result, flags) = addc 10 20 0
            result `shouldBe` 30
            flags `shouldBe` 0x00
            let (result2, flags2) = addc 10 20 1
            result2 `shouldBe` 31
            flags2 `shouldBe` 0x00
        it "Addc with carry" $ do
            let (result, flags) = addc 200 99 1
            result `shouldBe` 44
            (flags Bits..&. 0x10) `shouldBe` 0x10
            let (result2, flags2) = addc 200 100 0
            result2 `shouldBe` 44
            (flags2 Bits..&. 0x10) `shouldBe` 0x10
        it "Addc with half-carry" $ do
            let (result, flags) = addc 0x0F 0x00 0x01
            result `shouldBe` 0x10
            (flags Bits..&. 0x20) `shouldBe` 0x20
        it "Addc with carry and half-carry" $ do
            let (result, flags) = addc 0xFF 0x00 0x01
            result `shouldBe` 0x00
            (flags Bits..&. 0x10) `shouldBe` 0x10
            (flags Bits..&. 0x20) `shouldBe` 0x20
        it "Addc resulting in zero" $ do
            let (result, flags) = addc 0x00 0x00 0x00
            result `shouldBe` 0x00
            (flags Bits..&. 0x80) `shouldBe` 0x80
        it "Addc rollover to zero" $ do
            let (result, flags) = addc 0xFF 0x00 0x01
            result `shouldBe` 0x00
            (flags Bits..&. 0x80) `shouldBe` 0x80