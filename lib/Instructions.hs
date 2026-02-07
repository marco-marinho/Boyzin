module Instructions where

import Data.Bits
import Data.Word (Word8)
import Types

addc :: Word8 -> Word8 -> Word8 -> (Word8, Word8)
addc x y c = (result, flags)
  where
    result = x + y + c
    carry = result < x
    half_carry = (x .&. 0xF) + (y .&. 0xF) > 0xF
    flags = flagsToWord8 (result == 0) False half_carry carry

add :: Word8 -> Word8 -> (Word8, Word8)
add x y = (result, flags)
  where
    result = x + y
    carry = result < x
    half_carry = (x .&. 0xF) + (y .&. 0xF) > 0xF
    flags = flagsToWord8 (result == 0) False half_carry carry

and :: Word8 -> Word8 -> Word8
and x y = x .&. y

ccf :: Word8 -> Word8
ccf flags = flags `xor` 0x10

sub :: Word8 -> Word8 -> (Word8, Word8)
sub x y = (result, flags)
  where
    result = x - y
    carry = x < y
    half_carry = (x .&. 0xF) < (y .&. 0xF)
    zero = result == 0
    flags = flagsToWord8 zero True half_carry carry

flagsToWord8 :: Bool -> Bool -> Bool -> Bool -> Word8
flagsToWord8 zero substract halfCarry carry =
  (if zero then 0x80 else 0)
    .|. (if substract then 0x40 else 0)
    .|. (if halfCarry then 0x20 else 0)
    .|. (if carry then 0x10 else 0)

flagsFromWord8 :: Word8 -> FlagsRegister
flagsFromWord8 w =
  FlagsRegister ((w .&. 0x80) /= 0) ((w .&. 0x40) /= 0) ((w .&. 0x20) /= 0) ((w .&. 0x10) /= 0)