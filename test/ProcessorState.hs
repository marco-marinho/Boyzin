{-# LANGUAGE DeriveGeneric #-}

module ProcessorState where

import Data.Aeson
import Data.ByteString.Lazy qualified as B
import Data.Text as T
import GHC.Generics

data ProcessorState = ProcessorState
  { pc :: Int,
    sp :: Int,
    a :: Int,
    b :: Int,
    c :: Int,
    d :: Int,
    e :: Int,
    f :: Int,
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