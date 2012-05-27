{-# LANGUAGE UnicodeSyntax #-}
module Hach.Types
  ( Nick, Text, Timestamp
  , CMessage(..), SMessage(..)
  , C2S(..), S2C(..)
  ) where

import Data.Time

type Nick = String
type Text = String
type Timestamp = UTCTime

data S2C = S2C { text ∷ Text
               , messageType ∷ SMessage
               , time ∷ Timestamp
               } deriving (Read, Show)

data SMessage = SPlain Nick
              | SAction Nick
              | SSetNick Nick
              | SSystem
                deriving (Read, Show)

data C2S = C2S Text CMessage deriving (Read, Show)

data CMessage = CPlain
              | CAction
              | CSetNick
                deriving (Read, Show)
