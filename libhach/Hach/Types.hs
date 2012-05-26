{-# LANGUAGE UnicodeSyntax #-}
module Hach.Types
  ( Nick, Text, Timestamp, Message(..), CMessage(..), SMessage(..), C2S(..), S2C(..)
  , time
  ) where

import Data.Time

type Nick = String
type Text = String
type Timestamp = UTCTime

data S2C = S2C Text SMessage Timestamp deriving (Read, Show)

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

class Message α where
  text ∷ α → String

instance Message S2C where
  text (S2C τ _ _) = τ

instance Message C2S where
  text (C2S τ _) = τ

time ∷ S2C → UTCTime
time (S2C _ _ τ) = τ
