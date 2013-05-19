module Hach.Types
  ( Nick, Timestamp
  , CMessage(..), SMessage(..)
  , C2S(..), S2C(..)
  ) where

import Data.Time
import Data.Text

type Nick = String
type Timestamp = UTCTime

data S2C = S2C { text :: Text
               , messageType :: SMessage
               , time :: Timestamp
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
