module Hach.Types where

data S2C = SMessage Nick Text
         | SAction Nick Text
         | SSetNick Nick Text
         | SSystem Text
  deriving (Read, Show)

data C2S = CMessage Text
         | CAction Text
         | CSetNick Text
  deriving (Read, Show)

newtype Nick = Nick String deriving (Read, Show)
newtype Text = Text String deriving (Read, Show)
