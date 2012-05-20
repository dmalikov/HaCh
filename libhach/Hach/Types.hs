module Hach.Types where

type Nick = String
type Text = String

data S2C = SMessage Nick Text
         | SAction Nick Text
         | SSetNick Nick Text
         | SSystem Text
           deriving (Read, Show)

data C2S = CMessage Text
         | CAction Text
         | CSetNick Text
           deriving (Read, Show)
