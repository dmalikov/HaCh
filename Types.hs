module Types where

data Message = Message Type Nick Text deriving (Show, Read)
data Type = Plain | Action | System deriving (Show, Read)
newtype Nick = Nick String deriving (Show, Read)
newtype Text = Text String deriving (Show, Read)

