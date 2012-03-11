module Types where

data Message = Message Nick Text deriving (Show, Read)
newtype Nick = Nick String deriving (Show, Read)
newtype Text = Text String deriving (Show, Read)

