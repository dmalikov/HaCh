module Types where

newtype ClientId = ClientId Int deriving (Eq, Ord, Read, Show)
data Message = Message Type Nick Text deriving (Read, Show)
data Type = Plain | Action | System deriving (Read, Show)
newtype Nick = Nick String deriving (Read, Show)
newtype Text = Text String deriving (Read, Show)
