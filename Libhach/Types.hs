module Libhach.Types where

data Message = Message Type Nick Text deriving (Read, Show)
data Type = Plain
          | Action
          | SetNick
          | System
          deriving (Read, Show)

newtype Nick = Nick String deriving (Read, Show)
newtype Text = Text String deriving (Read, Show)
