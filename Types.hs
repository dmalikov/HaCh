module Types where

import Text.ParserCombinators.Parsec

data Message = Message Nick Text Timestamp
newtype Nick = Nick String deriving Show
newtype Text = Text String deriving Show
newtype Timestamp = Timestamp Integer deriving Show

instance Show Message where
  show = pack

instance Read Message where
  readsPrec _ s = case (parse messageP "" s) of
                    Left _ -> []
                    Right m -> [(m, "")]

pack :: Message -> String
pack (Message (Nick nick) (Text text) (Timestamp timestamp)) = "[" ++ show timestamp ++ "] " ++ "<" ++ nick ++ ">: " ++ text

messageP :: GenParser Char st Message
messageP = do
  char '['
  timestamp <- many1 digit
  char ']'
  space
  char '<'
  nick <- many1 $ noneOf ['>']
  string ">: "
  text <- many1 anyChar
  return $ Message (Nick nick) (Text text) (Timestamp $ read timestamp)
