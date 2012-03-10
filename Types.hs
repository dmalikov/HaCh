module Types where

import Text.ParserCombinators.Parsec

data Message = Message Nick Text Timestamp
newtype Nick = Nick String deriving Show
newtype Text = Text String deriving Show
newtype Timestamp = Timestamp Integer deriving Show

instance Show Message where
  show = pack

pack :: Message -> String
pack (Message (Nick nick) (Text text) (Timestamp timestamp)) = "[" ++ show timestamp ++ "] " ++ "<" ++ nick ++ ">: " ++ text

unpack :: String -> Message
unpack s = let message = parse messageP "" s in
  case message of
    Left _ -> undefined
    Right r -> r
    where
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
