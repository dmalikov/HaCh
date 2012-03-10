module Types where

import Control.Applicative ((<$>), (<*>), (*>))
import Text.ParserCombinators.Parsec

data Message = Message Nick Text
newtype Nick = Nick String deriving Show
newtype Text = Text String deriving Show

instance Show Message where
  show = pack

instance Read Message where
  readsPrec _ s = case parse messageP "" s of
                    Left _ -> []
                    Right m -> [(m, "")]

pack :: Message -> String
pack (Message (Nick nick) (Text text)) = "<" ++ nick ++ ">: " ++ text

messageP :: GenParser Char st Message
messageP = do
  messagify <$> (char '<' *> many1 (noneOf ">")) <*> ( string ">:" *> many1 anyChar)
    where messagify n t = Message (Nick n) (Text t)