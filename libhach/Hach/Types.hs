{-# LANGUAGE OverloadedStrings #-}
module Hach.Types
  ( Nick, Timestamp
  , CMessage(..), SMessage(..)
  , C2S(..), S2C(..)
  , fromS2C, toC2S
  ) where

import Control.Arrow (second)
import Data.Char (isSpace)
import Data.Monoid ((<>))
import Data.Text as T
import Data.Time
import System.Exit (exitSuccess)
import System.Locale (defaultTimeLocale)

type Nick = Text
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

fromS2C :: S2C -> Text
fromS2C (S2C message (SPlain n) t) = "[" <> formatTime' t <> "] <" <> n <> ">: " <> message <> "\n"
fromS2C (S2C message (SAction n) t) = "[" <> formatTime' t <> "] *" <> n <> " " <> message <> "\n"
fromS2C (S2C message (SSetNick n) t) = "[" <> formatTime' t <> "] " <> n <> " " <> message <> "\n"
fromS2C (S2C message  SSystem t) = "[" <> formatTime' t <> "] ! " <> message <> "\n"

formatTime' :: Timestamp -> Text
formatTime' = T.pack . formatTime defaultTimeLocale "%T"

toC2S :: T.Text -> IO C2S
toC2S m = case format m of
  ("/exit", _) -> exitSuccess
  ("/nick", t) -> return $ C2S t CSetNick
  ("/me", t) -> return $ C2S t CAction
  _ -> return $ C2S (T.reverse . T.drop 1 $ T.reverse m) CPlain
  where format = second (T.reverse . dropSpaces . T.reverse . dropSpaces) . T.break isSpace . dropSpaces
        dropSpaces = T.dropWhile isSpace
