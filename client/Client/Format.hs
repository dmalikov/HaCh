{-# LANGUAGE OverloadedStrings #-}
module Client.Format where

import Data.Monoid ((<>))
import Data.Text (pack, Text)

import Data.Time.Format
import System.Locale

import Hach.Types

format :: S2C -> Text
format (S2C message (SPlain n) t) = "[" <> formatTime' t <> "] <" <> pack n <> ">: " <> message <> "\n"
format (S2C message (SAction n) t) = "[" <> formatTime' t <> "] *" <> pack n <> " " <> message <> "\n"
format (S2C message (SSetNick n) t) = "[" <> formatTime' t <> "] " <> pack n <> " " <> message <> "\n"
format (S2C message  SSystem t) = "[" <> formatTime' t <> "] " <> message <> "\n"

formatTime' :: Timestamp -> Text
formatTime' = pack . formatTime defaultTimeLocale "%T"

commandAction :: String
commandAction = "/me "

commandSetNick :: String
commandSetNick = "/nick "
