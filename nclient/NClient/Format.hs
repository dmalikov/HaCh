{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE ViewPatterns #-}
module NClient.Format (fromS2C, toC2S) where

import Control.Applicative ((<$>))
import Control.Arrow (second)
import Data.Char (isSpace)
import Data.Time.Clock (getCurrentTime)
import Data.Time.Format (formatTime)
import Hach.Types
import System.Exit (exitFailure, exitSuccess)
import System.Locale (defaultTimeLocale)
import Text.Printf (printf)

fromS2C ∷ S2C → IO String
fromS2C m = formatMessage m . formatTime defaultTimeLocale timeFormat <$> getCurrentTime
  where timeFormat = "%H:%M:%S"
        messageFormat (SMessage n _) = "[%s] <" ++ n ++ ">: %s\n"
        messageFormat (SAction n _) = "[%s] *" ++ n ++ " %s\n"
        messageFormat (SSetNick n _) = "[%s] "  ++ n ++ " %s\n"
        messageFormat (SSystem _) = "[%s] ! %s\n"
        formatMessage m t = printf (messageFormat m) t (text m)

toC2S ∷ String → IO C2S
toC2S (format → ("/exit", t)) = exitSuccess
toC2S (format → ("/nick", t)) = return $ CSetNick t
toC2S (format → ("/me", t)) = return $ CAction t
toC2S t = return . CMessage . reverse . drop 1 . reverse $ t

format = second (reverse . dropSpaces . reverse . dropSpaces) . break isSpace . dropSpaces
  where dropSpaces = dropWhile isSpace

