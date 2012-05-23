{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE ViewPatterns #-}
module NClient.Message.Format
  ( fromS2C, toC2S
  , Format(..), formatter
  ) where

import Control.Applicative ((<$>))
import Control.Arrow (second)
import Data.Char (isSpace)
import Data.Time.Clock (getCurrentTime)
import Data.Time.Format (formatTime)
import Graphics.Vty.Attributes
import Graphics.Vty.Widgets.Text
import Graphics.Vty.Widgets.Util
import Hach.Types
import System.Exit (exitSuccess)
import System.Locale (defaultTimeLocale)
import Text.Printf (printf)
import Text.Trans.Tokenize

fromS2C ∷ S2C → IO String
fromS2C m = formatMessage . formatTime defaultTimeLocale timeFormat <$> getCurrentTime
  where timeFormat = "%H:%M:%S"
        messageFormat (SMessage n _) = "[%s] <" ++ n ++ ">: %s\n"
        messageFormat (SAction n _) = "[%s] *" ++ n ++ " %s\n"
        messageFormat (SSetNick n _) = "[%s] "  ++ n ++ " %s\n"
        messageFormat (SSystem _) = "[%s] ! %s\n"
        formatMessage t = printf (messageFormat m) t (text m)

toC2S ∷ String → IO C2S
toC2S (format → ("/exit", _)) = exitSuccess
toC2S (format → ("/nick", t)) = return $ CSetNick t
toC2S (format → ("/me", t)) = return $ CAction t
toC2S t = return . CMessage . reverse . drop 1 . reverse $ t

format = second (reverse . dropSpaces . reverse . dropSpaces) . break isSpace . dropSpaces
  where dropSpaces = dropWhile isSpace

data Format = Full | Tail

formatter ∷ Format → S2C → Formatter
formatter f s2c = case f of
  Full → Formatter $ \_ → return . colorizeStream
  Tail → Formatter $ \_ → return . colorizeStreamTail
  where colorizeStream = TS . map colorizeStreamEntity . streamEntities
        colorizeStreamTail ts = let x:xs = streamEntities ts
                                in TS $ x:map colorizeStreamEntity xs
        colorizeStreamEntity (T token) = T $ colorizeToken token
        colorizeStreamEntity NL = NL
        colorizeToken ws@(WS {}) = ws
        colorizeToken s = s {tokenAttr = attr}

        attr ∷ Attr
        attr = case s2c of
          (SAction {}) → fgColor green
          (SSetNick {}) → fgColor yellow
          (SSystem {}) → fgColor blue
          _ → def_attr
