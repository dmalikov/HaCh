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

fromS2C ∷ S2C → String
fromS2C m = do
  printf (messageFormat m) (formatTime defaultTimeLocale timeFormat (time m)) (text m)
  where timeFormat = "%H:%M:%S"
        messageFormat (S2C _ (SPlain n) _) = "[%s] <" ++ n ++ ">: %s\n"
        messageFormat (S2C _ (SAction n) _) = "[%s] *" ++ n ++ " %s\n"
        messageFormat (S2C _ (SSetNick n) _) = "[%s] "  ++ n ++ " %s\n"
        messageFormat (S2C _ SSystem _) = "[%s] ! %s\n"

toC2S ∷ String → IO C2S
toC2S (format → ("/exit", _)) = exitSuccess
toC2S (format → ("/nick", t)) = return $ C2S t CSetNick
toC2S (format → ("/me", t)) = return $ C2S t CAction
toC2S t = return $ C2S (reverse . drop 1 . reverse $ t) CPlain

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
          (S2C _ (SSetNick _) _) → fgColor yellow
          (S2C _ SSystem _) → fgColor blue
          _ → def_attr
