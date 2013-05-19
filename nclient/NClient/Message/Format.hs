module NClient.Message.Format
  ( fromS2C, toC2S
  , Format(..), formatter
  ) where

import Control.Arrow (second)
import Data.Char (isSpace)
import Data.Time.Format (formatTime)
import Graphics.Vty.Attributes
import Graphics.Vty.Widgets.Text
import Graphics.Vty.Widgets.Util
import Hach.Types
import System.Exit (exitSuccess)
import System.Locale (defaultTimeLocale)
import Text.Printf (printf)
import Text.Trans.Tokenize

fromS2C :: S2C -> String
fromS2C m = printf (format $ messageType m) (formatTime defaultTimeLocale "%T" $ time m) (text m)
  where format (SPlain n) = "[%s] <" ++ n ++ ">: %s\n"
        format (SAction n) = "[%s] *" ++ n ++ " %s\n"
        format (SSetNick n) = "[%s] "  ++ n ++ " %s\n"
        format SSystem = "[%s] ! %s\n"

toC2S :: String -> IO C2S
toC2S m = case format m of
  ("/exit", _) -> exitSuccess
  ("/nick", t) -> return $ C2S t CSetNick
  ("/me", t) -> return $ C2S t CAction
  _ -> return $ C2S (reverse . drop 1 $ reverse m) CPlain
  where format = second (reverse . dropSpaces . reverse . dropSpaces) . break isSpace . dropSpaces
        dropSpaces = dropWhile isSpace

data Format = Full | Tail

formatter :: Format -> S2C -> Formatter
formatter f s2c = case f of
  Full -> Formatter $ \_ -> return . colorizeStream
  Tail -> Formatter $ \_ -> return . colorizeStreamTail
  where colorizeStream = TS . map colorizeStreamEntity . streamEntities
        colorizeStreamTail ts = let x:xs = streamEntities ts
                                in TS $ x:map colorizeStreamEntity xs
        colorizeStreamEntity (T token) = T $ colorizeToken token
        colorizeStreamEntity NL = NL
        colorizeToken ws@(WS {}) = ws
        colorizeToken s = s {tokenAttr = attr}

        attr :: Attr
        attr = case messageType s2c of
          SAction {} -> fgColor green
          SSetNick {} -> fgColor yellow
          SSystem {} -> fgColor blue
          _ -> def_attr
