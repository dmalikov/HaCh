{-# LANGUAGE OverloadedStrings #-}

module NClient.Message.Format
  ( fromS2C, toC2S
  , Format(..), formatter
  ) where

import Control.Arrow (second)
import Data.Char (isSpace)
import Data.Monoid ((<>))
import qualified Data.Text as T
import Data.Time.Format (formatTime)
import Graphics.Vty.Attributes
import Graphics.Vty.Widgets.Text
import Graphics.Vty.Widgets.Util
import System.Exit (exitSuccess)
import System.Locale (defaultTimeLocale)
import Text.Trans.Tokenize

import Hach.Types

fromS2C :: S2C -> T.Text
fromS2C (S2C message (SPlain n) t) = "[" <> formatTime' t <> "] <" <> T.pack n <> ">: " <> message <> "\n"
fromS2C (S2C message (SAction n) t) = "[" <> formatTime' t <> "] *" <> T.pack n <> " " <> message <> "\n"
fromS2C (S2C message (SSetNick n) t) = "[" <> formatTime' t <> "] " <> T.pack n <> " " <> message <> "\n"
fromS2C (S2C message  SSystem t) = "[" <> formatTime' t <> "] ! " <> message <> "\n"

formatTime' :: Timestamp -> T.Text
formatTime' = T.pack . formatTime defaultTimeLocale "%T"

toC2S :: T.Text -> IO C2S
toC2S m = case format m of
  ("/exit", _) -> exitSuccess
  ("/nick", t) -> return $ C2S t CSetNick
  ("/me", t) -> return $ C2S t CAction
  _ -> return $ C2S (T.reverse . T.drop 1 $ T.reverse m) CPlain
  where format = second (T.reverse . dropSpaces . T.reverse . dropSpaces) . T.break isSpace . dropSpaces
        dropSpaces = T.dropWhile isSpace

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
