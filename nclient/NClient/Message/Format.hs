{-# LANGUAGE OverloadedStrings #-}

module NClient.Message.Format
  ( fromS2C, toC2S
  , Format(..), formatter
  ) where

import Graphics.Vty.Attributes
import Graphics.Vty.Widgets.Text
import Graphics.Vty.Widgets.Util
import Text.Trans.Tokenize

import Hach.Types

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
