{-# LANGUAGE UnicodeSyntax #-}
module NClient.GUI (gui) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.Chan (Chan, readChan, writeChan)
import Control.Monad (forever)
import Graphics.Vty.Attributes
import Graphics.Vty.Widgets.All
import Hach.Types

import NClient.Connect
import NClient.Format

gui ∷ (Input, Output) → IO ()
gui (i,o) = do
  messages ← newList (getNormalAttr defaultContext)
  newMessage ← editWidget
  box ← vBox messages newMessage
  ui ← centered box
  fg ← newFocusGroup
  addToFocusGroup fg newMessage
  c ← newCollection
  addToCollection c ui fg
  newMessage `onActivate` \this →
    getEditText this >>= toC2S >>= writeChan o >> setEditText this " "
  forkIO . forever $ readChan i >>= \m → fromS2C m >>= \s → do
    schedule $ do
      addToList messages s =<< plainTextWidget m s
      scrollDown messages
    threadDelay 100000
  runUi c defaultContext

colors ∷ S2C → Attr
colors (SAction _ _) = Attr Default (SetTo green) Default
colors (SSetNick _ _) = Attr Default (SetTo yellow) Default
colors (SSystem _) = Attr Default (SetTo blue) Default
colors _ = getNormalAttr defaultContext

plainTextWidget ∷ S2C → String → IO (Widget FormattedText)
plainTextWidget m s = plainTextWithAttrs [(s, colors m)]
