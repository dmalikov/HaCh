{-# LANGUAGE UnicodeSyntax #-}
module NClient.GUI (gui) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.Chan (Chan, readChan, writeChan)
import Control.Monad (forever)
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
  forkIO . forever $ readChan i >>= fromS2C >>= \s → do
    schedule $ do
      addToList messages s =<< plainText s
      scrollDown messages
    threadDelay 100000
  runUi c defaultContext
