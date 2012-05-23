{-# LANGUAGE UnicodeSyntax #-}
module NClient.GUI (gui) where

import Control.Applicative ((<$>))
import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.Chan (readChan, writeChan)
import Control.Monad (forever, forM_, void)
import Graphics.Vty.Widgets.All
import Graphics.Vty.DisplayRegion

import NClient.Connect
import NClient.Message.Format
import NClient.Message.Split as S

gui ∷ (Input, Output) → IO ()
gui (i,o) = do
  messages ← newList (getNormalAttr defaultContext)
  newMessage ← editWidget
  box ← vBox messages newMessage
  ui ← centered box
  fg ← newFocusGroup
  void $ addToFocusGroup fg newMessage
  c ← newCollection
  void $ addToCollection c ui fg
  newMessage `onActivate` \this →
    getEditText this >>= toC2S >>= writeChan o >> setEditText this " "
  void $ forkIO . forever $ readChan i >>= \m → fromS2C m >>= \s → do
    schedule $ do
      a:as ← S.words s . region_width <$> getCurrentSize messages
      addMessage (formatter Tail m) a messages
      forM_ as $ \γ → addMessage (formatter Full m) γ messages
    threadDelay 10000
  runUi c defaultContext
  where addMessage f xs ys = textWidget f xs >>= addToList ys xs >> scrollDown ys
