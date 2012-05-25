{-# LANGUAGE UnicodeSyntax #-}
module NClient.GUI (gui) where

import Control.Applicative ((<$>))
import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.Chan (readChan, writeChan)
import Control.Monad (forever, forM_, void)
import Data.IORef (newIORef, atomicModifyIORef)
import Graphics.Vty
import Graphics.Vty.Widgets.All

import NClient.Connect
import NClient.Message.Format
import qualified NClient.Message.History as H
import qualified NClient.Message.Split as S

gui ∷ (Input, Output) → IO ()
gui (i,o) = do
  history ← newIORef $ H.empty 10
  messages ← newList (getNormalAttr defaultContext)
  newMessage ← editWidget
  box ← vBox messages newMessage
  ui ← centered box
  fg ← newFocusGroup
  void $ addToFocusGroup fg newMessage
  c ← newCollection
  void $ addToCollection c ui fg
  -- Send message to server
  newMessage `onActivate` \this →
    getEditText this >>= toC2S >>= writeChan o
  --
  -- Add send message to history
  newMessage `onActivate` \this →
    getEditText this >>= \t → atomicModifyIORef history (\h → let α = H.prepend t h in (α, H.line α)) >>= setEditText this
  --
  -- Catch history movements
  newMessage `onKeyPressed` \this k m →
    case (k,m) of
      (KUp, []) → do
        t ← getEditText this
        t' ← atomicModifyIORef history $
          \h → let h' = H.next t h in (h', H.line h')
        setEditText this t'
        return True
      (KDown, []) → do
        t' ← atomicModifyIORef history $
          \h → let h' = H.previous h in (h', H.line h')
        setEditText this t'
        return True
      _ → return False
  --
  -- Read server messages when they come
  void . forkIO . forever $ readChan i >>= \m → fromS2C m >>= \s → do
    let addMessage f xs ys = textWidget f xs >>= addToList ys xs >> scrollDown ys
    schedule $
      do a:as ← S.words s . region_width <$> getCurrentSize messages
         addMessage (formatter Tail m) a messages
         forM_ as $ \γ → addMessage (formatter Full m) γ messages
    threadDelay 10000
  --
  runUi c defaultContext
