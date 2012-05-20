{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE ViewPatterns #-}
module Main (main) where

import Control.Applicative ((<$>))
import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.Chan (Chan, readChan, writeChan)
import Control.Monad (forever)
import Data.Char (isSpace)
import Data.Time.Clock (getCurrentTime)
import Data.Time.Format (formatTime)
import Graphics.Vty.Widgets.All
import Hach.Types
import System.Environment (getArgs)
import System.Exit (exitFailure, exitSuccess)
import System.Locale (defaultTimeLocale)
import Text.Printf (printf)

import NClient.Args
import NClient.Connect

main ∷ IO ()
main = getArgs >>= parseArgs >>= connect >>= gui

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
  newMessage `onActivate` \this → do
    t ← getEditText this
    m ← toC2S (init t)
    writeChan o m
    setEditText this " "
  forkIO . forever $ do
    m ← readChan i
    let fmt = "%H:%M:%S"
    t ← formatTime defaultTimeLocale fmt <$> getCurrentTime
    let s = formatted t m
    schedule $ do
      addToList messages s =<< plainText s
      scrollDown messages
    threadDelay 100000
  runUi c defaultContext

toC2S ∷ String → IO C2S
toC2S (break isSpace → ("/exit", t)) = exitSuccess
toC2S (break isSpace → ("/nick", t)) = return $ CSetNick t
toC2S (break isSpace → ("/me", t)) = return $ CAction t
toC2S t = return $ CMessage t

formatted ∷ String → S2C → String
formatted t m = printf (fmt m) t (text m)
  where fmt ∷ S2C → String
        fmt (SMessage n _) = "[%s] <" ++ n ++ ">: %s\n"
        fmt (SAction n _) = "[%s] *" ++ n ++ " %s\n"
        fmt (SSetNick n _) = "[%s] "  ++ n ++ " %s\n"
        fmt (SSystem _) = "[%s] ! %s\n"
