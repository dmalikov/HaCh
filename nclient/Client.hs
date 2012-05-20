{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE ViewPatterns #-}
module Main (main) where

import Control.Applicative ((<$>))
import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.Chan (Chan, newChan, readChan, writeChan)
import Control.Exception (SomeException, catch, handle)
import Control.Monad (forever, void)
import Data.Char (isSpace)
import Data.Time.Clock (getCurrentTime)
import Data.Time.Format (formatTime)
import Graphics.Vty.Widgets.All
import Network
import Prelude hiding (catch)
import System.Environment (getArgs)
import System.Exit (exitFailure, exitSuccess)
import System.Locale (defaultTimeLocale)
import System.IO (hFlush, hGetLine, hPrint, hPutStrLn, hSetBuffering, BufferMode(LineBuffering))
import Text.Printf (printf)

import Hach.Types

import NClient.Args

type Input = Chan S2C
type Output = Chan C2S

main ∷ IO ()
main = do
  (ip, nick) ← parseArgs =<< getArgs
  i ← newChan
  o ← newChan
  initClient i o ip nick
  gui i o

initClient ∷ Input → Output → String → String → IO ()
initClient i o ip nick = do
  withSocketsDo . void $
    do h ← connectTo ip $ PortNumber 7123
       hSetBuffering h LineBuffering
       hPrint h $ CSetNick nick
       forkIO $ catch (inputThread h) $ \(_ ∷ SomeException) → do
         putStrLn "Server has closed the connection."
         exitFailure
       forkIO $ catch (outputThread h) $ \(_ ∷ SomeException) → do
         putStrLn $ nick ++ " has left."
         exitSuccess
  where inputThread h = forever $ hGetLine h >>= writeChan i . read
        outputThread h = forever $ readChan o >>= \m → hPrint h m

gui ∷ Input → Output → IO ()
gui i o = do
  messages ← plainText ""
  newMessage ← editWidget
  box ← vBox messages newMessage
  ui ← centered box
  fg ← newFocusGroup
  addToFocusGroup fg newMessage
  c ← newCollection
  addToCollection c ui fg
  newMessage `onActivate` \this → do
    t ← getEditText this
    writeChan o $ toC2S (init t)
    setEditText this " "
  forkIO . forever $ do
    m ← readChan i
    let fmt = "%H:%M:%S"
    t ← formatTime defaultTimeLocale fmt <$> getCurrentTime
    schedule $ setText messages (formatted t m)
    threadDelay 100000
  runUi c defaultContext

toC2S ∷ String → C2S
toC2S (break isSpace → ("/nick", t)) = CSetNick t
toC2S (break isSpace → ("/me", t)) = CAction t
toC2S t = CMessage t

formatted ∷ String → S2C → String
formatted t m = printf (fmt m) t (text m)
  where text ∷ S2C → String
        text (SMessage _ t) = t
        text (SAction _ t) = t
        text (SSetNick _ t) = t
        text (SSystem t) = t

        fmt ∷ S2C → String
        fmt (SMessage n _) = "[%s] <" ++ n ++ ">: %s\n"
        fmt (SAction n _) = "[%s] *" ++ n ++ " %s\n"
        fmt (SSetNick n _) = "[%s] "  ++ n ++ " %s\n"
        fmt (SSystem _) = "[%s] ! %s\n"
