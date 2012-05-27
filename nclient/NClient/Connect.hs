{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnicodeSyntax #-}
module NClient.Connect (connect, Input, Output) where

import Control.Concurrent (forkIO)
import Control.Concurrent.Chan (Chan, newChan, readChan, writeChan)
import Control.Exception (SomeException, catch)
import Control.Monad (forever, void)
import Data.Time.Clock (getCurrentTime)
import Hach.Types
import Network
import Prelude hiding (catch)
import System.Exit (exitFailure, exitSuccess)
import System.IO (hGetLine, hPrint, hSetBuffering, BufferMode(LineBuffering))

type Input = Chan S2C
type Output = Chan C2S

connect ∷ (String, String) → IO (Input, Output)
connect (ip, nick) = do
  i ← newChan
  o ← newChan
  client ip nick i o
  return (i,o)

client ∷ String → String → Input → Output → IO ()
client ip nick i o = do
  withSocketsDo $
    do h ← connectTo ip $ PortNumber 7123
       hSetBuffering h LineBuffering
       τ ← getCurrentTime
       hPrint h $ C2S nick CSetNick
       void $ forkIO $ catch (inputThread h) $ \(_ ∷ SomeException) → do
         writeChan i (S2C "Server has closed the connection." SSystem τ)
         exitFailure
       void $ forkIO $ catch (outputThread h) $ \(_ ∷ SomeException) → do
         writeChan i (S2C (nick ++ " has left.") SSystem τ)
         exitSuccess
  where inputThread h = forever $ hGetLine h >>= writeChan i . read
        outputThread h = forever $ readChan o >>= \m → hPrint h m
