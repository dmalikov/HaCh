{-# LANGUAGE UnicodeSyntax #-}

module Client.Connect (processClient) where

import Control.Applicative ((<$>))
import Control.Concurrent (forkIO)
import Control.Exception
import Control.Monad (forever)
import Data.List (isPrefixOf)
import Data.Time.Clock (getCurrentTime)
import Data.Time.Format
import Network
import System.IO
import System.Locale
import Text.Printf (printf)

import Client.Format
import Hach.Types

processClient ∷ (String, String) → IO ()
processClient (serverIP, nick) = do
  putStrLn $ "Connected to " ++ serverIP
  withSocketsDo $
    do h ← connectTo serverIP $ PortNumber 7123
       hSetBuffering h LineBuffering
       client nick h

client ∷ Nick → Handle → IO ()
client nick h = forkIO
  (handle onDisconnect $ forever $ hGetLine h >>= printMessage . read) >>
  (hPrint h $ CSetNick nick) >>
  (handle onExit $ forever $ getLine >>= hPutStrLn h . processMessage >> hideOwnMessage)
    where processMessage t
            | commandAction  `isPrefixOf` t = show . CAction $ drop (length commandAction) t
            | commandSetNick `isPrefixOf` t = show . CSetNick $ drop (length commandSetNick) t
            | otherwise = show $ CMessage t
          hideOwnMessage = putStrLn "\ESC[2A"
          onExit (SomeException _) = putStrLn $ nick ++" has left"
          onDisconnect (SomeException _) = putStrLn "Server closed connection"

printMessage ∷ S2C → IO ()
printMessage message = do
  timestamp ← formatTime defaultTimeLocale timeFormat <$> getCurrentTime
  printf (format message) timestamp (text message)
