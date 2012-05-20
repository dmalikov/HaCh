{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE UnicodeSyntax #-}

module Main (main) where

import Control.Applicative ((<$>))
import Control.Exception
import Control.Monad (forever, when)
import Control.Concurrent
import Data.Maybe (fromMaybe)
import Network.Socket
import System.IO

import Storage
import Hach.Types

readC ∷ Storage → Chan (Int, S2C) → Handle → Int → IO ()
readC storage ch h cId' = do
  (cId, message) ← readChan ch
  hPrint h message

client ∷ Storage → Chan (Int, S2C) → Handle → Int → IO ()
client storage ch h cId = do
  ch' ← dupChan ch
  forkIO $ handle_ $ forever $ readC storage ch' h cId
  forever $ do
    m ← hGetLine h
    maybeNick ← getNick storage cId
    case maybeNick of
      Just nick → go nick $ read m
        where go ∷ Nick → C2S → IO ()
              go n m@(CMessage text) = writeChan ch' (cId, SMessage n text)
              go n m@(CAction text) = writeChan ch' (cId, SAction n text)
              go n m@(CSetNick text) = do
                nickExists ← doesNickExist storage text
                if nickExists
                  then hPrint h $ SSystem $ "nick " ++ text ++ " is already in use"
                  else do writeChan ch' (cId, SSetNick nick ("is known as " ++ text))
                          putNick storage cId text
      Nothing → do writeChan ch' (cId, SSystem $ nick ++ " is connected")
                   putNick storage cId nick
                   showStorage storage
                   where nick = text (read m ∷ C2S)
  where handle_ = handle $ \(SomeException e) → print e
        convertMessage ∷ Nick → C2S → S2C
        convertMessage n (CMessage t) = SMessage n t
        convertMessage n (CAction t) = SAction n t
        convertMessage n (CSetNick t) = SSetNick n t


serve ∷ Socket → Storage → Chan (Int, S2C) → Int → IO ()
serve sock storage ch !cId = do
  (s, _) ← accept sock
  h ← socketToHandle s ReadWriteMode
  hSetBuffering h LineBuffering
  forkIO $ handle (onDisconnect ch) $ client storage ch h cId
  serve sock storage ch $ cId + 1
  where 
    onDisconnect ∷ Chan (Int, S2C) → SomeException → IO ()
    onDisconnect ch' _ = do
      maybeNick ← getNick storage cId
      case maybeNick of
        Just nick → do
          writeChan ch' (cId, SSystem $ nick ++ " has quit conversation")
          delId storage cId
          showStorage storage
        Nothing → putStrLn "Error: undefined user has quit conversation"

main ∷ IO ()
main = withSocketsDo $ do
  storage ← newStorage
  sock ← socket AF_INET Stream 0
  setSocketOption sock ReuseAddr 1
  bindSocket sock (SockAddrInet 7123 iNADDR_ANY)
  listen sock 1024
  ch ← newChan
  forkIO $ forever $ readChan ch >>= const (return ())
  serve sock storage ch 0

