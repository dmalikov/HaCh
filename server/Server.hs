{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE UnicodeSyntax #-}

module Main (main) where

import Control.Exception
import Control.Monad (forever)
import Control.Concurrent
import Network.Socket
import System.IO

import Storage
import Hach.Types

readC ∷ Chan (Int, S2C) → Handle → IO ()
readC ch h = do
  (_, message) ← readChan ch
  hPrint h message

client ∷ Storage → Chan (Int, S2C) → Handle → Int → IO ()
client storage ch h cId = do
  ch' ← dupChan ch
  forkIO $ handle_ $ forever $ readC ch' h
  forever $ do
    m ← hGetLine h
    maybeNick ← getNick storage cId
    case maybeNick of
      Just nick → do
        go nick $ read m
        putStrLn m
        where go ∷ Nick → C2S → IO ()
              go n (CMessage t) = writeChan ch' (cId, SMessage n t)
              go n (CAction t) = writeChan ch' (cId, SAction n t)
              go _ (CSetNick t) = do
                nickExists ← doesNickExist storage t
                if nickExists
                  then hPrint h $ SSystem $ "nick " ++ t ++ " is already in use"
                  else do writeChan ch' (cId, SSetNick nick ("is known as " ++ t))
                          putNick storage cId t
      Nothing → do
        go $ read m
        putStrLn m
        where go ∷ C2S → IO ()
              go (CMessage _) = hPrint h $ SSystem "Undefined client nick"
              go (CAction  _) = hPrint h $ SSystem "Undefined client nick"
              go (CSetNick t) = do
                nickExists ← doesNickExist storage t
                if nickExists
                  then do hPrint h $ SSystem $  "nick " ++ t ++ " is already in use"
                  else do writeChan ch' (cId, SSystem $ t ++ " is connected")
                          putNick storage cId t
  where handle_ = handle $ \(SomeException e) → print e

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

