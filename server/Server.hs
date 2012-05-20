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

import Hach.Storage
import Hach.Types

readC ∷ Storage → Chan (Int, S2C) → Handle → Int → IO ()
readC storage ch h cId' = do
  (cId, message) ← readChan ch
  go message cId cId'
  where go message@(SSetNick nick text) cId cId' = do
          nickExists ← isNickExists storage text
          clientIdExists ← isClientIdExists storage cId
          if nickExists
            then when (cId == cId') $ hPrint h $ SSystem $ "nick " ++ text ++ " is already in use"
            else do when (cId == cId') $ putNick storage cId text
                    if clientIdExists
                      then hPrint h $ SSystem $ nick ++ " is now known as " ++ text
                      else hPrint h $ SSystem $ text ++ " is connected"
          showStorage storage
        go message _ _ = do
          hPrint h message
          print message

client ∷ Storage → Chan (Int, S2C) → Handle → Int → IO ()
client storage ch h cId = do
  ch' ← dupChan ch
  forkIO $ handle_ $ forever $ readC storage ch' h cId
  forever $ do
    m ← hGetLine h
    nick ← fromMaybe "" <$> getNick storage cId
    writeChan ch' (cId, convertMessage nick $ read m)
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

