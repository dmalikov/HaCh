{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE UnicodeSyntax #-}

module Main (main) where

import Control.Exception
import Control.Monad (forever)
import Control.Concurrent
import Network.Socket
import System.IO

import Server.Client
import Server.Message
import Server.Storage
import Hach.Types

serve ∷ Socket → Storage → Chan (Int, S2C) → Int → IO ()
serve sock storage ch !cId = do
  (s, _) ← accept sock
  h ← socketToHandle s ReadWriteMode
  hSetBuffering h LineBuffering
  forkIO $ handle (onDisconnect ch) $ clientProcessing storage ch h cId
  serve sock storage ch $ cId + 1
  where 
    onDisconnect ∷ Chan (Int, S2C) → SomeException → IO ()
    onDisconnect ch' _ = do
      maybeNick ← getNick storage cId
      case maybeNick of
        Just nick → do
          writeChan ch' (cId, leftClientM nick)
          delId storage cId
          showStorage storage
        Nothing → putStrLn "Error: undefined user has left conversation"

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
