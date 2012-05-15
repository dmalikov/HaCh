{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE UnicodeSyntax #-}

module Server where

import Control.Exception
import Control.Monad (forever, when)
import Control.Concurrent
import Network.Socket
import System.IO

import Libhach.Storage
import Libhach.Types

readC ∷ Storage → Chan (Int, Message) → Handle → Int → IO ()
readC storage ch h cId' = do
  (cId, m@(Message mType _ (Text text))) ← readChan ch
  case mType of
    SetNick → when (cId == cId') $ putNick storage cId (Nick text) >> showStorage storage
    _       → hPrint h m

client ∷ Storage → Chan (Int, Message) → Handle → Int → IO ()
client storage ch h cId = do
  ch' ← dupChan ch
  forkIO $ handle_ $ forever $ readC storage ch' h cId
  forever $ do
    m ← hGetLine h
    writeChan ch' (cId, read m)
  where handle_ = handle $ \(SomeException _) → return ()

serve ∷ Socket → Storage → Chan (Int, Message) → Int → IO ()
serve sock storage ch !cId = do
  (s, _) ← accept sock
  h ← socketToHandle s ReadWriteMode
  hSetBuffering h LineBuffering
  forkIO $ handle (onDisconnect ch) $ client storage ch h cId
  serve sock storage ch $ cId + 1
  where 
    onDisconnect ∷ Chan (Int, Message) → SomeException → IO ()
    onDisconnect ch' _ = do
      maybeNick ← getNick storage cId
      case maybeNick of
        Just nick → writeChan ch' (cId, Message System nick (Text "has quit conversation"))
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

