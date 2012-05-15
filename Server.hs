{-# LANGUAGE BangPatterns #-}
module Main where

import Control.Exception
import Control.Monad (forever, when)
import Control.Concurrent
import Network.Socket
import System.IO

import Storage
import Types

readC :: Storage -> Chan (Int, Message) -> Handle -> ClientId -> IO ()
readC storage ch h (ClientId cId') = do
  (cId, m@(Message mType _ (Text text))) <- readChan ch
  case mType of
    SetNick -> when (cId == cId') $ putNick storage (ClientId cId) (Nick text) >> showStorage storage
    _       -> hPrint h m

client :: Storage -> Chan (Int, Message) -> Handle -> ClientId -> IO ()
client storage ch h cId@(ClientId n) = do
  ch' <- dupChan ch
  forkIO (forever $ readC storage ch' h cId)
  handle (onDisconnect ch') $ forever $ do
    m <- hGetLine h
    writeChan ch' (n, read m)
  where onDisconnect :: Chan (Int, Message) -> SomeException -> IO ()
        onDisconnect ch' _ = do
          maybeNick <- getNick storage cId
          case maybeNick of
            Just nick -> writeChan ch' (n, Message System nick (Text "has quit conversation"))
            Nothing -> putStrLn "Error: undefined user has quit conversation"

serve :: Socket -> Storage -> Chan (Int, Message) -> ClientId -> IO ()
serve sock storage ch (ClientId !n) = do
  (s, _) <- accept sock
  h <- socketToHandle s ReadWriteMode
  hSetBuffering h LineBuffering
  forkIO $ client storage ch h (ClientId n)
  serve sock storage ch (ClientId $ n+1)

main :: IO ()
main = withSocketsDo $ do
  storage <- newStorage
  sock <- socket AF_INET Stream 0
  setSocketOption sock ReuseAddr 1
  bindSocket sock (SockAddrInet 7123 iNADDR_ANY)
  listen sock 1024
  ch <- newChan
  forkIO $ forever $ readChan ch >>= const (return ())
  serve sock storage ch (ClientId 0)
