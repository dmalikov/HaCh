{-# LANGUAGE BangPatterns #-}
module Main where

import Control.Exception
import Control.Monad (forever)
import Control.Concurrent (forkIO)
import Control.Concurrent.Chan
import Network.Socket
import System.IO

import Types

readC :: Chan (Int, Message) -> Handle -> IO ()
readC ch h = do
  (_, m) <- readChan ch
  hPrint h m

client :: Chan (Int, Message) -> Handle -> ClientId -> IO ()
client ch h (ClientId n) = do
  ch' <- dupChan ch
  forkIO (forever $ readC ch' h)
  handle onExit $ forever $ do
    m <- hGetLine h
    writeChan ch' (n, read m)
  where onExit :: SomeException -> IO ()
        onExit e = print e

serve :: Socket -> Chan (Int, Message) -> ClientId -> IO ()
serve sock ch (ClientId !n) = do
  (s, _) <- accept sock
  h <- socketToHandle s ReadWriteMode
  hSetBuffering h LineBuffering
  forkIO $ client ch h (ClientId n)
  serve sock ch (ClientId $ n+1)

main :: IO ()
main = withSocketsDo $ do
  sock <- socket AF_INET Stream 0
  setSocketOption sock ReuseAddr 1
  bindSocket sock (SockAddrInet 7123 iNADDR_ANY)
  listen sock 1024
  ch <- newChan
  forkIO $ forever $ readChan ch >>= const (return ())
  serve sock ch (ClientId 0)
