{-# LANGUAGE BangPatterns #-}
module Main where

import Control.Monad (forever, when)
import Control.Concurrent (forkIO)
import Control.Concurrent.Chan
import Network.Socket
import System.IO

import Types

readC :: Chan (Int, Message) -> Handle -> Int -> IO ()
readC ch h n = do (n', m) <- readChan ch
                  hPrint h m

client :: Chan (Int, Message) -> Handle -> Int -> IO ()
client ch h n = do ch' <- dupChan ch
                   forkIO (forever $ readC ch' h n)
                   forever $ do m <- hGetLine h
                                writeChan ch' (n, read m)

serve :: Socket -> Chan (Int, Message) -> Int -> IO ()
serve sock ch !n = do (s, _) <- accept sock
                      h <- socketToHandle s ReadWriteMode
                      hSetBuffering h LineBuffering
                      forkIO $ client ch h n
                      serve sock ch (n+1)

main :: IO ()
main = withSocketsDo $ do
         sock <- socket AF_INET Stream 0
         setSocketOption sock ReuseAddr 1
         bindSocket sock (SockAddrInet 7123 iNADDR_ANY)
         listen sock 1024
         ch <- newChan
         forkIO $ forever $ readChan ch >>= const (return ())
         serve sock ch 0
