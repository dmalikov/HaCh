{-# LANGUAGE BangPatterns #-}
module Main where

import Control.Monad (forever, unless, void, when)
import Control.Concurrent (forkIO)
import Control.Concurrent.Chan
import Network.Socket
import Prelude hiding (read)
import System.IO

read :: Chan (Int, String) -> Handle -> Int -> IO ()
read ch h n = do (n', m) <- readChan ch
                 when (n' /= n) (hPutStrLn h m)

client :: Chan (Int, String) -> Handle -> Int -> IO ()
client ch h n = do ch' <- dupChan ch
                   forkIO (forever $ read ch' h n)
                   forever $ do m <- hGetLine h
                                writeChan ch' (n, m)

serve :: Socket -> Chan (Int, String) -> Int -> IO ()
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
         serve sock ch 0
