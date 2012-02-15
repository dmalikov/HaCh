module Main where

import Control.Monad (forever, unless, void)
import Control.Concurrent (forkIO)
import Control.Concurrent.Chan
import Network.Socket
import System.IO

readLoop :: Chan String -> Handle -> IO ()
readLoop ch h = readChan ch >>= hPutStrLn h

client :: Chan String -> Handle -> IO ()
client ch h = forkIO (forever $ readLoop ch h) >> (forever $ hGetLine h >>= writeChan ch)

serve :: Socket -> Chan String -> IO ()
serve sock ch = do (s, _) <- accept sock
                   h <- socketToHandle s ReadWriteMode
                   hSetBuffering h LineBuffering
                   void $ forkIO (dupChan ch >>= flip client h)

main :: IO ()
main = withSocketsDo $ do
         sock <- socket AF_INET Stream 0
         setSocketOption sock ReuseAddr 1
         bindSocket sock (SockAddrInet 7123 iNADDR_ANY)
         listen sock 1024
         ch <- newChan
         forever $ serve sock ch
