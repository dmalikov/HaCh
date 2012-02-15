module Main where

import Control.Monad (forever, unless, void)
import Control.Concurrent (forkIO)
import Control.Concurrent.Chan
import Network.Socket
import System.IO

readLoop :: Chan String -> IO ()
readLoop ch = readChan ch >>= putStrLn

clientLoop :: Chan String -> Handle -> IO ()
clientLoop ch h = hGetLine h >>= writeChan ch

serve :: Socket -> Chan String -> IO ()
serve sock ch = do (s, _) <- accept sock
                   h <- socketToHandle s ReadWriteMode
                   hSetBuffering h LineBuffering
                   void $ forkIO (forever $ clientLoop ch h)

main :: IO ()
main = withSocketsDo $ do
         sock <- socket AF_INET Stream 0
         setSocketOption sock ReuseAddr 1
         bindSocket sock (SockAddrInet 7123 iNADDR_ANY)
         listen sock 1024
         ch <- newChan
         forkIO (forever $ readLoop ch)
         forever $ serve sock ch
