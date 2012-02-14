module Main where

import Control.Monad (forever, unless, void)
import Control.Concurrent (forkIO)
import Network.Socket
import System.IO

loop :: Handle -> IO ()
loop h = do r <- hGetLine h
            unless (null r) (void $ hPutStrLn h r)
            loop h

serve :: Socket -> IO ()
serve sock = do (s, _) <- accept sock
                h <- socketToHandle s ReadWriteMode
                hSetBuffering h LineBuffering
                forkIO (loop h)
                serve sock

main :: IO ()
main = withSocketsDo $ do
         sock <- socket AF_INET Stream 0
         setSocketOption sock ReuseAddr 1
         bindSocket sock (SockAddrInet 7123 iNADDR_ANY)
         listen sock 1024
         serve sock
