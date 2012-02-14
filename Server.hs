module Main where

import Control.Monad (forever, unless, void)
import Network
import System.IO

loop :: Handle -> IO ()
loop h = do r <- hGetLine h
            unless (null r) (void $ hPutStrLn h r)
            loop h

main :: IO ()
main = withSocketsDo $ do
         socket <- listenOn (PortNumber 7123)
         (h, _, _) <- accept socket
         hSetBuffering h NoBuffering
         forever (loop h)
