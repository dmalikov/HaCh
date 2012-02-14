module Main where

import Control.Monad (forever)
import Network
import System.IO

loop :: Handle -> IO ()
loop h = getLine >>= hPutStrLn h >> hGetLine h >>= putStrLn

main :: IO ()
main = withSocketsDo $
         do h <- connectTo "127.0.0.1" (PortNumber 7123)
            hSetBuffering h NoBuffering
            forever (loop h)
