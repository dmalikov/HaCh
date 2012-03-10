module Main where

import Control.Concurrent (forkIO)
import Control.Monad (forever)
import Network
import System.IO

import Types

client :: Handle -> IO ()
client h = forkIO (forever $ hGetLine h >>= putStrLn . formatMessage . unpack)
               >> (forever $ getLine >>= hPutStrLn h)

formatMessage :: Message -> String
formatMessage = show

main :: IO ()
main = withSocketsDo $
         do h <- connectTo "127.0.0.1" $ PortNumber 7123
            hSetBuffering h LineBuffering
            client h
