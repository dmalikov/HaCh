module Main where

import Control.Concurrent (forkIO)
import Control.Monad (forever)
import Network
import System.IO

import Types

client :: Nick -> Handle -> IO ()
client nick h = forkIO (forever $ hGetLine h >>= putStrLn) >> (forever $ getLine >>= hPutStrLn h . packMessage)
    where packMessage = pack . formatMessage nick . Text

main :: IO ()
main = do
  putStrLn "Set your nick, please"
  nick <- getLine
  putStrLn $ "Your nick is changed to \"" ++ nick ++ "\""
  withSocketsDo $
    do h <- connectTo "127.0.0.1" $ PortNumber 7123
       hSetBuffering h LineBuffering
       client (Nick nick) h

formatMessage :: Nick -> Text -> Message
formatMessage (Nick nick) (Text text) = Message (Nick nick) (Text text) (Timestamp 3)
