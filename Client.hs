module Main where

import Control.Applicative ((<$>))
import Control.Concurrent (forkIO)
import Control.Monad (forever)
import Data.Time.Clock (getCurrentTime)
import Data.Time.Format
import Network
import System.IO
import System.Locale
import Text.Printf (printf)

import Types

client :: Nick -> Handle -> IO ()
client nick h = forkIO
  (forever $ hGetLine h >>= printMessage . Text) >>
  (forever $ getLine >>= hPutStrLn h . packMessage >> putStrLn "\ESC[2A")
    where packMessage = pack . Message nick . Text

printMessage :: Text -> IO ()
printMessage (Text t) = do
  timestamp <- formatTime defaultTimeLocale "%H:%M:%S" <$> getCurrentTime
  printf "[%s] %s\n" timestamp t

main :: IO ()
main = do
  putStrLn "Set your nick, please"
  nick <- getLine
  putStrLn $ "Your nick is changed to \"" ++ nick ++ "\""
  withSocketsDo $
    do h <- connectTo "127.0.0.1" $ PortNumber 7123
       hSetBuffering h LineBuffering
       client (Nick nick) h
