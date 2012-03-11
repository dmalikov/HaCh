module Main where

import Control.Applicative ((<$>))
import Control.Concurrent (forkIO)
import Control.Monad (forever)
import Data.Time.Clock (getCurrentTime)
import Data.Time.Format
import Network
import System (getArgs)
import System.Console.GetOpt
import System.IO
import System.Locale
import Text.Printf (printf)

import Types

client :: Nick -> Handle -> IO ()
client nick h = forkIO
  (forever $ hGetLine h >>= printMessage . read) >>
  (forever $ getLine >>= hPutStrLn h . packMessage >> putStrLn "\ESC[2A")
    where packMessage = show . Message nick . Text

printMessage :: Message -> IO ()
printMessage (Message (Nick n) (Text t)) = do
  timestamp <- formatTime defaultTimeLocale "%H:%M:%S" <$> getCurrentTime
  printf "[%s] <%s>: %s\n" timestamp n t

main :: IO ()
main = do
  serverIP <- serverFromArgs =<< parseArgs =<< getArgs
  putStrLn "Set your nick, please"
  nick <- getLine
  putStrLn $ "Your nick is changed to \"" ++ nick ++ "\""
  withSocketsDo $
    do h <- connectTo serverIP $ PortNumber 7123
       hSetBuffering h LineBuffering
       client (Nick nick) h

data Flag = ServerIP String

options :: [OptDescr Flag]
options =
  [ Option "S" ["server"] (ReqArg ServerIP "server_ip") "set server ip adress"
  ]

parseArgs :: [String] -> IO [Flag]
parseArgs argv = case getOpt Permute options argv of
  (os, _, []) -> return os
  (_, _, es) -> error $ concat es ++ usageInfo header options
    where
      header = "Usage: ./Client.hs [OPTIONS...]"

serverFromArgs :: [Flag] -> IO String
serverFromArgs xs =
  case [ s | ServerIP s <- xs ] of
    [] -> error "serverIP undefined"
    (s:_) -> return s

