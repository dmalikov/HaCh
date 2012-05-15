{-# LANGUAGE UnicodeSyntax #-}

module Client.Client where

import Control.Applicative ((<$>))
import Control.Concurrent (forkIO)
import Control.Exception
import Control.Monad (forever)
import Data.List (isPrefixOf)
import Data.Time.Clock (getCurrentTime)
import Data.Time.Format
import Network
import System.Environment (getArgs)
import System.Console.GetOpt
import System.IO
import System.Locale
import Text.Printf (printf)

import Libhach.Format
import Libhach.Types

client ∷ Nick → Handle → IO ()
client nick@(Nick n) h = forkIO
  (forever $ hGetLine h >>= printMessage . read) >>
  (hPrint h . Message SetNick nick $ Text n) >>
  (handle onExit $ forever $ getLine >>= hPutStrLn h . processMessage >> hideOwnMessage)
    where
      processMessage text | commandAction  `isPrefixOf` text = show $ Message Action  nick $ Text $ drop (length commandAction)  text
                          | commandSetNick `isPrefixOf` text = show $ Message SetNick nick $ Text $ drop (length commandSetNick) text
                          | otherwise = show $ Message Plain nick $ Text text
      hideOwnMessage = putStrLn "\ESC[2A"
      onExit ∷ SomeException → IO ()
      onExit _ = printf "%s has left" n

printMessage ∷ Message → IO ()
printMessage (Message mtype nick (Text text)) = do
  timestamp ← formatTime defaultTimeLocale timeFormat <$> getCurrentTime
  printf (format (mtype, nick)) timestamp text

main ∷ IO ()
main = do
  serverIP ← serverFromArgs =<< parseArgs =<< getArgs
  putStrLn "Set your nick, please:"
  nick ← getLine
  putStrLn $ "Your nick is changed to \"" ++ nick ++ "\""
  withSocketsDo $
    do h ← connectTo serverIP $ PortNumber 7123
       hSetBuffering h LineBuffering
       client (Nick nick) h

data Flag = ServerIP String

options ∷ [OptDescr Flag]
options =
  [ Option "S" ["server"] (ReqArg ServerIP "server_ip") "set server ip adress"
  ]

parseArgs ∷ [String] → IO [Flag]
parseArgs argv = case getOpt Permute options argv of
  (os, _, []) → return os
  (_, _, es) → error $ concat es ++ usageInfo header options
    where
      header = "Usage: ./Client.hs [OPTIONS...]"

serverFromArgs ∷ [Flag] → IO String
serverFromArgs xs =
  case [ s | ServerIP s ← xs ] of
    [] → error "serverIP undefined"
    (s:_) → return s
