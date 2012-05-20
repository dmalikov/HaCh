{-# LANGUAGE UnicodeSyntax #-}
module Main (main) where

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

import Hach.Format
import Hach.Types

client ∷ Nick → Handle → IO ()
client nick h = forkIO
  (handle onDisconnect $ forever $ hGetLine h >>= printMessage . read) >>
  (hPrint h $ CSetNick nick) >>
  (handle onExit $ forever $ getLine >>= hPutStrLn h . processMessage >> hideOwnMessage)
    where processMessage t
            | commandAction  `isPrefixOf` t = show . CAction $ drop (length commandAction) t
            | commandSetNick `isPrefixOf` t = show . CSetNick $ drop (length commandSetNick) t
            | otherwise = show $ CMessage t
          hideOwnMessage = putStrLn "\ESC[2A"
          onExit (SomeException _) = putStrLn $ nick ++" has left"
          onDisconnect (SomeException _) = putStrLn "Server closed connection"

printMessage ∷ S2C → IO ()
printMessage message = do
  timestamp ← formatTime defaultTimeLocale timeFormat <$> getCurrentTime
  printf (format message) timestamp (getText message)
  where getText ∷ S2C → String
        getText (SMessage _ text) = text
        getText (SAction  _ text) = text
        getText (SSetNick _ text) = text
        getText (SSystem    text) = text

main ∷ IO ()
main = do
  (serverIP, nick) ← parseArgs =<< getArgs
  putStrLn $ "Connected to " ++ serverIP
  withSocketsDo $
    do h ← connectTo serverIP $ PortNumber 7123
       hSetBuffering h LineBuffering
       client nick h

data Flag = ServerIP String
          | ClientNick String

options ∷ [OptDescr Flag]
options =
  [ Option "s" ["server"] (ReqArg ServerIP "server_ip") "set server ip adress"
  , Option "n" ["nick"] (ReqArg ClientNick "user nickname") "set user nickname"
  ]

parseArgs ∷ [String] → IO (String, String)
parseArgs argv = case getOpt Permute options argv of
  (os, _, []) → do
    let ips = [ s | ServerIP s ← os ]
    let nicks = [ n | ClientNick n ← os ]
    case (ips, nicks) of
      ([ip], [nick]) → return (ip, nick)
      (_, _) → error $ usageInfo usage options
  (_, _, es) → error $ concat es ++ usageInfo usage options
  where usage = "Usage: hach-client [OPTIONS...]"
