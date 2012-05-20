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
client nick@(Nick n) h = forkIO
  (handle onDisconnect $ forever $ hGetLine h >>= printMessage . read) >>
  (hPrint h . CSetNick $ Text n) >>
  (handle onExit $ forever $ getLine >>= hPutStrLn h . processMessage >> hideOwnMessage)
    where processMessage t
            | commandAction  `isPrefixOf` t = show . CAction . Text $ drop (length commandAction) t
            | commandSetNick `isPrefixOf` t = show . CSetNick . Text $ drop (length commandSetNick) t
            | otherwise = show $ CMessage $ Text t
          hideOwnMessage = putStrLn "\ESC[2A"
          onExit (SomeException _) = putStrLn $ n ++" has left"
          onDisconnect (SomeException _) = putStrLn "Server closed connection"

printMessage ∷ S2C → IO ()
printMessage message = do
  timestamp ← formatTime defaultTimeLocale timeFormat <$> getCurrentTime
  printf (format message) timestamp (getText message)
  where getText ∷ S2C → String
        getText (SMessage _ (Text text)) = text
        getText (SAction  _ (Text text)) = text
        getText (SSetNick _ (Text text)) = text
        getText (SSystem    (Text text)) = text

main ∷ IO ()
main = do
  (serverIP : nick : _ ) ← parseArgs =<< getArgs
  putStrLn $ "Connected to " ++ serverIP
  withSocketsDo $
    do h ← connectTo serverIP $ PortNumber 7123
       hSetBuffering h LineBuffering
       client (Nick nick) h

data Flag = ServerIP String
          | ClientNick String

options ∷ [OptDescr Flag]
options =
  [ Option "s" ["server"] (ReqArg ServerIP "server_ip") "set server ip adress"
  , Option "n" ["nick"] (ReqArg ClientNick "user nickname") "set user nickname"
  ]

parseArgs ∷ [String] → IO [String]
parseArgs argv = case getOpt Permute options argv of
  (os, _, []) → do
    serverOpt ← serverFromArgs os
    nickOpt ← nickFromArgs os
    return [serverOpt, nickOpt]
  (_, _, es) → error $ concat es ++ usageInfo usage options

serverFromArgs ∷ [Flag] → IO String
serverFromArgs xs =
  case [ s | ServerIP s ← xs ] of
    [] → error $ usageInfo usage options
    (s:_) → return s

nickFromArgs ∷ [Flag] → IO String
nickFromArgs xs =
  case [ n | ClientNick n ← xs ] of
    [] → error $ usageInfo usage options
    (n:_) → return n

usage = "Usage: hach-client [OPTIONS ...]"
