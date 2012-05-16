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
  (handle onServerDied $ forever $ hGetLine h >>= printMessage . read) >>
  (hPrint h . Message SetNick nick $ Text n) >>
  (handle onExit $ forever $ getLine >>= hPutStrLn h . processMessage >> hideOwnMessage)
    where
      processMessage text | commandAction  `isPrefixOf` text = show $ Message Action  nick $ Text $ drop (length commandAction)  text
                          | commandSetNick `isPrefixOf` text = show $ Message SetNick nick $ Text $ drop (length commandSetNick) text
                          | otherwise = show $ Message Plain nick $ Text text
      hideOwnMessage = putStrLn "\ESC[2A"
      onExit (SomeException _) = putStrLn $ n ++" has left"
      onServerDied (SomeException _) = putStrLn "Server closed connection"

printMessage ∷ Message → IO ()
printMessage (Message mtype nick (Text text)) = do
  timestamp ← formatTime defaultTimeLocale timeFormat <$> getCurrentTime
  printf (format (mtype, nick)) timestamp text

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

usage = "Usage: hach-client"
