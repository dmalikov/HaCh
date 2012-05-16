{-# LANGUAGE UnicodeSyntax #-}
module Main (main) where

import Control.Applicative ((<$>))
import Control.Arrow ((&&&))
import Control.Concurrent (forkIO)
import Control.Exception
import Control.Monad (forever, liftM2)
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
  (serverIP, nick) ← (uncurry (liftM2 (,)) . (serverFromArgs &&& nickFromArgs)) =<< parseArgs =<< getArgs
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

parseArgs ∷ [String] → IO [Flag]
parseArgs argv = case getOpt Permute options argv of
  (os, _, []) → return os
  (_, _, es) → error $ concat es ++ usageInfo usage options

serverFromArgs ∷ [Flag] → IO String
serverFromArgs xs =
  case [ s | ServerIP s ← xs ] of
    [] → error usage
    (s:_) → return s

nickFromArgs ∷ [Flag] → IO Nick
nickFromArgs xs =
  case [ n | ClientNick n ← xs ] of
    [] → error usage
    (n:_) → return $ Nick n

usage = "Usage: hach-client [OPTIONS...]"
