{-# LANGUAGE OverloadedStrings #-}
module Client.Connect (processClient) where

import Control.Concurrent (forkIO)
import Control.Exception
import Control.Monad (forever)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Network
import System.IO

import Hach.Types

processClient :: (String, String) -> IO ()
processClient (serverIP, nick) = do
  putStrLn $ "Connected to " ++ serverIP
  withSocketsDo $
    do h <- connectTo serverIP $ PortNumber 7123
       hSetBuffering h LineBuffering
       client nick h

client :: Nick -> Handle -> IO ()
client nick h = forkIO
  (handle onDisconnect $ forever $ hGetLine h >>= printMessage . read) >>
  (hPrint h $ C2S (T.pack nick) CSetNick) >>
  (handle onExit $ forever $ TIO.getLine >>= hPutStrLn h . processMessage >> hideOwnMessage)
    where processMessage t
            | commandAction  `T.isPrefixOf` t = show $ C2S (T.drop (T.length commandAction) t) CAction
            | commandSetNick `T.isPrefixOf` t = show $ C2S (T.drop (T.length commandSetNick) t) CSetNick
            | otherwise = show $ C2S t CPlain
          hideOwnMessage = putStrLn "\ESC[2A"
          onExit (SomeException _) = putStrLn $ nick ++ " has left"
          onDisconnect (SomeException _) = putStrLn "Server closed connection"

printMessage :: S2C -> IO ()
printMessage message = print $ fromS2C message

commandAction :: T.Text
commandAction = "/me "

commandSetNick :: T.Text
commandSetNick = "/nick "
