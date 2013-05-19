
module Client.Connect (processClient) where

import Control.Concurrent (forkIO)
import Control.Exception
import Control.Monad (forever)
import Data.List (isPrefixOf)
import Data.Text (pack)
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
  (hPrint h $ C2S (pack nick) CSetNick) >>
  (handle onExit $ forever $ getLine >>= hPutStrLn h . processMessage >> hideOwnMessage)
    where processMessage t
            | commandAction  `isPrefixOf` t = show $ C2S (pack $ drop (length commandAction) t) CAction
            | commandSetNick `isPrefixOf` t = show $ C2S (pack $ drop (length commandSetNick) t) CSetNick
            | otherwise = show $ C2S (pack t) CPlain
          hideOwnMessage = putStrLn "\ESC[2A"
          onExit (SomeException _) = putStrLn $ nick ++" has left"
          onDisconnect (SomeException _) = putStrLn "Server closed connection"

printMessage :: S2C -> IO ()
printMessage message = print $ fromS2C message

commandAction :: String
commandAction = "/me "

commandSetNick :: String
commandSetNick = "/nick "
