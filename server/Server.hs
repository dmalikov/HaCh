{-# LANGUAGE BangPatterns #-}

module Main (main) where

import Control.Exception
import Control.Monad (forever)
import Control.Concurrent
import Data.Time.Clock (getCurrentTime)
import Network.Socket
import System.IO

import Server.Client
import Server.History
import Server.Message
import Server.Storage
import Hach.Types

serve :: Socket -> History -> NickStorage -> Chan (Int, S2C) -> Int -> IO ()
serve sock history storage ch !cId = do
  (s, _) <- accept sock
  h <- socketToHandle s ReadWriteMode
  hSetBuffering h LineBuffering
  hSetBuffering stdout NoBuffering
  forkIO $ handle (onDisconnect ch) $ clientProcessing history storage ch h cId
  serve sock history storage ch $ cId + 1
  where
    onDisconnect :: Chan (Int, S2C) -> SomeException -> IO ()
    onDisconnect ch' _ = do
      maybeNick <- getNick storage cId
      t <- getCurrentTime
      case maybeNick of
        Just n -> do
          writeChan ch' (cId, leftClientM n t)
          delId storage cId
          showStorage storage
        Nothing -> putStrLn "Error: undefined user has left conversation"

main :: IO ()
main = withSocketsDo $ do
  storage <- newStorage
  history <- emptyHistory
  sock <- socket AF_INET Stream 0
  setSocketOption sock ReuseAddr 1
  bindSocket sock (SockAddrInet 7123 iNADDR_ANY)
  listen sock 1024
  ch <- newChan
  forkIO $ forever $ readChan ch >>= const (return ())
  serve sock history storage ch 0
