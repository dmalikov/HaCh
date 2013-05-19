{-# LANGUAGE CPP, ScopedTypeVariables #-}
module NClient.Connect (connect, Input, Output) where

import Control.Concurrent (forkIO)
import Control.Concurrent.Chan (Chan, newChan, readChan, writeChan)
import Control.Exception (SomeException, catch)
import Control.Monad (forever, void)
import Data.Text (pack)
import Network
#if ! MIN_VERSION_base(4,6,0)
import Prelude hiding (catch)
#endif
import System.Exit (exitFailure, exitSuccess)
import System.IO (hGetLine, hPrint, hSetBuffering, BufferMode(LineBuffering))

import Hach.Types

type Input = Chan S2C
type Output = Chan C2S

connect :: (String, String) -> IO (Input, Output)
connect (ip, nick) = do
  i <- newChan
  o <- newChan
  client ip nick i o
  return (i,o)

client :: String -> String -> Input -> Output -> IO ()
client ip nick i o = do
  withSocketsDo $
    do h <- connectTo ip $ PortNumber 7123
       hSetBuffering h LineBuffering
       hPrint h $ C2S (pack nick) CSetNick
       void $ do
         forkIO $ catch (inputThread h) $ \(_ :: SomeException) -> exitFailure
         forkIO $ catch (outputThread h) $ \(_ :: SomeException) -> exitSuccess
  where inputThread h = forever $ hGetLine h >>= writeChan i . read
        outputThread h = forever $ readChan o >>= \m -> hPrint h m
