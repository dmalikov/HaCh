{-# LANGUAGE UnicodeSyntax #-}
module Server.Client (clientProcessing) where

import Control.Applicative ((<$>))
import Control.Concurrent
import Control.Exception
import Control.Monad (forever)
import System.IO

import Hach.Types
import Server.Message
import Server.Storage

readC ∷ Chan (Int, S2C) → Handle → IO ()
readC ch h = hPrint h =<< snd <$> readChan ch

clientProcessing ∷ Storage → Chan (Int, S2C) → Handle → Int → IO ()
clientProcessing storage ch h cId = do
  ch' ← dupChan ch
  forkIO $ handle_ $ forever $ readC ch' h
  forever $ do
    m ← hGetLine h
    maybeNick ← getNick storage cId
    case maybeNick of
      Just nick → do
        go nick $ read m
        putStrLn m
        where go ∷ Nick → C2S → IO ()
              go n (CMessage t) = writeChan ch' (cId, SMessage n t)
              go n (CAction t) = writeChan ch' (cId, SAction n t)
              go _ (CSetNick t) = do
                nickExists ← doesNickExist storage t
                if nickExists
                  then hPrint h $ existedNickM t
                  else do writeChan ch' (cId, settedNickM nick t)
                          putNick storage cId t
      Nothing → do
        go $ read m
        putStrLn m
        where go ∷ C2S → IO ()
              go (CMessage _) = hPrint h undefinedNickM
              go (CAction  _) = hPrint h undefinedNickM
              go (CSetNick t) = do
                nickExists ← doesNickExist storage t
                if nickExists
                  then do hPrint h $ existedNickM t
                  else do writeChan ch' (cId, connectedClientM t)
                          putNick storage cId t
  where handle_ = handle $ \(SomeException e) → print e
