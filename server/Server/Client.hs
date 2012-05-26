{-# LANGUAGE UnicodeSyntax #-}
module Server.Client (clientProcessing) where

import Control.Applicative ((<$>))
import Control.Concurrent
import Control.Exception
import Control.Monad (forever)
import Data.Time.Clock (getCurrentTime)
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
    τ ← getCurrentTime
    case maybeNick of
      Just nick → do
        go nick $ read m
        putStrLn m
        where go ∷ Nick → C2S → IO ()
              go η (C2S α CPlain) = writeChan ch' (cId, S2C α (SPlain η) τ)
              go η (C2S α CAction) = writeChan ch' (cId, S2C α (SAction η) τ)
              go η (C2S α CSetNick) = do
                nickExists ← doesNickExist storage α
                if nickExists
                  then hPrint h $ existedNickM α τ
                  else do writeChan ch' (cId, settedNickM η α τ)
                          putNick storage cId α
      Nothing → do
        go $ read m
        putStrLn m
        where go ∷ C2S → IO ()
              go (C2S η CSetNick) = do
                nickExists ← doesNickExist storage η
                if nickExists
                  then do hPrint h $ existedNickM η τ
                  else do writeChan ch' (cId, connectedClientM η τ)
                          putNick storage cId η
              go (C2S _ _) = hPrint h $ undefinedNickM τ

  where handle_ = handle $ \(SomeException e) → print e
