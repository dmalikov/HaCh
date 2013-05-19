module Server.Client (clientProcessing) where

import Control.Applicative ((<$>))
import Control.Concurrent
import Control.Exception
import Control.Monad (forever)
import Data.Text (unpack)
import Data.Time.Clock (getCurrentTime)
import System.IO

import qualified Data.Traversable as DT

import Hach.Types
import Server.History
import Server.Message
import Server.Storage

readC :: Chan (Int, S2C) -> Handle -> IO ()
readC ch h = hPrint h =<< snd <$> readChan ch

clientProcessing :: History -> Storage -> Chan (Int, S2C) -> Handle -> Int -> IO ()
clientProcessing history storage ch h cId = do
  ch' <- dupChan ch
  forkIO $ handle_ $ forever $ readC ch' h
  forever $ do
    message <- hGetLine h
    maybeNick <- getNick storage cId
    t <- getCurrentTime
    case maybeNick of
      Just nick -> do
        go nick $ read message
        putStrLn message
        where go :: Nick -> C2S -> IO ()
              go n (C2S a CPlain)  = do writeChan ch' (cId, m)
                                        putMessage history m
                                          where m = S2C a (SPlain n) t
              go n (C2S a CAction) = do writeChan ch' (cId, m)
                                        putMessage history m
                                          where m = S2C a (SAction n) t
              go n (C2S a CSetNick) = do
                nickExists <- doesNickExist storage (unpack a)
                if nickExists
                  then hPrint h $ existedNickM (unpack a) t
                  else do writeChan ch' (cId, m)
                          putMessage history m
                          putNick storage cId (unpack a)
                            where m = settedNickM n (unpack a) t
      Nothing -> do
        go $ read message
        putStrLn message
        where go :: C2S -> IO ()
              go (C2S n CSetNick) = do
                nickExists <- doesNickExist storage (unpack n)
                if nickExists
                  then do hPrint h $ existedNickM (unpack n) t
                          hPrint h $ undefinedNickM t
                  else do DT.mapM (hPrint h) . lastNMinutes 10 t =<< getMessages history
                          writeChan ch' (cId, m)
                          putMessage history m
                          putNick storage cId (unpack n)
                            where m = connectedClientM (unpack n) t
              go (C2S _ _) = hPrint h $ undefinedNickM t
  where handle_ = handle $ \(SomeException e) -> print e
