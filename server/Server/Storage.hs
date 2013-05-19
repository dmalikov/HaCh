
module Server.Storage
  ( Storage(..)
  , newStorage, getNick, putNick, delId
  , doesNickExist
  , showStorage
  ) where

import Control.Applicative ((<$>))
import Control.Concurrent.MVar

import qualified Data.Map as M

import Hach.Types

type ClientId = Int

newtype Storage = Storage (MVar (M.Map ClientId Nick))

newStorage :: IO Storage
newStorage = Storage <$> newMVar M.empty

getNick :: Storage -> ClientId -> IO (Maybe Nick)
getNick (Storage s) c = M.lookup c <$> readMVar s

putNick :: Storage -> ClientId -> Nick -> IO ()
putNick (Storage s) c n = modifyMVar_ s $ return . M.insert c n

delId :: Storage -> ClientId -> IO ()
delId (Storage s) c = modifyMVar_ s $ return . M.delete c

doesNickExist :: Storage -> Nick -> IO Bool
doesNickExist (Storage s) n = elem n <$> M.elems <$> readMVar s

showStorage :: Storage -> IO ()
showStorage (Storage s) = print =<< readMVar s