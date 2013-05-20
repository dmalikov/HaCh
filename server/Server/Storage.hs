
module Server.Storage
  ( NickStorage(..)
  , newStorage, getNick, putNick, delId
  , doesNickExist
  , showStorage
  ) where

import Control.Applicative ((<$>))
import Control.Concurrent.MVar
import qualified Data.Map as M
import Data.Text

type ClientId = Int

data NickStorage = NickStorage (MVar (M.Map ClientId Text))

newStorage :: IO NickStorage
newStorage = NickStorage <$> newMVar M.empty

getNick :: NickStorage -> ClientId -> IO (Maybe Text)
getNick (NickStorage s) c = M.lookup c <$> readMVar s

putNick :: NickStorage -> ClientId -> Text -> IO ()
putNick (NickStorage s) c n = modifyMVar_ s $ return . M.insert c n

delId :: NickStorage -> ClientId -> IO ()
delId (NickStorage s) c = modifyMVar_ s $ return . M.delete c

doesNickExist :: NickStorage -> Text -> IO Bool
doesNickExist (NickStorage s) n = elem n <$> M.elems <$> readMVar s

showStorage :: NickStorage -> IO ()
showStorage (NickStorage s) = print =<< readMVar s
