{-# LANGUAGE UnicodeSyntax #-}

module Storage
  ( Storage(..), newStorage, getNick, putNick, showStorage
  ) where

import Control.Applicative ((<$>))
import Control.Concurrent.MVar

import qualified Data.Map as M

import Types

type ClientId = Int

newtype Storage = Storage (MVar (M.Map ClientId Nick))

newStorage ∷ IO Storage
newStorage = Storage <$> newMVar M.empty

getNick ∷ Storage → ClientId → IO (Maybe Nick)
getNick (Storage s) c = M.lookup c <$> readMVar s

putNick ∷ Storage → ClientId → Nick → IO ()
putNick (Storage s) c n = modifyMVar_ s $ return . M.insert c n

showStorage ∷ Storage → IO ()
showStorage (Storage s) = print =<< readMVar s
