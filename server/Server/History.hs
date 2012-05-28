{-# LANGUAGE UnicodeSyntax #-}
module Server.History
  ( History(..)
  , emptyHistory, putMessage, getMessages
  ) where

import Control.Applicative ((<$>))
import Control.Concurrent.MVar

import qualified Data.DList as DL

import Hach.Types

newtype History = History (MVar (DL.DList S2C))

emptyHistory ∷ IO History
emptyHistory = History <$> newMVar DL.empty

putMessage ∷ History → S2C → IO ()
putMessage (History α) μ = modifyMVar_ α (\h → return $ DL.append h $ DL.singleton μ)

getMessages ∷ History → IO [S2C]
getMessages (History α) = DL.toList <$> readMVar α
