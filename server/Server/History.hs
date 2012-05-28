{-# LANGUAGE UnicodeSyntax #-}
module Server.History
  ( History(..)
  , emptyHistory, putMessage, getMessages
  ) where

import Control.Applicative ((<$>))
import Control.Concurrent.MVar

import qualified Data.Sequence as S

import Hach.Types

newtype History = History (MVar (S.Seq S2C))

emptyHistory ∷ IO History
emptyHistory = History <$> newMVar S.empty

putMessage ∷ History → S2C → IO ()
putMessage (History α) μ = modifyMVar_ α (\h → return $ h S.|> μ)

getMessages ∷ History → IO (S.Seq S2C)
getMessages (History α) = readMVar α
