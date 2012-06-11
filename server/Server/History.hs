{-# LANGUAGE UnicodeSyntax #-}
module Server.History
  ( History(..)
  , emptyHistory, putMessage, getMessages, lastNMinutes
  ) where

import Control.Applicative ((<$>))
import Control.Concurrent.MVar
import Data.Time.Clock (diffUTCTime, NominalDiffTime)

import qualified Data.Sequence as S

import Hach.Types

newtype History = History (MVar (S.Seq S2C))

emptyHistory ∷ IO History
emptyHistory = History <$> newMVar S.empty

putMessage ∷ History → S2C → IO ()
putMessage (History α) μ = modifyMVar_ α (\h → return $ h S.|> μ)

getMessages ∷ History → IO (S.Seq S2C)
getMessages (History α) = readMVar α

lastNMinutes ∷ Int → Timestamp → S.Seq S2C → S.Seq S2C
lastNMinutes minutes currentTime = S.takeWhileR inLastMinutes
  where inLastMinutes ∷ S2C → Bool
        inLastMinutes μ = diffUTCTime currentTime (time μ) < nominalMinutes
          where nominalMinutes = 60 * fromIntegral minutes ∷ NominalDiffTime
