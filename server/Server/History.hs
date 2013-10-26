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

emptyHistory :: IO History
emptyHistory = History <$> newMVar S.empty

putMessage :: History -> S2C -> IO ()
putMessage (History a) m = modifyMVar_ a (\h -> return $ h S.|> m)

getMessages :: History -> IO (S.Seq S2C)
getMessages (History a) = readMVar a

lastNMinutes :: Int -> Timestamp -> S.Seq S2C -> S.Seq S2C
lastNMinutes minutes currentTime = S.takeWhileR inLastMinutes
  where inLastMinutes :: S2C -> Bool
        inLastMinutes m = diffUTCTime currentTime (time m) < nominalMinutes
          where nominalMinutes = 60 * fromIntegral minutes :: NominalDiffTime
