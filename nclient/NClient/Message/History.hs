{-# LANGUAGE ViewPatterns, OverloadedStrings #-}
module NClient.Message.History
  ( History
  , empty, prepend
  , line
  , next, previous
  ) where

import Data.Sequence (Seq, (<|))
import Prelude hiding (lines)
import qualified Data.Sequence as Seq
import Data.Text (Text)

data History = History { lines :: Seq Text, current :: Int, capacity :: Int }

prepend :: Text -> History -> History
prepend l h = h { lines = Seq.take (capacity h + 1) $ " " <| l <| Seq.drop 1 (lines h), current = 0 }

empty :: Int -> History
empty c = History { lines = Seq.empty, current = 0, capacity = c }

line :: History -> Text
line h = lines h `Seq.index` current h

next :: Text -> History -> History
next t = tryNext . trySetCurrent t

tryNext :: History -> History
tryNext h@(History ls i _)
  | i == Seq.length ls - 1 = h
  | otherwise = h { current = succ i }

previous :: History -> History
previous h@(History _ 0 _) = h
previous h = h { current = pred $ current h }

trySetCurrent :: Text -> History -> History
trySetCurrent a h@(current -> 0) = h { lines = a <| Seq.drop 1 (lines h) }
trySetCurrent _ h = h
